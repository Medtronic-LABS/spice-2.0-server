package com.mdtlabs.coreplatform.commonservice.common.aspects;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.OrganizationUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import jakarta.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static java.util.Objects.nonNull;

/**
 * <p>
 * This class is the business logic implementation for validation annotation
 * used under @UserTenantValidation annotation.
 * </p>
 * 
 * @author Gopinath created on Aug 01, 2024
 */
@Aspect
@Component
public class UserTenantPermissionValidation {

    /**
     * <p>
     * Creating an instance of the `ObjectMapper` class from the Jackson library, which is used to
     * convert Java objects to JSON and vice versa
     * </p>
     */
    private final ObjectMapper mapper = new ObjectMapper();

    private final RedisTemplate<String, Map<String, List<String>>> redisTemplate;

    public final OrganizationRepository organizationRepository;

    public final OrganizationUtil organizationUtil;

    @Autowired
    public UserTenantPermissionValidation(RedisTemplate<String, Map<String, List<String>>> redisTemplate,
                                          OrganizationRepository organizationRepository, OrganizationUtil organizationUtil) {
        this.redisTemplate = redisTemplate;
        this.organizationRepository = organizationRepository;
        this.organizationUtil = organizationUtil;
    }

    private List<Organization> organizationList = new ArrayList<>();
    
    private Map<String, Object> requestBody;

    /**
     * <p>
     * This is a Java aspect that validates user and tenant information in the request body for certain
     * HTTP methods, unless the user has super user or super admin roles.
     * </p>
     *
     * @param joinPoint {@link JoinPoint} object represents the execution of a method in the program
     * @param body      The body parameter object represents the request body of an HTTP request.
     */
    @Before("@annotation(com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation)  && args(.., @RequestBody body)")
    public void validateAspect(JoinPoint joinPoint, final Object body) {
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes())
                .getRequest();
        UserContextDTO user = UserContextHolder.getUserDto();
        if ((Constants.METHOD_POST.equalsIgnoreCase(request.getMethod())
                || Constants.METHOD_PUT.equalsIgnoreCase(request.getMethod())
                || Constants.METHOD_DELETE.equalsIgnoreCase(request.getMethod())
                || Constants.METHOD_PATCH.equalsIgnoreCase(request.getMethod()))
                && user.getRoles().stream().noneMatch(role -> role.getName().equals(Constants.ROLE_SUPER_USER))
                && user.getRoles().stream().noneMatch(role -> role.getName().equals(Constants.ROLE_SUPER_ADMIN))) {
            if (body.getClass().equals(ArrayList.class)) {
                List<Object> bodyList = mapper.convertValue(body, ArrayList.class);
                bodyList.stream().forEach(object -> {
                    requestBody = mapper.convertValue(object, Map.class);
                    validationCheck(requestBody);
                });
            } else {
                requestBody = mapper.convertValue(body, Map.class);
                validationCheck(requestBody);
            }
            requestBody.clear();

        }
    }

    /**
     * <p>
     * Validation check on id and tenant id.
     * </p>
     *
     * @param requestBody {@link Map } input object
     */
    private void validationCheck(Map<String, Object> requestBody) {
        Logger.logInfo("Request Body: "+requestBody);
        if (!requestBody.containsKey(Constants.TENANT_PARAMETER_NAME)) {
            throw new Validation(20006);
        }
        if (Objects.isNull(requestBody.get(Constants.TENANT_PARAMETER_NAME))) {
            throw new Validation(20006);
        }
        List<String> childIds = new ArrayList<>();
        List<Map<String, List<String>>> organizationListRedis = new ArrayList<>();
        try {
            organizationListRedis = redisTemplate.opsForList()
                    .range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, organizationList.size());
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        if (!Objects.isNull(organizationListRedis)  && organizationListRedis.isEmpty()) {
            organizationUtil.getParentChildTenantMap();
            organizationListRedis = redisTemplate.opsForList()
                    .range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, organizationList.size());
        }
        if (!CollectionUtils.isEmpty(organizationListRedis) && nonNull(organizationListRedis.getFirst())) {
            childIds = organizationListRedis.getFirst().get(String.valueOf(UserContextHolder.getUserDto().getTenantId()));
            Logger.logInfo("Organization child ids: "+ childIds);
        }

        if (Objects.nonNull(childIds)) {
            if (!childIds.contains(String.valueOf(requestBody.get(Constants.TENANT_PARAMETER_NAME)))) {
                throw new Validation(20005);
            }
            childIds.clear();
        }
    }
}