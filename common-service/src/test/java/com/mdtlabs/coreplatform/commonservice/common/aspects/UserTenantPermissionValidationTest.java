package com.mdtlabs.coreplatform.commonservice.common.aspects;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.OrganizationUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class UserTenantPermissionValidationTest {

    RedisTemplate<String, Map<String, List<String>>> redisTemplate = mock(RedisTemplate.class);
    OrganizationRepository organizationRepository = mock(OrganizationRepository.class);
    OrganizationUtil organizationUtil = mock(OrganizationUtil.class);
    UserTenantPermissionValidation userTenantPermissionValidation = new UserTenantPermissionValidation(redisTemplate, organizationRepository, organizationUtil);

    HttpServletRequest request = mock(HttpServletRequest.class);
    MockedStatic<RequestContextHolder> requestContextHolderMockedStatic;
    ServletRequestAttributes servletRequestAttributes = mock(ServletRequestAttributes.class);
    MockedStatic<UserContextHolder> userContextHolderStatic;


    @BeforeEach
    void before() {
        UserContextDTO userContextDTO = getSampleUserContextDTO();
        requestContextHolderMockedStatic = mockStatic(RequestContextHolder.class);
        requestContextHolderMockedStatic.when(RequestContextHolder::currentRequestAttributes).thenReturn(servletRequestAttributes);
        when(servletRequestAttributes.getRequest()).thenReturn(request);
        userContextHolderStatic = mockStatic(UserContextHolder.class);
        userContextHolderStatic.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);

    }

    private UserContextDTO getSampleUserContextDTO() {
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setTenantId(0L);
        userContextDTO.setRoles(List.of(new Role(1L, Constants.ROLE_REGION_ADMIN)));
        return userContextDTO;
    }

    @AfterEach
    void closeConnection() {
        requestContextHolderMockedStatic.close();
        userContextHolderStatic.close();
    }

    @Test
    void testValidateAspect() {
        ListOperations<String, Map<String, List<String>>> listOperations = mock(ListOperations.class);
        when(request.getMethod()).thenReturn(Constants.METHOD_PATCH);
        UserDTO userDTO = new UserDTO();
        Map<String, List<String>> ids = new HashMap<>();
        ids.put("123", new ArrayList<>(Arrays.asList("123", "1", "2", "3")));
        userDTO.setTenantId(123L);
        when(redisTemplate.opsForList()).thenReturn(listOperations);
        when(listOperations.range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, 0)).thenReturn(List.of(ids));
        userTenantPermissionValidation.validateAspect(null, userDTO);
        verify(organizationUtil, times(0)).getParentChildTenantMap();
    }

    @Test
    void testValidateAspectWhenBodyInArrayList() {
        ListOperations<String, Map<String, List<String>>> listOperations = mock(ListOperations.class);
        when(request.getMethod()).thenReturn(Constants.METHOD_PATCH);
        UserDTO userDTO = new UserDTO();
        ArrayList<Object> user = new ArrayList<>();
        Map<String, List<String>> ids = new HashMap<>();
        ids.put("123", new ArrayList<>(Arrays.asList("123", "1", "2", "3")));
        userDTO.setTenantId(123L);
        user.add(userDTO);
        when(redisTemplate.opsForList()).thenReturn(listOperations);
        when(listOperations.range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, 0)).thenReturn(List.of(ids));
        userTenantPermissionValidation.validateAspect(null, user);
        verify(organizationUtil, times(0)).getParentChildTenantMap();
    }

    @Test
    void testValidateAspectWithException() {
        when(request.getMethod()).thenReturn(Constants.METHOD_PATCH);
        UserDTO userDTO = new UserDTO();
        ArrayList<Object> user = new ArrayList<>();
        Map<String, List<String>> ids = new HashMap<>();
        ids.put("123", new ArrayList<>(Arrays.asList("123", "1", "2", "3")));
        userDTO.setTenantId(123L);
        user.add(userDTO);
        doThrow(new RedisConnectionFailureException("")).when(redisTemplate).opsForList();
        assertThrows(Exception.class, () -> userTenantPermissionValidation.validateAspect(null, user));
    }

    @Test
    void testValidateAspectWhenTenantIdIsNullInObject() {
        //given
        ListOperations<String, Map<String, List<String>>> listOperations = mock(ListOperations.class);
        UserDTO userDTO = new UserDTO();

        //when
        when(request.getMethod()).thenReturn(Constants.METHOD_PATCH);
        when(redisTemplate.opsForList()).thenReturn(listOperations);

        //then
        assertThrows(Validation.class, () -> userTenantPermissionValidation.validateAspect(null, userDTO));
    }

//    @Test
    void testValidateAspectWhenTenantIdIsNullInArrayList() {
        //given
        ListOperations<String, Map<String, List<String>>> listOperations = mock(ListOperations.class);
        UserDTO userDTO = new UserDTO();

        ArrayList<Object> user = new ArrayList<>();
        user.add(userDTO);

        //when
        when(request.getMethod()).thenReturn(Constants.METHOD_PATCH);
        when(redisTemplate.opsForList()).thenReturn(listOperations);

        //then
        assertThrows(Validation.class, () -> userTenantPermissionValidation.validateAspect(null, user));
    }
}