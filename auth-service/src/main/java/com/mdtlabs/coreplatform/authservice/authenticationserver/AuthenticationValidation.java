package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.text.ParseException;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import com.mdtlabs.coreplatform.authservice.helper.HelperService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jwt.EncryptedJWT;
import io.jsonwebtoken.ExpiredJwtException;
import org.apache.commons.lang3.StringUtils;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.authservice.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ContextsDTO;

@Component
public class AuthenticationValidation {

    private final RedisTemplate<String, String> redisTemplate;
    private final HelperService helperService;

    public AuthenticationValidation(RedisTemplate<String, String> redisTemplate, HelperService helperService) {
        this.redisTemplate = redisTemplate;
        this.helperService = helperService;
    }

    /**
     * <p>
     * Used to validate the authentication token
     * </p>
     *
     * @param jwtToken - jwt token of the logged-in user
     * @return UserDTO - user information
     */
    public ContextsDTO validateAspect(String jwtToken, String client, String cookie)
            throws ParseException {
        if (StringUtils.isBlank(jwtToken)) {
            jwtToken = Constants.SPACE;
        } else {
            jwtToken = jwtToken.substring(Constants.BEARER.length());
        }
        EncryptedJWT jwt = helperService.getJWT(jwtToken);
        Object tenants = jwt.getJWTClaimsSet().getClaim(Constants.TENANT_IDS_CLAIM);
        UserContextDTO userDetail = null;
        Object rawJson = jwt.getJWTClaimsSet().getClaim(Constants.USER_DATA);
        ObjectMapper objectMapper = new ObjectMapper();
        userDetail = objectMapper.convertValue(rawJson, UserContextDTO.class);
        userDetail.setAuthorization(jwtToken);
        String key = Constants.SPICE + Constants.COLON +
                Constants.LOGIN + Constants.COLON + userDetail.getUsername() +
                Constants.COLON + userDetail.getAuthorization();
        List<String> redisKeyList = redisTemplate.opsForList().range(key, Constants.ZERO, Constants.ONE);
        if (null == redisKeyList || redisKeyList.isEmpty()) {
            throw new ExpiredJwtException(null, null, ErrorConstants.TOKEN_EXPIRED);
        }
        userDetail.setClient(client);
        if (Objects.nonNull(client) && (Constants.CLIENT_WEB.equals(client))) {
            userDetail.setCookie(cookie);
        }
        redisTemplate.expire(key, Constants.AUTH_TOKEN_EXPIRY_MINUTES, TimeUnit.MINUTES);
        return new ContextsDTO(userDetail, tenants);
    }
}
