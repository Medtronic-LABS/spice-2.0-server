package com.mdtlabs.coreplatform.authservice.authenticationserver;

import static org.junit.Assert.assertThrows;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.authservice.helper.HelperService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.nimbusds.jose.EncryptionMethod;
import com.nimbusds.jose.JWEAlgorithm;
import com.nimbusds.jose.JWEHeader;
import com.nimbusds.jwt.EncryptedJWT;
import com.nimbusds.jwt.JWTClaimsSet;
import io.jsonwebtoken.ExpiredJwtException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.authservice.common.TestConstants;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;

import java.util.*;
import java.util.concurrent.TimeUnit;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AuthenticationValidationTest {

    @InjectMocks
    AuthenticationValidation authenticationValidation;

    @Mock
    private RedisTemplate<String, String> redisTemplate;

    @Mock
    private ListOperations<String, String> listOperations;

    @Mock
    private HelperService commonUtil;


    @Test
    void validateAspect() {
        String jwtToken = TestConstants.TOKEN;
        String client = "client";
        UserContextDTO userDetail = new UserContextDTO();
        userDetail.setUsername(TestConstants.USER_NAME);
        userDetail.setAuthorization(jwtToken);
        String key = Constants.SPICE + Constants.COLON +
                Constants.LOGIN + Constants.COLON + userDetail.getUsername() +
                Constants.COLON + userDetail.getAuthorization();
        List<Long> tenantIds = new ArrayList<>();
        Map<String, Object> userInfo = new HashMap<>();

        tenantIds.add(123L);
        JWTClaimsSet.Builder claimsSet = new JWTClaimsSet.Builder();
        claimsSet.claim(Constants.USER_DATA, userInfo);
        claimsSet.claim(Constants.TENANT_IDS_CLAIM, tenantIds);
        JWEHeader header = new JWEHeader(JWEAlgorithm.RSA_OAEP_256, EncryptionMethod.A128GCM);
        EncryptedJWT jwt = new EncryptedJWT(header, claimsSet.build());
        List<String> redisKeyList = List.of(TestConstants.REDIS_KEY);
        //when
        when(commonUtil.getJWT("iOiJBMTI4R0NNIiwiYWxnIjoiUlNBLU9BRVAtMjU2In0.R6sXOkgrpneFRbl2YJrvRXwq-iuN2CMWPuKoelr5GlcN-C0gwwJXSCHQl4wdbMZgvlHDcAk9K-uAQODJCebWHPYcAk78n5_pzpQPYxJ4MM89mz3JgA3k4bIqgRt2OHgIWUSKIweeC_wdDkZG9TfKkoYY94Y3QOv5_TNUS9jiwO7WYfU8L7d6tZQrAJYlDvKuvT5oR5X2bmfa55aIkadGlOc3c6RTz2eUwNCYjTiOvQZ4UzJ2Nt0QcaksPhN0EJtyWNLlc4P6cwYreNX8XUIQfO-RH_W7tRJtIwyCR4vGQoPR-iY5mMTZcjXzPD-pQ2UQ2A1xnt2bAil9-hh2fk4A8w.jbMIPjHGqfX3M-oZ.BRWupgAdmsEe4rUtSunb4YvdFQXU4C0AqIYTpNzMqFSxQYVCNfg3ZQQJ3Q18d0mczqSf74l0gRPLd-vlen6DDHxg6sJtWkA0sSDJm2O-rilJiqwgYf5NtXReUWtn_xJt0TlC2w2haRrs0PAPjEzYB069xLyI0fffvrR9KCXrc7w0_hISheWO9utUfBnoJ63mgVGCfUgoIwItj-1asBLcmNfivcAS62RQXUtqNU6fEdYUnNkXRzqSpt9_kHmz8aYCFvFRzen5qREjl8BRpMkVKDGFkTwt2aT_AhDmPn8OrMChC5QlO99pjzydSgnCoiM7Wh0sSpCySHXVARhWmM7dmamDC6P7WGF8u-JDQlf7lSEG1xqbj9oca-iXR8v0EMknJvzcJo5HOJWRwY8QEF0Y8P5TaqH9mcIwZWA2CRHnJP74urqKvu4j1nk58DJUCbcZtAJhc0plBI7SUci0TyDRmY_in03TOXKCyE5WghYckNcYRIFOsCGJ-uRN523D9y5qc_NzQXTDmIg1lrc3sy4jshKplgeiEeH7KtfD9o-rgu99d7EoZaK4SY-Rfm6yTC2hO6MMBBwSvwwS_z1YmGdGNrH9gIgmQPLUxFVVpKLVgSq81ugh7kXKHgXaWbDzMFT-ZjZh0aPVEU5uHDE1okEqVXbKJ0iecBfs2RS2NbHkgQIFmTKtaQhF2FNhhziz8ZqMRQ8T3TGh4GIdI3oCej4EdhJ1pvELmypPZ4nWTB5t_MArA7iX0zatc8IMzlPxTnS2vMoE90zi89qXP6RTjMrdCVTiVIlrZSx4YrZLsqSzDQsmYjwr7Qicjdnw4GhN1rcBbgov39IV4CotCDvU5vs-1Crl4LCDw_08e6qMQE1yfmhyHIGLlmQap2za-Ujh6N2NL2cI-yyXhFK_ypXNrRDaER5YkrX1zGr_y0quW7D4HGwPExNNn4Vvt6aNUfazM75SFeDhSwrPUqa_1y2h9ApDDzQZXdxRzDy4Frj9N1dsYFwUfGcZcljnfPAjdoa8xRNOoUrrzVQNNNRsn0O8ex8S8rye9-FzRvz6KoOkQ0L51gNRZKyjq16y0ozm0mgDlz5w15hx5K6WA8RD8fuSGQ6EJ7qT9xtxzBd5NYvVOUqwr5_1a8rzH_Mx5oCJPf96Ty2tpNICcdHkwnnRfXHJi9ooHG3Lrt-xmCjzR_njugDwm1wwTjR8HP78_Zu55dhb1-CX5_x6QA-10tqDnITO9nPvsJRRft8-DmRVMIEwPw9Z7aw-e-KtML-AXGp3FlHS881kbt9eEBhv3D22m2z0QSrWx_tcq6HGUSsiGcivOr0mAlGyIS2aNTt9zXojBCot_M4cYygBjNAwR_lReNB1CBU.b_CUQ9aySmgIEoJpTVfigw")).thenReturn(jwt);
        when(redisTemplate.opsForList()).thenReturn(listOperations);
        when(listOperations.range(key, Constants.ZERO, Constants.ONE)).thenReturn(redisKeyList);
        when(redisTemplate.expire(key, Constants.AUTH_TOKEN_EXPIRY_MINUTES, TimeUnit.MINUTES)).thenReturn(null);
        assertThrows(ExpiredJwtException.class, () -> authenticationValidation.validateAspect(jwtToken, client, null));

    }
}
