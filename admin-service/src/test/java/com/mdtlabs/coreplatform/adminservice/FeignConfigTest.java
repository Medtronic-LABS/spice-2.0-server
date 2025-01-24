package com.mdtlabs.coreplatform.adminservice;

import feign.Request;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.mdtlabs.coreplatform.adminservice.util.TestConstants;

/**
 * <p>
 * FeignConfigTest class used to test all possible positive
 * and negative cases for all methods and conditions used in FeignConfig class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on July 2, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FeignConfigTest {

    @InjectMocks
    FeignConfig feignConfig;

    @Test
    void requestOptions() {
        ReflectionTestUtils.setField(feignConfig, TestConstants.READ_TIME_OUT, TestConstants.INT_ONE);
        ReflectionTestUtils.setField(feignConfig, TestConstants.CONNECTION_TIME_OUT, TestConstants.INT_ONE);
        Request.Options options = feignConfig.requestOptions();
        assertEquals(TestConstants.INT_ONE, options.connectTimeoutMillis());
        assertEquals(TestConstants.INT_ONE, options.readTimeoutMillis());
    }
}
