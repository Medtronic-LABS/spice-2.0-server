package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CommonUtilsTest {

    @InjectMocks
    CommonUtils commonUtils;

    @Test
    void gestationalAge() {
        for (int i = 0; i < 50; i++) {
            Assertions.assertNotNull(commonUtils.gestationalAge(i));
        }
    }

    @Test
    void birthWeight() {
        for (double i = 0; i < 10; i = i + 0.5) {
            Assertions.assertNotNull(commonUtils.birthWeight(i));
        }
    }
}