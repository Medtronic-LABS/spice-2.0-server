package com.mdtlabs.coreplatform.fhirmapper.converter;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;

import org.hl7.fhir.r4.model.Location;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class LocationConverterTest {

    @InjectMocks
    LocationConverter locationConverter;

    @Test
    void createLocationTest() {
        Location location = null;

        // when
        location = locationConverter.createLocation(TestConstants.LANDMARK,
                TestConstants.ONE_TWO_THREE, TestConstants.ONE_TWO_THREE_FOUR);
        Assertions.assertNotNull(location);
    }
}
