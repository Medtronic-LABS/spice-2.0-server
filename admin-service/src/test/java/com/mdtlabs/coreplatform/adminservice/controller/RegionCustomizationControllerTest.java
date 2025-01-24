package com.mdtlabs.coreplatform.adminservice.controller;

import com.mdtlabs.coreplatform.adminservice.service.RegionCustomizationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class RegionCustomizationControllerTest {

    @Mock
    RegionCustomizationService regionCustomizationService;

    @InjectMocks
    RegionCustomizationController regionCustomizationController;

    @Test
    void testGetRegionCustomizationsByCategory() {
        regionCustomizationController.getRegionCustomizationsByCategory("s");
        verify(regionCustomizationService, times(1)).getRegionCustomizationsByCategory("s");
    }

}