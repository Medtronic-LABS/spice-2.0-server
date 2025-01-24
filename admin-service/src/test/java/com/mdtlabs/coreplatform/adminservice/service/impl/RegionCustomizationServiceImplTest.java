package com.mdtlabs.coreplatform.adminservice.service.impl;

import com.mdtlabs.coreplatform.adminservice.repository.RegionCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class RegionCustomizationServiceImplTest {

    @Mock
    private RegionCustomizationRepository regionCustomizationRepository;

    @InjectMocks
    private RegionCustomizationServiceImpl regionCustomizationService;


    @BeforeEach
    public void setup() {
        TestDataProvider.init();
    }

    @AfterEach
    public void close() {
        TestDataProvider.cleanUp();
    }

    @Test
    public void testGetRegionCustomizationsByCategory() {
        TestDataProvider.getStaticMock();
        regionCustomizationService.getRegionCustomizationsByCategory("s");
        verify(regionCustomizationRepository, times(1)).findByCountryIdAndCategoryAndIsActiveTrueAndIsDeletedFalse(UserContextHolder.getUserDto().getCountry().getId(), "s");
    }
}