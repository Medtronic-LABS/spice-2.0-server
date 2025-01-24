package com.mdtlabs.coreplatform.adminservice.service.impl;


import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.CountryCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CountryCustomizationServiceImplTest {

    @Mock
    private CountryCustomizationRepository countryCustomizationRepository;

    @InjectMocks
    private CountryCustomizationServiceImpl countryCustomizationService;

    @BeforeEach
    public void setup() {
        TestDataProvider.init();
    }

    @AfterEach
    public void close() {
        TestDataProvider.cleanUp();
    }

    @Test
    void testCreateCustomization_Success() {
        CountryCustomization countryCustomization = new CountryCustomization();
        when(countryCustomizationRepository.save(any(CountryCustomization.class))).thenReturn(countryCustomization);

        CountryCustomization result = countryCustomizationService.createCustomization(countryCustomization);

        assertNotNull(result);
        verify(countryCustomizationRepository, times(1)).save(countryCustomization);
    }

    @Test
    void testGetCustomization_Success() {
        CustomizationRequestDTO requestDTO = new CustomizationRequestDTO();
        requestDTO.setCountryId(1L);
        requestDTO.setCategory("category1");
        requestDTO.setType("type1");
        requestDTO.setTenantId(1L);
        requestDTO.setCultureId(1L);

        CountryCustomization customization = new CountryCustomization();
        when(countryCustomizationRepository.findByCountryIdAndCategoryAndTypeAndTenantId(
                anyLong(), anyString(), anyString(), eq(false), anyLong(), anyLong()))
                .thenReturn(List.of(customization));

        CountryCustomization result = countryCustomizationService.getCustomization(requestDTO);

        assertNotNull(result);
        Assertions.assertEquals(customization, result);
    }

    @Test
    void testGetCustomization_NotFound_ThrowsDataNotFoundException() {
        CustomizationRequestDTO requestDTO = new CustomizationRequestDTO();
        requestDTO.setCountryId(1L);
        requestDTO.setCategory("category1");
        requestDTO.setType("type1");
        requestDTO.setTenantId(1L);
        requestDTO.setCultureId(1L);

        when(countryCustomizationRepository.findByCountryIdAndCategoryAndTypeAndTenantId(
                anyLong(), anyString(), anyString(), eq(false), anyLong(), anyLong()))
                .thenReturn(Collections.emptyList());

        assertThrows(DataNotFoundException.class, () -> countryCustomizationService.getCustomization(requestDTO));
    }

    @Test
    void testUpdateCustomization_Success() {
        TestDataProvider.getStaticMock();
        CountryCustomization countryCustomization = new CountryCustomization();
        countryCustomization.setId(1L);
        countryCustomization.setTenantId(1L);

        CountryCustomization existingCustomization = new CountryCustomization();
        when(countryCustomizationRepository.findByIdAndTenantIdAndIsDeletedFalse(anyLong(), anyLong()))
                .thenReturn(existingCustomization);

        when(countryCustomizationRepository.save(any(CountryCustomization.class))).thenReturn(existingCustomization);

        CountryCustomization result = countryCustomizationService.updateCustomization(countryCustomization);

        assertNotNull(result);
        verify(countryCustomizationRepository, times(1)).save(existingCustomization);
    }

    @Test
    void testUpdateCustomization_NotFound_ThrowsDataNotFoundException() {
        TestDataProvider.getStaticMock();
        CountryCustomization countryCustomization = new CountryCustomization();
        countryCustomization.setId(1L);
        countryCustomization.setTenantId(1L);

        when(countryCustomizationRepository.findByIdAndTenantIdAndIsDeletedFalse(anyLong(), anyLong()))
                .thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> countryCustomizationService.updateCustomization(countryCustomization));
    }

    @Test
    void testGetCountryCustomizations_Success() {
        TestDataProvider.getStaticMock();
        List<CountryCustomization> customizations = List.of(new CountryCustomization());
        when(countryCustomizationRepository.findByCountryIdAndCultureId(UserContextHolder.getUserDto().getCountry().getId(), 1L)).thenReturn(customizations);

        List<CountryCustomization> result = countryCustomizationService.getCountryCustomizations(1L);

        assertNotNull(result);
        Assertions.assertEquals(1, result.size());
    }

    @Test
    void testGetCountryCustomizationsByCategory_Success() {
        TestDataProvider.getStaticMock();
        List<CountryCustomization> customizations = List.of(new CountryCustomization());
        when(countryCustomizationRepository.findByCountryIdAndCultureIdAndCategory(UserContextHolder.getUserDto().getCountry().getId(), 1L, "category1"))
                .thenReturn(customizations);

        List<CountryCustomization> result = countryCustomizationService.getCountryCustomizationsByCategory(1L, "category1");

        assertNotNull(result);
        Assertions.assertEquals(1, result.size());
    }
}