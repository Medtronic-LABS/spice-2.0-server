package com.mdtlabs.coreplatform.adminservice.controller;

import com.mdtlabs.coreplatform.adminservice.model.dto.CountryCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.service.CountryCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CountryCustomizationControllerTest {

    @Mock
    private CountryCustomizationService countryCustomizationService;

    @InjectMocks
    private CountryCustomizationController countryCustomizationController;


    @Test
    public void testAddCustomization() {
        CountryCustomizationDTO customizationDto = new CountryCustomizationDTO();

        when(countryCustomizationService.createCustomization(any(CountryCustomization.class))).thenReturn(null);

        countryCustomizationController.addCustomization(customizationDto);
        verify(countryCustomizationService, times(1)).createCustomization(any(CountryCustomization.class));
    }

    @Test
    public void testGetCustomization() {
        CustomizationRequestDTO requestDto = new CustomizationRequestDTO();
        CountryCustomization customization = new CountryCustomization();

        when(countryCustomizationService.getCustomization(requestDto)).thenReturn(customization);

        countryCustomizationController.getCustomization(requestDto);

        verify(countryCustomizationService, times(1)).getCustomization(any(CustomizationRequestDTO.class));
    }

    @Test
    public void testUpdateCustomization() {
        CountryCustomizationDTO customizationDto = new CountryCustomizationDTO();

        when(countryCustomizationService.updateCustomization(any(CountryCustomization.class))).thenReturn(null);

        countryCustomizationController.updateCustomization(customizationDto);
        verify(countryCustomizationService, times(1)).updateCustomization(any(CountryCustomization.class));
    }

    @Test
    public void testGetCountryCustomizations() {
        long cultureId = 1L;
        List<CountryCustomization> customizations = new ArrayList<>();

        when(countryCustomizationService.getCountryCustomizations(cultureId)).thenReturn(customizations);

        countryCustomizationController.getCountryCustomizations(cultureId);
        verify(countryCustomizationService, times(1)).getCountryCustomizations(cultureId);
    }

    @Test
    public void testGetCountryCustomizationsByCategory() {
        long cultureId = 1L;
        String category = "exampleCategory";
        List<CountryCustomization> customizations = new ArrayList<>();

        when(countryCustomizationService.getCountryCustomizationsByCategory(cultureId, category)).thenReturn(customizations);

        countryCustomizationController.getCountryCustomizationsByCategory(cultureId, category);

        verify(countryCustomizationService, times(1)).getCountryCustomizationsByCategory(cultureId, category);
    }

}