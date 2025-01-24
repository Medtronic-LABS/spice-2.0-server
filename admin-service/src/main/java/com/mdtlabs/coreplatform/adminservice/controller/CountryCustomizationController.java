package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;
import java.util.Map;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.CountryCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.service.CountryCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;

import jakarta.validation.Valid;


/**
 * <p>
 *   This controller class maintains CRUD operation for country customization data.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2022
 */
@RestController
@RequestMapping("/country-customization")
public class CountryCustomizationController {

    private final CountryCustomizationService countryCustomizationService;
    private ModelMapper modelMapper = new ModelMapper();

    @Autowired
    public CountryCustomizationController(CountryCustomizationService countryCustomizationService) {
        this.countryCustomizationService = countryCustomizationService;
    }

    /**
     * <p>
     *   This method is used to add a country customization form data.
     * </p>
     *
     * @param countryCustomizationDto {@link CountryCustomization} country customization entity
     * @return {@link SuccessResponse CountryCustomization} CountryCustomization entity
     */
    @UserTenantValidation
    @PostMapping("/create")
    public SuccessResponse<CountryCustomization> addCustomization(@Valid @RequestBody 
    CountryCustomizationDTO countryCustomizationDto) {
        countryCustomizationService
                .createCustomization(modelMapper.map(countryCustomizationDto, CountryCustomization.class));
        return new SuccessResponse<>(SuccessCode.COUNTRY_CUSTOMIZATION_SAVE, HttpStatus.OK);
    }

    /**
     * <p>
     *   Get the country customization data details such as screening, enrollment and
     *   consent forms based on country organization id.
     * </p>
     *
     * @param customizationRequestDto {@link CustomizationRequestDTO} Customization request dto
     * @return {@link SuccessResponse CountryCustomization} CountryCustomization entity
     */
    @UserTenantValidation
    @PostMapping("/details")
    public SuccessResponse<CountryCustomization> getCustomization(@Valid @RequestBody 
    	CustomizationRequestDTO customizationRequestDto) {
        return new SuccessResponse<>(SuccessCode.GET_COUNTRY_CUSTOMIZATION,
                countryCustomizationService.getCustomization(customizationRequestDto), HttpStatus.OK);
    }

    /**
     * <p>
     *   Update country customization data like screening, enrollment forms and consent
     *   data based on country.
     * </p>
     *
     * @param countryCustomization {@link CountryCustomization}  country customization entity
     * @return {@link SuccessResponse CountryCustomization} country customization entity
     */
    @UserTenantValidation
    @PutMapping("/update")
    public SuccessResponse<CountryCustomization> updateCustomization(@Valid @RequestBody 
    CountryCustomizationDTO countryCustomizationDto) {
        countryCustomizationService
                .updateCustomization(modelMapper.map(countryCustomizationDto, CountryCustomization.class));
        return new SuccessResponse<>(SuccessCode.COUNTRY_CUSTOMIZATION_UPDATE, HttpStatus.OK);
    }

    /**
     * <p>
     *   Gets list of customizations.
     * </p>
     *
     * @param requestData {@link Map} Request data map
     * @return {@link List} List of CountryCustomizations
     */
    @PostMapping("/static-data/get-list/{cultureId}")
    public List<CountryCustomization> getCountryCustomizations(@PathVariable("cultureId") long cultureId) {
        return countryCustomizationService.getCountryCustomizations(cultureId);
    }

    /**
     * <p>
     *   Gets list of customizations.
     * </p>
     *
     * @param requestData {@link Map} Request data map
     * @return {@link List} List of CountryCustomizations
     */
    @PostMapping("/static-data/get-list/{cultureId}/{category}")
    public List<CountryCustomization> getCountryCustomizationsByCategory(@PathVariable("cultureId") long cultureId, @PathVariable("category") String category) {
        return countryCustomizationService.getCountryCustomizationsByCategory(cultureId, category);
    }
}
