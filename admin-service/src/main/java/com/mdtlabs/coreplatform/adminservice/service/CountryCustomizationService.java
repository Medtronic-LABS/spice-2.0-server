package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;


/**
 * <p>
 *   This service class maintains the CRUD operations for country customization.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2022
 */
public interface CountryCustomizationService {

    /**
     * <p>
     *   This method is used to add a country customization form data.
     * </p>
     *
     * @param regionCustomization {@link CountryCustomization} Region Customization entity
     * @return {@link CountryCustomization} CountryCustomization entity.
     */
    public CountryCustomization createCustomization(CountryCustomization regionCustomization);

    /**
     * <p>
     *   Get the region customization data details such as screening, enrollment and
     *   consent forms based on region customization id, type and category.
     * </p>
     *
     * @param customizationRequestDto {@link CustomizationRequestDTO} entity
     * @return {@link CountryCustomization} CountryCustomization entity
     */
    public CountryCustomization getCustomization(CustomizationRequestDTO customizationRequestDto);

    /**
     * <p>
     *   Update region customization data like screening, enrollment forms and consent
     *   data based on country id and region customization id.
     * </p>
     *
     * @param regionCustomization {@link CountryCustomization} Region customization entity
     * @return {@link CountryCustomization}  Region customization entity
     */
    public CountryCustomization updateCustomization(CountryCustomization regionCustomization);

    /**
     * <p>
     *   To get list of region customized data.
     * </p>
     *
     * @param requestData {@link Map<String, Object>} Request data containing country Id, tenantId, etc.,
     * @return {@link List<CountryCustomization>} List of CountryCustomization Entity.
     */
    public List<CountryCustomization> getCountryCustomizations(long cultureId);

    /**
     * <p>
     *   To get list of region customized data.
     * </p>
     *
     * @param requestData {@link Map<String, Object>} Request data containing country Id, tenantId, etc.,
     * @return {@link List<CountryCustomization>} List of CountryCustomization Entity.
     */
    public List<CountryCustomization> getCountryCustomizationsByCategory(long cultureId, String category);


}
