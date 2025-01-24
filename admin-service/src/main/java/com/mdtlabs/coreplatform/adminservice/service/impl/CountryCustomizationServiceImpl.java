package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.dto.CustomizationRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.CountryCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.service.CountryCustomizationService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;

/**
 * <p>
 *   This service class maintains the CRUD operations for country customization.
 * </p>
 *
 * @author Karthick M created on Jun30, 2022
 *
 */
@Service
public class CountryCustomizationServiceImpl implements CountryCustomizationService {
	
    private final CountryCustomizationRepository countryCustomizationRepository;
    private ModelMapper modelMapper = new ModelMapper();

    @Autowired
    public CountryCustomizationServiceImpl(CountryCustomizationRepository countryCustomizationRepository) {
        this.countryCustomizationRepository = countryCustomizationRepository;
    }

    /**
     * {@inheritDoc}
     */
    public CountryCustomization createCustomization(CountryCustomization countryCustomization) {
        return countryCustomizationRepository.save(countryCustomization);
    }

    /**
     * {@inheritDoc}
     */
    public CountryCustomization getCustomization(CustomizationRequestDTO customizationRequestDto) {
        List<CountryCustomization> countryCustomization = countryCustomizationRepository.findByCountryIdAndCategoryAndTypeAndTenantId(
            customizationRequestDto.getCountryId(), customizationRequestDto.getCategory(),
            customizationRequestDto.getType(), false, customizationRequestDto.getTenantId(), customizationRequestDto.getCultureId());
        if (Objects.isNull(countryCustomization) || countryCustomization.isEmpty()) {
            throw new DataNotFoundException(2251);
        }
        return countryCustomization.get(Constants.ZERO);
    }

    /**
     * {@inheritDoc}
     */
    public CountryCustomization updateCustomization(CountryCustomization countryCustomization) {
        CountryCustomization existingCountryCustomization = countryCustomizationRepository
            .findByIdAndTenantIdAndIsDeletedFalse(countryCustomization.getId(), countryCustomization.getTenantId());
        if (Objects.isNull(existingCountryCustomization)) {
            throw new DataNotFoundException(2151);
        }
        modelMapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        modelMapper.map(countryCustomization, existingCountryCustomization);
        return countryCustomizationRepository.save(existingCountryCustomization);
    }

    /**
     * {@inheritDoc}
     */
    public List<CountryCustomization> getCountryCustomizations(long cultureId) {
        Long countryId = UserContextHolder.getUserDto().getCountry().getId();
        return countryCustomizationRepository.findByCountryIdAndCultureId(countryId, cultureId);
    }

    /**
     * {@inheritDoc}
     */
    public List<CountryCustomization> getCountryCustomizationsByCategory(long cultureId, String category) {
        Long countryId = UserContextHolder.getUserDto().getCountry().getId();
        return countryCustomizationRepository.findByCountryIdAndCultureIdAndCategory(countryId, cultureId, category);
    }
}