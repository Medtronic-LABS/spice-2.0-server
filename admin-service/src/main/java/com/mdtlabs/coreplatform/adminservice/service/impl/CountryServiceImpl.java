package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.repository.CountryRepository;
import com.mdtlabs.coreplatform.adminservice.service.CountryService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CountryRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;

/**
 * <p>
 * This service class contain all the business logic for country module and
 * perform all the country operation here.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
@Service
public class CountryServiceImpl implements CountryService {

    private final CountryRepository countryRepository;
    private final UserServiceApiInterface userServiceApiInterface;

    @Autowired
    public CountryServiceImpl(CountryRepository countryRepository, UserServiceApiInterface userServiceApiInterface) {
        this.countryRepository = countryRepository;
        this.userServiceApiInterface = userServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public Country createCountry(CountryRequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        List<Country> existingCountryByCodeOrName = countryRepository
                .findByPhoneNumberCodeOrNameIgnoreCaseAndIsActiveTrueAndIsDeletedFalse(request.getPhoneNumberCode(), request.getName().strip());
        if (!existingCountryByCodeOrName.isEmpty()) {
            throw new DataConflictException(19001, request.getName());
        }
        Country country = mapper.map(request, Country.class);
        country.setDisplayValues(Constants.COUNTRY_DEFAULT_DISPLAY_VALUES);
        countryRepository.save(country);
        return country;
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO addRegionAdmin(UserRequestDTO request) {
        return userServiceApiInterface.addAdmin(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), request).getBody();
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO updateRegionAdmin(UserRequestDTO request) {
        return userServiceApiInterface.updateAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), request).getBody();
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO removeAdmin(SearchRequestDTO request) {
        return userServiceApiInterface.removeAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), request).getBody();
    }

    /**
     * {@inheritDoc}
     */
    public CountryDTO getCountryDetails(SearchRequestDTO request) {
        if (Objects.isNull(request.getId())) {
            throw new BadRequestException(1001);
        }

        Country country = countryRepository.getCountryById(request.getId(), request.getTenantId(), false, true);
        if (Objects.isNull(country)) {
            throw new DataNotFoundException(19004);
        }
        return new ModelMapper().map(country, CountryDTO.class);
    }

    /**
     * {@inheritDoc}
     */
    public Country updateCountry(CountryRequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        Country country = countryRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getId());
        if (Objects.isNull(country)) {
            throw new DataNotFoundException(19004);
        }
        mapper.map(request, country);
        return countryRepository.save(country);
    }

    /**
     * {@inheritDoc}
     */
    public Page<Country> getCountries(String searchTerm, Pageable pageable) {
        return countryRepository.searchCountries(searchTerm, pageable);
    }
}
