package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.repository.UserTermsAndConditionsRepository;
import com.mdtlabs.coreplatform.adminservice.service.UserTermsAndConditionsService;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;

/**
 * <p>
 * This service class maintains the CRUD operations for Terms and condition customization.
 * </p>
 *
 * @author Divya created on Oct 22, 2024
 */
@Service
public class UserTermsAndConditionsServiceImpl implements UserTermsAndConditionsService {

    private final UserTermsAndConditionsRepository userTermsAndConditionsRepository;

    @Autowired
    public UserTermsAndConditionsServiceImpl(UserTermsAndConditionsRepository userTermsAndConditionsRepository) {
        this.userTermsAndConditionsRepository = userTermsAndConditionsRepository;
    }

    /**
     * {@inheritDoc}
     */
    public UserTermsAndConditions getTermsAndConditionsValue(SearchRequestDTO requestDTO) {
        List<UserTermsAndConditions> userTermsAndConditionlist = userTermsAndConditionsRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getCountryId());
        if (Objects.isNull(userTermsAndConditionlist) || userTermsAndConditionlist.isEmpty()) {
            throw new DataNotFoundException(2300);
        }
        return userTermsAndConditionlist.getFirst();
    }
}
