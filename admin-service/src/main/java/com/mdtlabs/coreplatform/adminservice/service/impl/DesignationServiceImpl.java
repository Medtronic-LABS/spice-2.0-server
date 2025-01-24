package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.repository.DesignationRepository;
import com.mdtlabs.coreplatform.adminservice.service.DesignationService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;

/**
 * <p>
 * This service class contain all the business logic for Designation module and perform
 * all the designation operation here.
 * </p>
 *
 * @author Divya S created on Oct 18, 2024
 */
@Service
public class DesignationServiceImpl implements DesignationService {

    private final DesignationRepository designationRepository;

    @Autowired
    public DesignationServiceImpl(DesignationRepository designationRepository) {
        this.designationRepository = designationRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Designation> getAllDesignation(Long countryId) {
        Long level = getLoggedUserDefaultRole().getLevel();
        return designationRepository.getAllDesignations(countryId, level);
    }

    private RoleDTO getLoggedUserDefaultRole() {
        RoleDTO roleDTO = UserContextHolder.getUserDto().getRoles().stream().filter(role -> Constants.CLIENT_ADMIN.equals(role.getSuiteAccessName())).findFirst().orElse(null);

        if (Objects.isNull(roleDTO) || Objects.isNull(roleDTO.getLevel())) {
            throw new DataNotFoundException(13001);
        }
        return roleDTO;
    }
}
