package com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.impl;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.common.model.CustomizedModule;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.repository.CustomizedModuleRepository;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;

/**
 * This class contains business logic for CustomizedModules entity.
 *
 * @author Divya S
 * @since Oct 24, 2024
 */
@Service
public class CustomizedModulesServiceImpl implements CustomizedModulesService {

    private final CustomizedModuleRepository customizedModuleRepository;

    @Autowired
    public CustomizedModulesServiceImpl(CustomizedModuleRepository customizedModuleRepository) {
        this.customizedModuleRepository = customizedModuleRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createCustomizedModules(List<Map<String, Object>> modules, String type, String memberId, String patientId) {
        List<CustomizedModule> updatedModules = modules.stream().map(module -> {
            CustomizedModule customizedModule = new CustomizedModule();
            customizedModule.setModuleValue(module);
            customizedModule.setScreenType(type);
            customizedModule.setMemberId(memberId);
            customizedModule.setPatientId(patientId);
            customizedModule.setClinicalWorkflowId((Long.parseLong(module.get(Constants.ID).toString())));
            return customizedModule;
        }).filter(obj -> Constants.BOOLEAN_TRUE).toList();
        Logger.logInfo("Saving the customized modules");
        customizedModuleRepository.saveAll(updatedModules);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateCustomizedModules(String memberId, String patientId) {
        List<CustomizedModule> customizedModules = customizedModuleRepository.findByMemberIdAndPatientIdIsNullAndIsActiveTrueAndIsDeletedFalse(memberId);
        if (Objects.nonNull(customizedModules) && !customizedModules.isEmpty()) {
            customizedModules.forEach(module -> module.setPatientId(patientId));
            customizedModuleRepository.saveAll(customizedModules);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateCustomizedModulesForTransfer(String memberId, Long tenantId) {
        List<CustomizedModule> customizedModules = customizedModuleRepository.findByMemberIdAndIsActiveTrueAndIsDeletedFalse(memberId);
        if (!Objects.isNull(customizedModules) && !customizedModules.isEmpty()) {
            customizedModules.forEach(customizedModule -> customizedModule.setTenantId(tenantId));
            customizedModuleRepository.saveAll(customizedModules);
        }
    }
}
