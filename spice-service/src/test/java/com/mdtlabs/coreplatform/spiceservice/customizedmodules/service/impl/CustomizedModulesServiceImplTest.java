package com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.impl;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.model.CustomizedModule;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.repository.CustomizedModuleRepository;

/**
 * <p>
 * CustomizedModulesServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in CustomizedModulesService class.
 * </p>
 *
 * @author Divya S created on Oct 24, 2022
 */
@ExtendWith(MockitoExtension.class)
class CustomizedModulesServiceImplTest {

    @InjectMocks
    private CustomizedModulesServiceImpl customizedModulesService;
    @Mock
    private CustomizedModuleRepository customizedModuleRepository;

    @Test
    @DisplayName("CreateCustomizedModules Test")
    void createCustomizedModules() {
        //given
        String type = Constants.TYPE;
        Map<String, Object> module = Map.of(Constants.ID, Constants.LONG_ONE);
        List<Map<String, Object>> modules = List.of(module);
        List<CustomizedModule> customizedModules = List.of(TestDataProvider.getCustomizedModule());
        customizedModules.getFirst().setModuleValue(module);
        //when
        when(customizedModuleRepository.saveAll(customizedModules)).thenReturn(customizedModules);
        //then
        customizedModulesService.createCustomizedModules(modules, type, Constants.STRING_ONE, Constants.STRING_ONE);
        verify(customizedModuleRepository, atLeastOnce()).saveAll(customizedModules);
    }

    @Test
    void updateCustomizedModulesForTransfer() {
        //given
        List<CustomizedModule> customizedModules = List.of(TestDataProvider.getCustomizedModule());

        //when
        when(customizedModuleRepository.findByMemberIdAndIsActiveTrueAndIsDeletedFalse(Constants.STRING_ONE)).thenReturn(customizedModules);
        when(customizedModuleRepository.saveAll(customizedModules)).thenReturn(customizedModules);

        //then
        customizedModulesService.updateCustomizedModulesForTransfer(Constants.STRING_ONE, Constants.LONG_ONE);
        verify(customizedModuleRepository, atLeastOnce()).findByMemberIdAndIsActiveTrueAndIsDeletedFalse(Constants.STRING_ONE);
    }

    @Test
    void updateCustomizedModules() {
        //given
        List<CustomizedModule> customizedModules = List.of(TestDataProvider.getCustomizedModule());

        //when
        when(customizedModuleRepository.findByMemberIdAndPatientIdIsNullAndIsActiveTrueAndIsDeletedFalse(Constants.STRING_ONE)).thenReturn(customizedModules);
        when(customizedModuleRepository.saveAll(customizedModules)).thenReturn(customizedModules);

        //then
        customizedModulesService.updateCustomizedModules(Constants.STRING_ONE, Constants.STRING_ONE);
        verify(customizedModuleRepository, atLeastOnce()).findByMemberIdAndPatientIdIsNullAndIsActiveTrueAndIsDeletedFalse(Constants.STRING_ONE);
    }
}
