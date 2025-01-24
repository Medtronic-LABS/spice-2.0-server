package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.adminservice.repository.DesignationRepository;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;


/**
 * <p>
 * This class has the test methods for Designation service implementation.
 * </p>
 *
 * @author Divya S
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DesignationServiceTest {

    @InjectMocks
    private DesignationServiceImpl designationServiceImpl;

    @Mock
    private DesignationRepository designationRepository;


    @Test
    void getAllDesignationTest() {
        //given
        TestDataProvider.init();
        Designation designation = TestDataProvider.getDesignation();
        RoleDTO roleDTO = new RoleDTO();
        roleDTO.setSuiteAccessName(Constants.CLIENT_ADMIN);
        roleDTO.setLevel(1L);
        Role role = TestDataProvider.getRole();
        role.setSuiteAccessName(Constants.CLIENT_ADMIN);
        List<Designation> designationList = List.of(designation);

        //when
        TestDataProvider.getStaticMock();
        when(designationRepository.getAllDesignations(any(), any())).thenReturn(designationList);
        List<Designation> response = designationServiceImpl.getAllDesignation(designation.getId());
        assertNotNull(response, "The response should not be null.");

        assertEquals(1, response.size(), "The returned list should contain exactly one designation.");
        assertSame(designation, response.get(0), "The returned designation should be the same as the mocked one.");
        TestDataProvider.cleanUp();
    }
}
