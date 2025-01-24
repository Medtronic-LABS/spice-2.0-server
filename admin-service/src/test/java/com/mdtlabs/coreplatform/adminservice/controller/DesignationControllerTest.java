package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.impl.DesignationServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DesignationListResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;


/**
 * <p>
 * This class has the test methods for Designation controller.
 * </p>
 *
 * @author Divya S
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DesignationControllerTest {

    @InjectMocks
    private DesignationController designationController;

    @Mock
    private DesignationServiceImpl designationService;

    @Mock
    private ModelMapper modelMapper;

    @Test
    void getAllDesignationTest() {
        Designation designation = TestDataProvider.getDesignation();
        DesignationListResponseDTO designationListResponseDTO = TestDataProvider.getDesignationListResponseDTO();

        when(designationService.getAllDesignation(TestConstants.ONE)).thenReturn(List.of(designation));
        when(modelMapper.map(designation, DesignationListResponseDTO.class)).thenReturn(designationListResponseDTO);

        SuccessResponse<List<DesignationListResponseDTO>> designationResponse = designationController.getAllDesignation(TestConstants.ONE);
        assertNotNull(designationResponse);

    }

    @Test
    void getEmptyDesignationTest() {
        Designation designation = TestDataProvider.getDesignation();

        when(designationService.getAllDesignation(TestConstants.ONE)).thenReturn(new ArrayList<>());
        when(modelMapper.map(designation, DesignationListResponseDTO.class)).thenReturn(new DesignationListResponseDTO());

        SuccessResponse<List<DesignationListResponseDTO>> designationResponse = designationController.getAllDesignation(TestConstants.ONE);
        assertNotNull(designationResponse);
        Assertions.assertEquals(HttpStatus.OK, designationResponse.getStatusCode());

    }
}
