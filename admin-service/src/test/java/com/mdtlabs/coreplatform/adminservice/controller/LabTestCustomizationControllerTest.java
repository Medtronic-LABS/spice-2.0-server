package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.service.LabTestCustomizationService;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class LabTestCustomizationControllerTest {

    @InjectMocks
    private LabTestCustomizationController labTestCustomizationController;

    @Mock
    private LabTestCustomizationService labTestCustomizationService;

    private LabTestCustomizationDTO labTestCustomizationDTO = TestDataProvider.getLabTestCustomizationDTO();
    @Test
    void createLabTestCustomizationTest() {
        //given
        SuccessResponse<LabTestCustomizationDTO> response = new SuccessResponse<>(SuccessCode.LABTEST_SAVE,
                labTestCustomizationDTO, HttpStatus.CREATED);
        //when
        when(labTestCustomizationService.createLabTestCustomization(labTestCustomizationDTO)).thenReturn(labTestCustomizationDTO);
        //then
        SuccessResponse<LabTestCustomizationDTO> actualResponse = labTestCustomizationController.createLabTestCustomization(labTestCustomizationDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.CREATED, actualResponse.getStatusCode());
    }

    @Test
    void getLabTestCustomizationTest() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setName("ur");
        //when
        when(labTestCustomizationService.getLabTestCustomization(requestDTO)).thenReturn(labTestCustomizationDTO);
        //then
        LabTestCustomizationDTO actualResponse = labTestCustomizationController
                .getLabTestCustomization(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(labTestCustomizationDTO, actualResponse);
        Assertions.assertEquals(labTestCustomizationDTO.getTestName(), actualResponse.getTestName());
    }

    @Test
    void getLabTestCustomizationByUniqueNameTest() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setName(Constants.NAME);
        SuccessResponse<LabTestCustomizationDTO> response = new SuccessResponse<>(SuccessCode.GET_LABTEST,
                labTestCustomizationDTO, HttpStatus.OK);
        //when
        when(labTestCustomizationService.getLabTestCustomizationByUniqueName(requestDTO))
                .thenReturn(labTestCustomizationDTO);
        //then
        SuccessResponse<LabTestCustomizationDTO> actualResponse = labTestCustomizationController
                .getLabTestCustomizationByUniqueName(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void listLabTestCustomizationTest() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        ResponseListDTO<LabTestCustomizationDTO> listDTO = new ResponseListDTO();
        listDTO.setData(List.of(labTestCustomizationDTO));
        SuccessResponse<LabTestCustomizationDTO> response = new SuccessResponse<>(SuccessCode.GET_LABTESTS,
                listDTO.getData(), listDTO.getTotalCount(), HttpStatus.OK);
        //when
        when(labTestCustomizationService.listLabTestCustomization(requestDTO))
                .thenReturn(listDTO);
        //then
        SuccessResponse<LabTestCustomizationDTO> actualResponse = labTestCustomizationController
                .listLabTestCustomization(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void updateLabTestCustomizationTest() {
        //given
        labTestCustomizationDTO.setFormInput(Constants.FORM_LAYOUT);
        SuccessResponse<LabTestCustomizationDTO> response = new SuccessResponse<>(SuccessCode.LABTEST_UPDATE,
                labTestCustomizationDTO, HttpStatus.ACCEPTED);
        //when
        when(labTestCustomizationService.updateLabTestCustomization(labTestCustomizationDTO))
                .thenReturn(labTestCustomizationDTO);
        //then
        SuccessResponse<LabTestCustomizationDTO> actualResponse = labTestCustomizationController
                .updateLabTestCustomization(labTestCustomizationDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.ACCEPTED, actualResponse.getStatusCode());
    }

    @Test
    void deleteLabTestCustomizationTest() {
        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        SuccessResponse<Boolean> response = new SuccessResponse<>(SuccessCode.LABTEST_DELETE,
                true, HttpStatus.OK);
        //when
        doNothing().when(labTestCustomizationService).deleteLabTestCustomization(searchRequestDTO);
        //then
        SuccessResponse<Boolean> actualResponse = labTestCustomizationController
               .deleteLabTestCustomization(searchRequestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    @DisplayName("validateLabTesCustomization")
    void validateLabTesCustomization() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        SuccessResponse<Boolean> response = new SuccessResponse<>(SuccessCode.LABTEST_VALIDATE, Boolean.TRUE, HttpStatus.OK);
        //when
        when(labTestCustomizationService.validateLabTestCustomization(requestDTO)).thenReturn(Boolean.TRUE);
        //then
        SuccessResponse<Boolean> actualResponse = labTestCustomizationController.validateLabTestCustomization(requestDTO);
        Assertions.assertNotNull(actualResponse);
        Assertions.assertEquals(response, actualResponse);
        Assertions.assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }
}
