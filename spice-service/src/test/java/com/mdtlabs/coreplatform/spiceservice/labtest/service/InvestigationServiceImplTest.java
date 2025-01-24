package com.mdtlabs.coreplatform.spiceservice.labtest.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestCustomizationDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.labtest.service.impl.InvestigationServiceImpl;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InvestigationServiceImplTest {

    @InjectMocks
    private InvestigationServiceImpl investigationService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Test
    @DisplayName("createOrUpdateInvestigation")
    void createOrUpdateInvestigation() {
        TestDataProvider.init();
        LabTestRequestDTO requestDTO = TestDataProvider.getLabTestRequestDTO();
        Map<String, String> response = Map.of("id", "123");

        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.createOrUpdateInvestigation(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), CommonUtil.getTenantId(), requestDTO))
                .thenReturn(response);

        Map<String, String> actualResponse = investigationService.createOrUpdateInvestigation(requestDTO);
        Assertions.assertNotNull(actualResponse);
        assertEquals(response, actualResponse);
        TestDataProvider.cleanUp();
    }

    @Test
    @DisplayName("getInvestigatedDetails")
    void getInvestigatedDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getInvestigatedDetails("BearerTest", "mob", requestDTO))
                .thenReturn(labTestDTO);
        //then
        LabTestDTO actualLabTestDTO = investigationService.getInvestigatedDetails(requestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(actualLabTestDTO);
        assertEquals(labTestDTO, actualLabTestDTO);
    }

    @Test
    @DisplayName("getInvestigationsByEncounter")
    void getInvestigationsByEncounter() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<LabTestDTO> labTestDTOList = List.of(TestDataProvider.getLabTestDTO());
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getInvestigationsByEncounter("BearerTest", "mob", requestDTO))
                .thenReturn(labTestDTOList);
        //then
        List<LabTestDTO> actualLabTestDTOList = investigationService.getInvestigationsByEncounter(requestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(actualLabTestDTOList);
        assertEquals(labTestDTOList, actualLabTestDTOList);
        assertEquals(labTestDTOList.size(), actualLabTestDTOList.size());
    }

    @Test
    @DisplayName("removeInvestigation")
    void removeInvestigation() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Map<String, String> response = Map.of("id", "123");
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.removeInvestigation("BearerTest", "mob", requestDTO))
                .thenReturn(response);
        //then
        Map<String, String> actualResponse = investigationService.removeInvestigation(requestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(actualResponse);
        assertEquals(response, actualResponse);
    }

    @Test
    @DisplayName("getHistoryInvestigatedDetails")
    void getHistoryInvestigatedDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        LabTestHistoryDTO labTestHistoryDTO = TestDataProvider.getLabTestHistoryDTO();
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getHistoryInvestigatedDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO, CommonUtil.getTenantId()))
                .thenReturn(labTestHistoryDTO);
        //then
        LabTestHistoryDTO actualLabTestDTO = investigationService.getHistoryInvestigatedDetails(requestDTO);
        Assertions.assertNotNull(actualLabTestDTO);
        assertEquals(labTestHistoryDTO, actualLabTestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void testUpdateInvestigationResult() {
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();
        investigationService.updateInvestigationResult(labTestRequestDTO);
        verify(fhirServiceApiInterface).updateInvestigationResult(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                labTestRequestDTO
        );
        TestDataProvider.cleanUp();
    }

    @Test
    void testReviewInvestigation() {
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        RequestDTO request = TestDataProvider.getRequestDTO();
        investigationService.reviewInvestigation(request);
        verify(fhirServiceApiInterface).reviewInvestigation(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                request);

        TestDataProvider.cleanUp();
    }

    @Test
    void testUpdateInvestigationResultkl() {

        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();

        LabTestRequestDTO labTestRequestDTO = TestDataProvider.getLabTestRequestDTO();
        investigationService.updateInvestigationResult(labTestRequestDTO);
        verify(fhirServiceApiInterface).updateInvestigationResult(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                labTestRequestDTO
        );
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetIntensificationDetails() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        RequestDTO request = TestDataProvider.getRequestDTO();
        Map<String, List<LabTestDTO>> expectedResponse = null;
        when(fhirServiceApiInterface.getIntensificationDetails(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                request)).thenReturn(expectedResponse);

        // Act: Call the method under test
        Map<String, List<LabTestDTO>> actualResponse = investigationService.getIntensificationDetails(request);

        // Assert: Verify the response and interactions
        assertEquals(expectedResponse, actualResponse);
        verify(fhirServiceApiInterface).getIntensificationDetails(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                request
        );
        TestDataProvider.cleanUp();
    }


    @Test
    public void testGetLabTestCustomization() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        LabTestCustomizationDTO expectedCustomization = new LabTestCustomizationDTO();
        when(adminServiceApiInterface.getLabTestCustomization(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                requestDTO)).thenReturn(expectedCustomization);


        LabTestCustomizationDTO actualCustomization = investigationService.getLabTestCustomization(requestDTO);

        assertEquals(expectedCustomization, actualCustomization);
        verify(adminServiceApiInterface).getLabTestCustomization(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                requestDTO
        );
        TestDataProvider.cleanUp();
    }
}
