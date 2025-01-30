package com.mdtlabs.coreplatform.spiceservice.bplog.service.impl;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.SymptomRepository;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class BpLogServiceImplTest {

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @InjectMocks
    private BpLogServiceImpl bpLogService;

    @Mock
    private FollowUpService followUpService;

    @Mock
    private SymptomRepository symptomRepository;

    @BeforeEach
    public void setup() {
        TestDataProvider.init();
    }

    @AfterEach
    public void wrap() {
        TestDataProvider.cleanUp();
    }

    @Test
    void testAddBpLog_Success() {
        TestDataProvider.getStaticMock();
        BpLogDTO bpLogDTO = new BpLogDTO();
        bpLogDTO.setRelatedPersonFhirId("relatedPersonId");
        bpLogDTO.setPatientId("patientId");
        bpLogDTO.setAssessmentOrganizationId("orgId");
        AssessmentDTO assessmentDTO = new AssessmentDTO();
        assessmentDTO.setPatientId("patient1");
        assessmentDTO.setNextBpAssessmentDate(Date.from(Instant.parse("2023-01-01T00:00:00Z")));

        //when
        when(fhirServiceApiInterface.assessmentCreate(anyString(), anyString(), any(AssessmentDTO.class))).thenReturn(assessmentDTO);
        //then
        BpLogDTO result = bpLogService.addBpLog(bpLogDTO);
        assertNotNull(result);
        assertEquals(Constants.ASSESSMENT_TYPE_MEDICAL_REVIEW, result.getType());
        verify(fhirServiceApiInterface, times(1)).assessmentCreate(anyString(), anyString(), any(AssessmentDTO.class));
    }

    @Test
    void testAddBpLog_ThrowsDataNotFoundException_WhenRelatedPersonFhirIdIsNull() {
        BpLogDTO bpLogDTO = new BpLogDTO();
        bpLogDTO.setPatientId("patientId");
        bpLogDTO.setAssessmentOrganizationId("orgId");

        assertThrows(DataNotFoundException.class, () -> bpLogService.addBpLog(bpLogDTO));
    }

    @Test
    void testAddBpLog_ThrowsDataNotFoundException_WhenPatientIdIsNull() {
        BpLogDTO bpLogDTO = new BpLogDTO();
        bpLogDTO.setRelatedPersonFhirId("relatedPersonId");

        assertThrows(DataNotFoundException.class, () -> bpLogService.addBpLog(bpLogDTO));
    }

    @Test
    void testAddBpLog_ThrowsDataNotFoundException_WhenAssessmentOrganizationIdIsNull() {
        BpLogDTO bpLogDTO = new BpLogDTO();
        bpLogDTO.setRelatedPersonFhirId("relatedPersonId");
        bpLogDTO.setPatientId("patientId");

        assertThrows(DataNotFoundException.class, () -> bpLogService.addBpLog(bpLogDTO));
    }

    @Test
    void testGetPatientBpLogsWithSymptoms_Success() {
        TestDataProvider.getStaticMock();
        RequestDTO requestData = new RequestDTO();
        requestData.setMemberId("memberId");

        PatientBpLogsDTO patientBpLogsDTO = new PatientBpLogsDTO();
        BpLogDTO bpLogDTO = new BpLogDTO();
        List<BpLogDTO> bpLogList = new ArrayList<>();
        bpLogList.add(bpLogDTO);
        patientBpLogsDTO.setBpLogList(bpLogList);

        List<String> symptoms = new ArrayList<>();
        String symptom = "sd";
        symptoms.add(symptom);
        bpLogDTO.setSymptoms(symptoms);
        patientBpLogsDTO.setLatestBpLog(bpLogDTO);
        when(fhirServiceApiInterface.getBpLogList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestData)).thenReturn(patientBpLogsDTO);

        List<Symptom> bpSymptomps = new ArrayList<>();
        bpSymptomps.add(new Symptom());
        when(symptomRepository.findByNameInAndType(any(),any())).thenReturn(bpSymptomps);

        PatientBpLogsDTO result = bpLogService.getPatientBpLogsWithSymptoms(requestData);
        assertNotNull(result);
        verify(fhirServiceApiInterface, times(1)).getBpLogList(anyString(), anyString(), eq(requestData));
    }

    @Test
    void testGetBpPatientLogsThrowsDataNotFoundWhenMemberIdNotPresent() {
        RequestDTO requestDTO = new RequestDTO();
        assertThrows(DataNotFoundException.class, () -> bpLogService.getPatientBpLogsWithSymptoms(requestDTO));
    }
}