package com.mdtlabs.coreplatform.spiceservice.glucoselog.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.PatientTreatmentPlanService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.SymptomRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class GlucoseLogServiceImplTest {

    @Mock
    private PatientTreatmentPlanService patientTreatmentPlanService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private FollowUpService followUpService;

    @InjectMocks
    private GlucoseLogServiceImpl glucoseLogService;

    @Mock
    private SymptomRepository symptomRepository;


    @BeforeEach
    public void setup() {
        TestDataProvider.init();
    }

    @AfterEach
    public void close() {
        TestDataProvider.cleanUp();
    }

    @Test
    void testAddGlucoseLog_ValidInput_Success() {
        GlucoseLogDTO glucoseLogDTO = new GlucoseLogDTO();
        AssessmentDTO patientAssessmentDTO = new AssessmentDTO();
        patientAssessmentDTO.setNextBgAssessmentDate(new Date());
        CallRegister callRegister = TestDataProvider.getCallRegister();
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();
        callRegister.setMemberId(assessmentDTO.getMemberReference());
        callRegister.setPatientId(Objects.nonNull(assessmentDTO.getPatientId())
                ? assessmentDTO.getPatientId() : assessmentDTO.getPatientReference());
        callRegister.setIsInitiated(Boolean.FALSE);
        callRegister.setType(AppointmentType.ASSESSMENT);
        callRegister.setIsWrongNumber(Boolean.FALSE);
        callRegister.setVillageId(assessmentDTO.getVillageId());
        callRegister.setNextBGAssessmentTime(assessmentDTO.getNextBgAssessmentDate());
        callRegister.setReferredSiteId(assessmentDTO.getAssessmentOrganizationId());
        callRegister.setPatientStatus("OnTreatment");
        callRegister.setReason("Malaria");
        callRegister.setHouseholdId("1");
        callRegister.setMemberId(assessmentDTO.getMemberReference());
        callRegister.setEncounterType("ICCM");


        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.assessmentCreate(anyString(), anyString(), any(AssessmentDTO.class))).thenReturn(patientAssessmentDTO);
        //then
        GlucoseLogDTO result = glucoseLogService.addGlucoseLog(glucoseLogDTO);
        assertNotNull(result);
    }

    @Test
    void testGetPatientGlucoseLogsWithSymptoms_ValidRequest_Success() {
        TestDataProvider.getStaticMock();
        RequestDTO requestData = new RequestDTO();
        requestData.setMemberId("12345");

        PatientGlucoseLogDTO patientGlucoseLogDTO = new PatientGlucoseLogDTO();
        patientGlucoseLogDTO.setLatestGlucoseLog(new GlucoseLogDTO());
        patientGlucoseLogDTO.getLatestGlucoseLog().setSymptoms(List.of(TestConstants.SYMPTOMS));
        List<Symptom> bgSymptoms = List.of(TestDataProvider.getSymptom());
        when(fhirServiceApiInterface.getGlucoseList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestData))
                .thenReturn(patientGlucoseLogDTO);
        when(symptomRepository.findByNameInAndType(patientGlucoseLogDTO.getLatestGlucoseLog().getSymptoms(), Constants.DIABETES)).thenReturn(bgSymptoms);
        PatientGlucoseLogDTO result = glucoseLogService.getPatientGlucoseLogsWithSymptoms(requestData);

        assertNotNull(result);
        verify(fhirServiceApiInterface, times(1)).getGlucoseList(anyString(), anyString(), any(RequestDTO.class));
    }

    @Test
    void testGetPatientGlucoseLogsWithSymptoms_MissingMemberId_ThrowsDataNotFoundException() {
        RequestDTO requestData = new RequestDTO();

        assertThrows(DataNotFoundException.class, () -> glucoseLogService.getPatientGlucoseLogsWithSymptoms(requestData));
    }

}