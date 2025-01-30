package com.mdtlabs.coreplatform.fhirmapper.converter;

import java.util.ArrayList;
import java.util.List;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthStatus;
import org.hl7.fhir.r4.model.Condition;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdPatientStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientStatusConverterTest {

    @InjectMocks
    private PatientStatusConverter patientStatusConverter;

    @Mock
    private FhirUtils fhirUtils;

    @Test
    void createDiabetesStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        NcdPatientStatus ncdPatientStatus = new NcdPatientStatus();
        ncdPatientStatus.setDiabetesStatus("Known Patient");
        ncdPatientStatus.setDiabetesControlledType("Controlled");
        ncdPatientStatus.setDiabetesYearOfDiagnosis("2020");
        ncdPatientStatus.setDiabetesDiagnosis("Diabetes Mellitus");
        patientStatusDto.setNcdPatientStatus(ncdPatientStatus);
        Condition condition = new Condition();

        //then
        Condition response = patientStatusConverter.createDiabetesStatus(patientStatusDto, condition);
        Assertions.assertNotNull(response);
    }

    @Test
    void createHypertensionStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        NcdPatientStatus ncdPatientStatus = new NcdPatientStatus();
        ncdPatientStatus.setDiabetesStatus(TestConstants.ACTIVE_UPPER);
        ncdPatientStatus.setDiabetesControlledType("Controlled");
        ncdPatientStatus.setDiabetesYearOfDiagnosis("2020");
        ncdPatientStatus.setDiabetesDiagnosis("Diabetes Mellitus");
        ncdPatientStatus.setHypertensionStatus("Hyper Tension");
        patientStatusDto.setNcdPatientStatus(ncdPatientStatus);
        Condition condition = new Condition();

        //then
        Condition response = patientStatusConverter.createHypertensionStatus(patientStatusDto, condition);
        Assertions.assertNotNull(response);
    }

    @Test
    void setReference() {
        //given
        Condition condition = new Condition();
        String patientId = TestDataProvider.getPatient().getId();

        //then
        patientStatusConverter.setReference(condition, patientId, null, null);
        Assertions.assertNotNull(condition.getSubject());
    }

    @Test
    void setReferencePatientIdNull() {
        //given
        Condition condition = new Condition();
        String patientId = null;

        //then
        patientStatusConverter.setReference(condition, patientId, null, null);
        Assertions.assertNotNull(condition.getSubject());
    }

    @Test
    void createConfirmedNCDStatus() {
        //given
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        Condition condition = new Condition();
        patientStatusDto.setNcdPatientStatus(new NcdPatientStatus());
        patientStatusDto.getNcdPatientStatus().setDiabetesDiagnosis("Hypertension");
        patientStatusDto.getNcdPatientStatus().setDiabetesStatus("Known Patient");
        patientStatusDto.getNcdPatientStatus().setHypertensionStatus("Known Patient");

        //then
        Condition response = patientStatusConverter.createConfirmedNCDStatus(patientStatusDto, condition);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateConfirmedDiagnosis() {
        //given
        ConfirmDiagnosisDTO confirmDiagnosisDTO = new ConfirmDiagnosisDTO();
        confirmDiagnosisDTO.setDiagnosisNotes(TestConstants.BLANK_STRING);
        Condition condition = new Condition();
        String type = "Other";
        List<String> diagnoses = new ArrayList<>();
        diagnoses.add("diagnoses");

        //then
        Condition response = patientStatusConverter.updateConfirmedDiagnosis(confirmDiagnosisDTO, condition, type, diagnoses);
        Assertions.assertNotNull(response);
    }

    @Test
    void setMentalHealthStatus(){
        Condition condition = new Condition();
        MentalHealthStatus mentalHealthStatus = new MentalHealthStatus();
        mentalHealthStatus.setStatus("Known Patient");
        String value = Constants.MENTAL_HEALTH_STATUS;
        mentalHealthStatus.setComments(TestConstants.STRING_VALUE);
        mentalHealthStatus.setMentalHealthDisorder(new ArrayList<>());
        patientStatusConverter.setMentalHealthStatus(condition,mentalHealthStatus,value);
        Assertions.assertNotNull(condition.getCategory());
    }

    @Test
    void  createConfirmedMentalHealthStatus() {
        Condition condition = new Condition();
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        MentalHealthStatus mentalHealthStatus = new MentalHealthStatus();
        mentalHealthStatus.setStatus("Known Patient");
        mentalHealthStatus.setMentalHealthDisorder(new ArrayList<>());
        patientStatusDto.setSubstanceUseStatus(mentalHealthStatus);
        patientStatusDto.setMentalHealthStatus(mentalHealthStatus);
        patientStatusConverter.createConfirmedMentalHealthStatus(patientStatusDto, condition);
        Assertions.assertNotNull(condition.getCategory());
    }
}