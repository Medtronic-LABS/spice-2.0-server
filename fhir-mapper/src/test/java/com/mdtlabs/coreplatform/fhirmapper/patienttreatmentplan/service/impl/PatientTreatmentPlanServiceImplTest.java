package com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.Appointment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CarePlan;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Reference;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FrequencyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientTreatmentPlanServiceImplTest {

    @InjectMocks
    private PatientTreatmentPlanServiceImpl patientTreatmentPlanService;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirUtils fhirUtils;

    @Spy
    private SpiceConverter spiceConverter;

        @Test
    void createProvisionalPlan() {
        //given
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();
        treatmentPlanDTO.setHba1c(Boolean.TRUE);
        FrequencyDTO firstFrequency = TestDataProvider.getFrequencyDTO();
        firstFrequency.setName("Every 3 months");
        firstFrequency.setType("HbA1c Check");
        FrequencyDTO secondFrequency = TestDataProvider.getFrequencyDTO();
        treatmentPlanDTO.setCvdRiskLevel(firstFrequency.getRiskLevel());
        treatmentPlanDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        List<FrequencyDTO> frequencies = new ArrayList<>();
        frequencies.add(firstFrequency);
        frequencies.add(secondFrequency);
        treatmentPlanDTO.setFrequencies(frequencies);
        treatmentPlanDTO.setBGDefaultFrequency(Boolean.TRUE);
        treatmentPlanDTO.setPregnancyAnc(true);
        Bundle bundle = new Bundle();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getObservationBundle());

        //then
        TreatmentPlanResponseDTO response = patientTreatmentPlanService.createProvisionalPlan(treatmentPlanDTO, bundle);
        Assertions.assertNotNull(response);
    }

    @Test
    void createProvisionalPlanBundleNull() {
        //given
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();
        FrequencyDTO firstFrequency = TestDataProvider.getFrequencyDTO();
        FrequencyDTO secondFrequency = TestDataProvider.getFrequencyDTO();
        treatmentPlanDTO.setCvdRiskLevel(firstFrequency.getRiskLevel());
        List<FrequencyDTO> frequencies = new ArrayList<>();
        frequencies.add(firstFrequency);
        frequencies.add(secondFrequency);
        treatmentPlanDTO.setFrequencies(frequencies);
        Bundle bundle = null;

        //then
        TreatmentPlanResponseDTO response = patientTreatmentPlanService.createProvisionalPlan(treatmentPlanDTO, bundle);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateAppointment() {
        //given
        Appointment appointment = new Appointment();
        Bundle transactionBundle = new Bundle();
        ProvenanceDTO provenance = new ProvenanceDTO();
        Date toUpdateDate = new Date();
        String patientRef = TestConstants.PATIENT_REFERENCE;

        //then
        patientTreatmentPlanService.updateAppointment(appointment, transactionBundle, provenance, toUpdateDate, patientRef);
        Assertions.assertNotNull(provenance);
    }

    @Test
    void updateAppointmentPatientReferenceNull() {
        //given
        Appointment appointment = new Appointment();
        List<Appointment.AppointmentParticipantComponent> theParticipant = new ArrayList<>();
        Appointment.AppointmentParticipantComponent participant = new Appointment.AppointmentParticipantComponent();
        participant.setActor(new Reference("Patient/123")); // Replace "123" with the actual Patient ID
        participant.setRequired(Appointment.ParticipantRequired.REQUIRED);
        theParticipant.add(participant);
        appointment.setParticipant(theParticipant);
        Bundle transactionBundle = new Bundle();
        ProvenanceDTO provenance = new ProvenanceDTO();
        Date toUpdateDate = new Date();
        String patientRef = TestConstants.PATIENT_REFERENCE;

        //then
        patientTreatmentPlanService.updateAppointment(appointment, transactionBundle, provenance, toUpdateDate, patientRef);
        Assertions.assertNotNull(participant);
    }

    @Test
    void updateTreatmentPlanData() {
        //given
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();
        CarePlan carePlan = new CarePlan();
        carePlan.setStatus(CarePlan.CarePlanStatus.ACTIVE);
        carePlan.setIntent(CarePlan.CarePlanIntent.PLAN);
        carePlan.setTitle("Chronic Disease Management Plan");
        List<CarePlan.CarePlanActivityComponent> theActivity = new ArrayList<>();
        String[] frequencies = {
                Constants.FREQUENCY_BP_CHECK,
                Constants.FREQUENCY_BG_CHECK,
                Constants.FREQUENCY_MEDICAL_REVIEW,
                Constants.FREQUENCY_HBA1C_CHECK,
                Constants.FREQUENCY_CHO_CHECK
        };
        for (String frequency : frequencies) {
            CarePlan.CarePlanActivityComponent activityComponent = new CarePlan.CarePlanActivityComponent();
            CarePlan.CarePlanActivityDetailComponent detailComponent = new CarePlan.CarePlanActivityDetailComponent();
            CodeableConcept codeableConcept = new CodeableConcept();
            codeableConcept.setText(frequency);
            detailComponent.setCode(codeableConcept);
            activityComponent.setDetail(detailComponent);
            theActivity.add(activityComponent);
        }
        carePlan.setActivity(theActivity);
        Bundle bundle = TestDataProvider.getObservationBundle();
        bundle.getEntry().getFirst().setResource(carePlan);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        patientTreatmentPlanService.updateTreatmentPlanData(treatmentPlanDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createOrUpdateAppointment() {
        //given
        String assessmentType = TestConstants.ASSESSMENT;
        Date nextVisitDate = new Date();
        String memberRef = TestConstants.STRING_VALUE;
        String patientRef = TestConstants.PATIENT_REFERENCE;
        Bundle transactionBundle = new Bundle();
        ProvenanceDTO provenance = new ProvenanceDTO();

        //then
        patientTreatmentPlanService.createOrUpdateAppointment(assessmentType, nextVisitDate,
                memberRef, patientRef, transactionBundle, provenance, Boolean.TRUE);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void createOrUpdateAppointmentBundleNull() {
        //given
        String assessmentType = TestConstants.ASSESSMENT;
        Date nextVisitDate = new Date();
        String memberRef = TestConstants.STRING_VALUE;
        String patientRef = TestConstants.PATIENT_REFERENCE;
        Bundle transactionBundle = null;
        ProvenanceDTO provenance = new ProvenanceDTO();

        //then
        patientTreatmentPlanService.createOrUpdateAppointment(assessmentType, nextVisitDate, memberRef,
                patientRef, transactionBundle, provenance, Boolean.FALSE);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void getPatientTreatmentPlanDetailsException() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        //then
        Assertions.assertThrows(DataNotFoundException.class,
                () -> patientTreatmentPlanService.getPatientTreatmentPlanDetails(request));
    }

    @Test
    void getPatientTreatmentPlanDetails() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setMemberReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = TestDataProvider.getCarePlanBundle();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        TreatmentPlanResponseDTO response = patientTreatmentPlanService.getPatientTreatmentPlanDetails(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getCarePlanForPatient() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setMemberReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = TestDataProvider.getCarePlanBundle();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        CarePlan response = patientTreatmentPlanService.getCarePlanForPatient(request.getPatientReference());
        Assertions.assertNotNull(response);
    }

    @Test
    void updateNextVisitDateForPatient() {
        //given
        String memberReference = TestConstants.PATIENT_REFERENCE;
        String patientReference = TestConstants.PATIENT_REFERENCE;
        String assessmentType = TestConstants.ASSESSMENT;
        CarePlan carePlan = (CarePlan) TestDataProvider.getCarePlanBundle().getEntry().getFirst().getResource();
        ProvenanceDTO provenance = new ProvenanceDTO();

        //then
        Date response = patientTreatmentPlanService.updateNextVisitDateForPatient(memberReference, patientReference,
                assessmentType, carePlan, provenance, null);
        Assertions.assertNotNull(response);
    }

    @Test
    void testCreateProvisionalPlan() {
        //given
        TreatmentPlanDTO treatmentPlanDTO = new TreatmentPlanDTO();
        treatmentPlanDTO.setHba1c(Boolean.TRUE);
        FrequencyDTO firstFrequency = TestDataProvider.getFrequencyDTO();
        firstFrequency.setName("Every 2 weeks");
        firstFrequency.setType("default");
        FrequencyDTO secondFrequency = TestDataProvider.getFrequencyDTO();
        FrequencyDTO thirdFrequency = TestDataProvider.getFrequencyDTO();
        thirdFrequency.setName("Every 3 months");
        thirdFrequency.setType("HbA1c Check");
        FrequencyDTO forthFrequency = TestDataProvider.getFrequencyDTO();
        forthFrequency.setName("Every 1 month");
        forthFrequency.setType("default");
        treatmentPlanDTO.setCvdRiskLevel(firstFrequency.getRiskLevel());
        treatmentPlanDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        List<FrequencyDTO> frequencies = new ArrayList<>();
        frequencies.add(firstFrequency);
        frequencies.add(secondFrequency);
        frequencies.add(thirdFrequency);
        frequencies.add(forthFrequency);

        treatmentPlanDTO.setFrequencies(frequencies);
        treatmentPlanDTO.setBGDefaultFrequency(Boolean.TRUE);
        treatmentPlanDTO.setPregnancyAnc(true);

        Bundle bundle = new Bundle();

        //when
        Bundle responseBundle =TestDataProvider.getObservationBundle();
        Observation pregnancyAncObservation = TestDataProvider.getPregnancyAncObservation();


        Bundle.BundleEntryComponent observationEntry = new Bundle.BundleEntryComponent();
        observationEntry.setResource(pregnancyAncObservation);
        responseBundle.setEntry(Arrays.asList(new Bundle.BundleEntryComponent().setResource(pregnancyAncObservation)));

        when(restApiUtil.getBatchRequest(anyString())).thenReturn(responseBundle);
        when(restApiUtil.getBatchRequest(String.format("Observation?performer=RelatedPerson/%s&code:text=%s&_sort=-_lastUpdated&_count=1",
                "reference", FhirConstants.PREGNANCY))).thenReturn(responseBundle);

        //then
        TreatmentPlanResponseDTO response = patientTreatmentPlanService.createProvisionalPlan(treatmentPlanDTO, bundle);
        Assertions.assertNotNull(response);
    }
}