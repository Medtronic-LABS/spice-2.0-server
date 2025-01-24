package com.mdtlabs.coreplatform.fhirmapper.prescription.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PrescriptionRequestServiceImplTest {

    @InjectMocks
    private PrescriptionRequestServiceImpl prescriptionRequestService;

    @Mock
    private FhirAssessmentMapper fhirAssessmentMapper;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirMapper fhirMapper;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private PatientService patientService;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    private PatientVisitService patientVisitService;

    @Mock
    private CommonConverter commonConverter;

    @Mock
    private PatientConverter patientConverter;

    private static MockedStatic<CommonUtil> commonUtil;

    @Test
    void createMedicationRequest() {
        //given
        String signatureRef = "Observation/123";
        when(fhirAssessmentMapper.createObservation(any(), anyString(), anyString(), any(), any())).thenReturn(signatureRef);
        String uniqueId = "uuid-123";
        when(fhirUtils.getUniqueId()).thenReturn(uniqueId);
        PrescriptionDTO prescription = new PrescriptionDTO();
        EncounterDetailsDTO encounterDetails = new EncounterDetailsDTO();
        encounterDetails.setPatientReference("Patient/456");
        encounterDetails.setMemberId("789");
        encounterDetails.setId("Encounter/101112");
        encounterDetails.setProvenance(TestDataProvider.getProvenance());
        Bundle bundle = new Bundle();
        List<PrescriptionDTO> prescriptionsToSave = List.of(prescription);

        //when
        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());

        //then
        prescriptionRequestService.createMedicationRequest(prescriptionsToSave, TestConstants.PATIENT_REFERENCE, encounterDetails, bundle);
        Mockito.verify(fhirAssessmentMapper).createObservation(encounterDetails, Constants.SIGNATURE, Constants.PRESCRIPTION,
                encounterDetails.getSignature(), bundle);
    }

    @Test
    void getPrescriptions() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setIsActive(Boolean.TRUE);
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = new Bundle();

        MedicationRequest medicationRequest = new MedicationRequest();

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(medicationRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference(),
                Boolean.FALSE.equals(requestDTO.getIsActive()) ? Constants.COMPLETED_SMALLER_CASE : Constants.ACTIVE))).thenReturn(bundle);
        when(fhirMapper.mapPrescriptionDTO(medicationRequest)).thenReturn(prescriptionDTO);

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getPrescriptions(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescribedDetails() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        //then
        Assertions.assertThrows(SpiceValidation.class, () -> prescriptionRequestService.getPrescribedDetails(requestDTO));

        //given
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = new Bundle();

        Identifier identifier = new Identifier();
        identifier.setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
        identifier.setValue(TestConstants.REFERRED);

        Period period = new Period();
        period.setStart(new Date());

        Encounter encounter = new Encounter();
        encounter.setIdentifier(List.of(identifier));
        encounter.setPeriod(period);
        encounter.setId(TestConstants.STRING_THREE);

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(encounter);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);
        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PRESCRIPTION_ENCOUNTER_QUERY, requestDTO.getPatientReference(),
                StringUtil.concatString(FhirIdentifierConstants.PRESCRIPTION_STATUS_SYSTEM_URL, Constants.VERTICAL_BAR, Constants.PRESCRIBED)))).thenReturn(bundle);

        when(restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, TestConstants.STRING_THREE))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference(),
                String.join(Constants.COMMA, List.of(Constants.COMPLETED_SMALLER_CASE, Constants.ACTIVE))))).thenReturn(bundle);
        //then
        PrescriptionHistoryDTO response = prescriptionRequestService.getPrescribedDetails(requestDTO);
        Assertions.assertNotNull(response);

        //given
        requestDTO.setEncounterId(TestConstants.STRING_THREE);
        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_ENCOUNTER_BY_ID_QUERY, requestDTO.getEncounterId()))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.PRESCRIPTION_LIST_PARAMS, requestDTO.getPatientReference(),
                String.join(Constants.COMMA, List.of(Constants.COMPLETED_SMALLER_CASE, Constants.ACTIVE))))).thenReturn(bundle);
        //then
        response = prescriptionRequestService.getPrescribedDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionsByEncounter() {
        //given
        Bundle bundle = new Bundle();

        MedicationRequest medicationRequest = new MedicationRequest();
        medicationRequest.setId(TestConstants.STRING_THREE);
        medicationRequest.setEncounter(new Reference(TestConstants.ENCOUNTER_REFERENCE));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(medicationRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.PRESCRIPTION_LIST_PARAMS, TestConstants.PATIENT_REFERENCE,
                String.join(Constants.COMMA, List.of(Constants.COMPLETED_SMALLER_CASE, Constants.ACTIVE))))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(String.format(Constants.MEDICATION_REQUEST_HISTORY, TestConstants.STRING_THREE))).thenReturn(bundle);

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getPrescriptionsByEncounter(TestConstants.STRING_THREE, TestConstants.PATIENT_REFERENCE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionHistory() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPrescriptionId(TestConstants.STRING_THREE);

        Bundle bundle = new Bundle();

        MedicationRequest medicationRequest = new MedicationRequest();
        medicationRequest.setId(TestConstants.STRING_THREE);
        medicationRequest.setEncounter(new Reference(TestConstants.ENCOUNTER_REFERENCE));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(medicationRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.MEDICATION_REQUEST_HISTORY, requestDTO.getPrescriptionId()))).thenReturn(bundle);
        when(fhirMapper.mapPrescriptionDTO(medicationRequest)).thenReturn(new PrescriptionDTO());

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void removePrescription() {
        //given
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);

        MedicationRequest medicationRequest = new MedicationRequest();
        medicationRequest.setId(TestConstants.STRING_THREE);
        medicationRequest.setEncounter(new Reference(TestConstants.ENCOUNTER_REFERENCE));

        List<Bundle.BundleEntryComponent> entry = new ArrayList<>();
        Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
        bundleEntryComponent.setResource(medicationRequest);
        entry.add(bundleEntryComponent);
        bundle.setEntry(entry);

        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        prescriptionRequestDTO.setPrescriptionId(TestConstants.STRING_THREE);
        prescriptionRequestDTO.setDiscontinuedReason(TestConstants.NO);
        prescriptionRequestDTO.setProvenance(TestDataProvider.getProvenance());

        FhirResponseDTO fhirResponse = TestDataProvider.getFhirResponse(TestConstants.TWO_STR,
                String.valueOf(ResourceType.RelatedPerson),
                prescriptionRequestDTO);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponse, HttpStatus.OK);
        responseEntity.getBody().setId(TestConstants.STRING_THREE);
        String bundleDto = new String();
        HttpHeaders headers = new HttpHeaders();

        ReflectionTestUtils.setField(prescriptionRequestService, "fhirServerUrl", "fhirServerUrl");

        //when
        when(restApiUtil.getBatchRequest(String.format(Constants.GET_PRESCRIPTION_PARAMS, String.join(Constants.COMMA,
                prescriptionRequestDTO.getPrescriptionId())))).thenReturn(bundle);
        doNothing().when(fhirUtils).setBundle(StringUtil.concatString(Constants.FHIR_RESOURCE_MEDICATION_REQUEST, Constants.FORWARD_SLASH, medicationRequest.getIdPart()),
                StringUtil.concatString(Constants.FHIR_BASE_URL, medicationRequest.getIdPart()),
                Bundle.HTTPVerb.PUT, medicationRequest, bundle, prescriptionRequestDTO.getProvenance());
        when(restApiUtil.constructRequestEntity(any(Bundle.class))).thenReturn(new HttpEntity<>(bundleDto, headers));
        when(restApiUtil.postBatchRequest("fhirServerUrl", new HttpEntity<>(bundleDto, headers))).thenReturn(responseEntity);

        //then
        prescriptionRequestService.removePrescription(prescriptionRequestDTO, MedicationRequest.MedicationRequestStatus.COMPLETED);
        Mockito.verify(restApiUtil).getBatchRequest(String.format(Constants.GET_PRESCRIPTION_PARAMS, String.join(Constants.COMMA,
                prescriptionRequestDTO.getPrescriptionId())));
    }

        @Test
    void createNcdMedicationRequest() {
        //given
        String signatureRef = "Observation/123";
        when(fhirAssessmentMapper.createObservation(any(), anyString(), anyString(), any(), any())).thenReturn(signatureRef);
        String uniqueId = "uuid-123";
        when(fhirUtils.getUniqueId()).thenReturn(uniqueId);
        PrescriptionDTO prescription = new PrescriptionDTO();
        EncounterDetailsDTO encounterDetails = new EncounterDetailsDTO();
        encounterDetails.setPatientReference("Patient/456");
        encounterDetails.setMemberId("789");
        encounterDetails.setId("Encounter/101112");
        encounterDetails.setProvenance(TestDataProvider.getProvenance());
        Bundle bundle = new Bundle();
        List<PrescriptionDTO> prescriptionsToSave = List.of(prescription);

        //when
        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());

        //then
        prescriptionRequestService.createNcdMedicationRequest(prescriptionsToSave, encounterDetails, bundle);
        verify(fhirAssessmentMapper, atLeastOnce()).mapMedicationRequest(any(), any());
    }

    @Test
    void createOrUpdateMedicationRequestException() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () ->prescriptionRequestService.createOrUpdateMedicationRequest(prescriptionRequestDTO));
    }

    @Test
    void createOrUpdateMedicationRequest() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        prescriptionDTO.setPrescriptionId(null);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionDTOList.add(prescriptionDTO);
        prescriptionRequestDTO.setPrescriptions(prescriptionDTOList);
        prescriptionRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        //when
        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);

        //then
        Map<String, String> response = prescriptionRequestService.createOrUpdateMedicationRequest(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdPrescribedDetails() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.PATIENT_REFERENCE);
        request.setPatientVisitId(TestConstants.STRING_THREE);
        commonUtil = mockStatic(CommonUtil.class);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        PrescriptionHistoryDTO response = prescriptionRequestService.getNcdPrescribedDetails(request);
        Assertions.assertNotNull(response);
        commonUtil.close();
    }

    @Test
    void getNcdPrescribedDetailsException() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        //then
        Assertions.assertThrows(SpiceValidation.class, () -> prescriptionRequestService.getNcdPrescribedDetails(request));
    }

    @Test
    void getNcdPrescribedDetailsAndPatientVisitIdIsNull() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.PATIENT_REFERENCE);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        PrescriptionHistoryDTO response = prescriptionRequestService.getNcdPrescribedDetails(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdPrescribedDetailsAndEncounterIsNotEmpty() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = TestDataProvider.getEncounterBundle();
        Encounter encounter = new Encounter();
        Identifier validIdentifier = new Identifier();
        validIdentifier.setSystem("encounter-type");
        validIdentifier.setValue("medicalreviewvisit");
        encounter.setIdentifier(List.of(validIdentifier));
        bundle.getEntry().getFirst().setResource(encounter);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        PrescriptionHistoryDTO response = prescriptionRequestService.getNcdPrescribedDetails(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdPrescribedDetailsAndEncounterIsNotEmptyPatientVisitNull() {
        commonUtil = mockStatic(CommonUtil.class);
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientVisitId(null);
        request.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = TestDataProvider.getEncounterBundle();

        Identifier validIdentifier = new Identifier();
        validIdentifier.setSystem("nullencounter-type");
        validIdentifier.setValue("medicalreviewvisit");
        Encounter encounter = (Encounter) bundle.getEntry().getFirst().getResource();
        encounter.setIdentifier(List.of(validIdentifier));


        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);
        when(adminServiceApiInterface.getAllMedicationByIds(any(), any(), any())).thenReturn(TestDataProvider.getMedicationDTOList());

        //then
        PrescriptionHistoryDTO response = prescriptionRequestService.getNcdPrescribedDetails(request);
        Assertions.assertNotNull(response);
        commonUtil.close();
    }

    @Test
    void getNcdPrescriptionHistory() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getMedicationBundle());

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getNcdPrescriptionHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateNcdPrescriptionException() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, ()-> prescriptionRequestService.createOrUpdateNcdPrescription(prescriptionRequestDTO));
    }

    @Test
    void createOrUpdateNcdPrescription() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        List<PrescriptionDTO> prescriptions = TestDataProvider.getPrescriptionDTOList();
        PrescriptionDTO prescriptionDTO2 = new PrescriptionDTO();
        prescriptionDTO2.setPrescriptionId("prescriptionId");
        prescriptionDTO2.setPrescribedDays(1L);
        prescriptions.add(prescriptionDTO2);
        prescriptionRequestDTO.setProvenance(TestDataProvider.getProvenance());
        prescriptionRequestDTO.setPrescriptions(prescriptions);
        prescriptionRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        prescriptionRequestDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        bundle.addEntry(getBundle().getEntry().getFirst());
        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);

        //then
        Map<String, String> response = prescriptionRequestService.createOrUpdateNcdPrescription(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNcdPrescriptions() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Bundle bundle = TestDataProvider.getMedicationBundle();
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        prescriptionDTO.setMedicationId(TestConstants.ONE_STR);
        commonUtil = mockStatic(CommonUtil.class);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirMapper.mapPrescriptionDTO(any())).thenReturn(prescriptionDTO);
        when(adminServiceApiInterface.getAllMedicationByIds(any(), any(), any())).thenReturn(TestDataProvider.getMedicationDTOList());

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getNcdPrescriptions(requestDTO);
        Assertions.assertNotNull(response);
        commonUtil.close();
    }

    @Test
    void listDispensePrescription() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        commonUtil = mockStatic(CommonUtil.class);
        requestDTO.setIsActive(Boolean.TRUE);
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);

        PrescriptionDTO prescriptionDTO = new PrescriptionDTO();
        prescriptionDTO.setPrescriptionId("prescription1");
        prescriptionDTO.setMedicationId("1");
        //when
        Bundle bundle = getBundle();

        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirMapper.mapPrescriptionDTO(any())).thenReturn(prescriptionDTO);
        when(adminServiceApiInterface.getAllMedicationByIds(any(),any(),any())).thenReturn(TestDataProvider.getMedicationDTOList());
        when(fhirMapper.mapPrescriptionDTO(any())).thenReturn(prescriptionDTO);

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.listDispensePrescription(requestDTO);
        Assertions.assertNotNull(response);
        commonUtil.close();
    }

    @Test
    void updateDispensePrescription() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        List<PrescriptionDTO> prescriptions = TestDataProvider.getPrescriptionDTOList();
        PrescriptionDTO prescriptionDTO2 = new PrescriptionDTO();
        prescriptionDTO2.setPrescriptionId("prescriptionId");
        prescriptions.add(prescriptionDTO2);
        ProvenanceDTO provenance = TestDataProvider.getProvenance();
        prescriptionRequestDTO.setProvenance(provenance);
        prescriptionRequestDTO.setPrescriptions(prescriptions);
        EncounterDetailsDTO encounterDTO = TestDataProvider.getEncounterDTO();
        encounterDTO.setProvenance(provenance);
        prescriptionRequestDTO.setEncounter(encounterDTO);
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(getBundle());
        when(fhirAssessmentMapper.updateDispenseRequest(any(),any())).thenReturn(new MedicationRequest());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);

        //then
        Map<String, String> response = prescriptionRequestService.updateDispensePrescription(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateDispensePrescriptionException() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, ()->prescriptionRequestService.updateDispensePrescription(prescriptionRequestDTO));
    }

    @Test
    void getDispenseHistory() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.PATIENT_REFERENCE);
        request.setPatientVisitId(TestConstants.STRING_THREE);
        commonUtil = mockStatic(CommonUtil.class);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getEncounterBundle());
        when(adminServiceApiInterface.getAllMedicationByIds(any(),any(),any())).thenReturn(TestDataProvider.getMedicationDTOList());

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getDispenseHistory(request);
        Assertions.assertNotNull(response);
        commonUtil.close();
    }

    @Test
    void getDispenseHistoryException() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        //then
        Assertions.assertThrows(SpiceValidation.class, ()->prescriptionRequestService.getDispenseHistory(request));
    }

    @Test
    void getPrescriptionCount() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        Bundle bundle = TestDataProvider.getMedicationBundle();

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        //then
        Integer response = prescriptionRequestService.getPrescriptionCount(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPrescriptionsByMemberId() {
        //given
        String memberId = TestConstants.ONE_STR;
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        prescriptionDTO.setMedicationId(TestConstants.ONE_STR);
        commonUtil = mockStatic(CommonUtil.class);

        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(TestDataProvider.getMedicationBundle());
        when(fhirMapper.mapPrescriptionDTO(any())).thenReturn(prescriptionDTO);
        when(adminServiceApiInterface.getAllMedicationByIds(any(),any(),any())).thenReturn(TestDataProvider.getMedicationDTOList());

        //then
        List<PrescriptionDTO> response = prescriptionRequestService.getPrescriptionsByMemberId(memberId);
        Assertions.assertNotNull(response);
        commonUtil.close();
    }

    @Test
    void getPrescriptionHistoryByPrescriptions() {
        //given
        List<String> prescriptionIds = new ArrayList<>();
        prescriptionIds.add(TestConstants.ONE_STR);
        prescriptionIds.add(TestConstants.TWO_STR);
        prescriptionIds.add(TestConstants.STRING_THREE);
        Bundle bundle = TestDataProvider.getObservationBundle();
        bundle.getEntry().getFirst().setResource(TestDataProvider.getMedicationBundle());

        //when
        when(restApiUtil.getBundle(any())).thenReturn(bundle);

        //then
        Map<String, List<PrescriptionDTO>> response = prescriptionRequestService.getPrescriptionHistoryByPrescriptions(prescriptionIds);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateMedicationRequest_prescriptionNotEmpty() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        PrescriptionDTO prescriptionDTO = TestDataProvider.getPrescriptionDTO();
        prescriptionDTO.setPrescriptionId(null);
        List<PrescriptionDTO> prescriptionDTOList = new ArrayList<>();
        prescriptionDTOList.add(prescriptionDTO);
        PrescriptionDTO prescriptionDTO2 = new PrescriptionDTO();
        prescriptionDTO2.setPrescriptionId("prescriptionId");
        prescriptionDTOList.add(prescriptionDTO2);

        prescriptionRequestDTO.setPrescriptions(prescriptionDTOList);
        prescriptionRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        //when
        Bundle bundle = getBundle();
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);

        //then
        Map<String, String> response = prescriptionRequestService.createOrUpdateMedicationRequest(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    private Bundle getBundle() {
        Bundle bundle = new Bundle();
        Bundle.BundleEntryComponent entryComponent = new Bundle.BundleEntryComponent();
        MedicationRequest medicationRequest = new MedicationRequest();
        medicationRequest.setId("prescriptionId");
        MedicationRequest.MedicationRequestDispenseRequestComponent dispenseRequest = new MedicationRequest.MedicationRequestDispenseRequestComponent();
        Period period = new Period();
        period.setStart(new Date());
        period.setEnd(new Date());
        dispenseRequest.setValidityPeriod(period);
        medicationRequest.setDispenseRequest(dispenseRequest);

        entryComponent.setResource(medicationRequest);
        List<Bundle.BundleEntryComponent> entryList = new ArrayList<>();
        entryList.add(entryComponent);
        bundle.setEntry(entryList);
        return bundle;
    }

    @Test
    void createOrUpdateNcdPrescriptionforEncounter() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        List<PrescriptionDTO> prescriptions = TestDataProvider.getPrescriptionDTOList();
        PrescriptionDTO prescriptionDTO2 = new PrescriptionDTO();
        prescriptionDTO2.setPrescriptionId("prescriptionId");
        prescriptionDTO2.setPrescribedDays(1L);
        prescriptions.add(prescriptionDTO2);
        prescriptionRequestDTO.setProvenance(TestDataProvider.getProvenance());
        prescriptionRequestDTO.setPrescriptions(prescriptions);
        prescriptionRequestDTO.setEncounter(TestDataProvider.getEncounterDTO());
        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        relatedPerson.getPatient().setReference(null);

        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        prescriptionRequestDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);

        bundle.addEntry(getBundle().getEntry().getFirst());
        //when
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);

        //then
        Map<String, String> response = prescriptionRequestService.createOrUpdateNcdPrescription(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createOrUpdateNcdPrescriptionNotEncounterPatientReference() {
        //given
        PrescriptionRequestDTO prescriptionRequestDTO = TestDataProvider.getPrescriptionRequestDTO();
        List<PrescriptionDTO> prescriptions = TestDataProvider.getPrescriptionDTOList();
        PrescriptionDTO prescriptionDTO2 = new PrescriptionDTO();
        prescriptionDTO2.setPrescriptionId("prescriptionId");
        prescriptionDTO2.setPrescribedDays(1L);
        prescriptions.add(prescriptionDTO2);
        prescriptionRequestDTO.setProvenance(TestDataProvider.getProvenance());
        prescriptionRequestDTO.setPrescriptions(prescriptions);
        EncounterDetailsDTO encounterDTO = TestDataProvider.getEncounterDTO();
        encounterDTO.setPatientReference(null);
        prescriptionRequestDTO.setEncounter(encounterDTO);

        Bundle bundle = new Bundle();
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        relatedPerson.getPatient().setReference(null);

        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        prescriptionRequestDTO.getEncounter().setProvenance(TestDataProvider.getProvenance());
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        
        //when
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=member001")).thenReturn(bundle);
        ScreeningLogRequestDTO screeningLogRequestDTO = new ScreeningLogRequestDTO();
        screeningLogRequestDTO.setBioMetrics(TestDataProvider.getBioMetricsDTO());
        screeningLogRequestDTO.setBioData(TestDataProvider.getBioData());
        when(commonConverter.setPatientDetails(any(),any())).thenReturn(screeningLogRequestDTO);
        Patient patient = TestDataProvider.getPatient();
        when(patientConverter.createPatient(any(),any(),any(),any())).thenReturn(patient);


        //addEncounter
        when(restApiUtil.getBatchRequest("Encounter?_id=visit123")).thenReturn(getEncounterBundle(new Bundle()));


        //add medication Request
        Bundle medicationRequestBundle = getBundle();
        when(restApiUtil.getBatchRequest("MedicationRequest?_id=prescriptionId,prescriptionid2")).thenReturn(medicationRequestBundle);


        when(fhirAssessmentMapper.mapMedicationRequest(any(),any())).thenReturn(new MedicationRequest());
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(null, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);



        //then
        Map<String, String> response = prescriptionRequestService.createOrUpdateNcdPrescription(prescriptionRequestDTO);
        Assertions.assertNotNull(response);
    }

    private Bundle getEncounterBundle(Bundle bundle) {
        Bundle.BundleEntryComponent entryComponent = new Bundle.BundleEntryComponent();
        Encounter encounter = new Encounter();
        encounter.setId(String.valueOf(123));
        entryComponent.setResource(encounter);
        bundle.addEntry(entryComponent);
        return bundle;

    }
}