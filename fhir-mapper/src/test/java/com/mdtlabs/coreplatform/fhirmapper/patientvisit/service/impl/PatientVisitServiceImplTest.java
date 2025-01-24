package com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientVisitServiceImplTest {

    @InjectMocks
    private PatientVisitServiceImpl patientVisitService;

    @Mock
    private EncounterConverter encounterConverter;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirUtils fhirUtils;

    @Test
    void createPatientVisit() {
        //given
        PatientVisitDTO patientVisit = new PatientVisitDTO();

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        Map<String, Object> response = patientVisitService.createPatientVisit(patientVisit);
        Assertions.assertNotNull(response);
    }

    @Test
    void createPatientVisitAndBundleIsEmpty() {
        //given
        PatientVisitDTO patientVisit = new PatientVisitDTO();
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        fhirResponseDTO.setId("12345");
        fhirResponseDTO.setResourceType("Patient");
        List<Object> entryList = new ArrayList<>();
        entryList.add(new Object());
        fhirResponseDTO.setEntry(entryList);
        ResponseEntity<FhirResponseDTO> responseEntity = new ResponseEntity<>(fhirResponseDTO, HttpStatus.OK);
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(TestDataProvider.getEncounterBundle());
        Map<String, List<String>> fhirResponse = new HashMap<>();
        List<String> encounterReferences = new ArrayList<>();
        encounterReferences.add("encounter-id-123");
        fhirResponse.put(String.valueOf(ResourceType.Encounter), encounterReferences);

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(new Bundle());
        when(fhirUtils.getFhirBaseUrl()).thenReturn(TestConstants.URL);
        when(restApiUtil.constructRequestEntity(any())).thenReturn(new HttpEntity<>(bundleDto, new HttpHeaders()));
        when(restApiUtil.postBatchRequest(TestConstants.URL, new HttpEntity<>(bundleDto, new HttpHeaders()))).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(responseEntity.getBody())).thenReturn(fhirResponse);

        //then
        Map<String, Object> response = patientVisitService.createPatientVisit(patientVisit);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientVisitStatus() {
        //given
        String encounterId = TestConstants.STRING_THREE;
        boolean isPrescription = true;
        boolean isMedicalReview = true;
        boolean isInvestication = true;
        String patientReference = TestConstants.PATIENT_REFERENCE;

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        Encounter response = patientVisitService.updatePatientVisitStatus(encounterId, isPrescription, isMedicalReview, isInvestication, patientReference);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientMedicalReviewStatus() {
        //given
        String memberReference = TestConstants.STRING_THREE;

        //when
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(TestDataProvider.getEncounterBundle());

        //then
        boolean response = patientVisitService.getPatientMedicalReviewStatus(memberReference);
        Assertions.assertTrue(response);
    }
}