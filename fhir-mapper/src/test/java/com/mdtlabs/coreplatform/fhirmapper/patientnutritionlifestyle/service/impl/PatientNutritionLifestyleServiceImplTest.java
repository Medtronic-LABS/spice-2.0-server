package com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.Bundle;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

/**
 * <p>
 *     Test class for {@link PatientNutritionLifestyleServiceImpl}
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientNutritionLifestyleServiceImplTest {

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private UserService userService;

    @InjectMocks
    private PatientNutritionLifestyleServiceImpl patientNutritionLifestyleService;

    @Mock
    private PatientService patientService;

    @Test
    void addPatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = TestDataProvider.getPatientNutritionLifestyle();
        ResponseEntity<FhirResponseDTO> responseEntity = ResponseEntity.ok(new FhirResponseDTO());
        when(fhirUtils.getUniqueId()).thenReturn("unique-id");
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);
        when(fhirUtils.getFhirIdsFromResponse(any())).thenReturn(Collections.singletonMap("Observation", Collections.singletonList("obs-id")));

        PatientNutritionLifestyle result = patientNutritionLifestyleService.addPatientNutritionLifestyle(patientNutritionLifestyle);

        assertEquals("obs-id", result.getId());
    }

    @Test
    void getPatientNutritionLifeStyleList() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference("patient-ref");
        Bundle bundle = new Bundle();
        bundle.setEntry(new ArrayList<>());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        List<PatientNutritionLifestyle> result = patientNutritionLifestyleService.getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(0, result.size());
    }

    @Test
    void getPatientNutritionLifeStyleListIsNutritionist() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference("patient-ref");
        requestDTO.setNutritionist(true);
        Bundle bundle = new Bundle();
        bundle.setEntry(new ArrayList<>());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        List<PatientNutritionLifestyle> result = patientNutritionLifestyleService.getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(0, result.size());
    }

    @Test
    void getPatientNutritionLifeStyleListIsNutritionHistoryRequired() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference("patient-ref");
        requestDTO.setNutritionHistoryRequired(true);
        Bundle bundle = TestDataProvider.getObservationBundle();
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        List<PatientNutritionLifestyle> result = patientNutritionLifestyleService.getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(1, result.size());
    }

    @Test
    void getPatientNutritionLifeStyleList_nullPatientReference() {
        RequestDTO requestDTO = new RequestDTO();

        assertThrows(DataNotFoundException.class, () -> patientNutritionLifestyleService.getPatientNutritionLifeStyleList(requestDTO));
    }

    @Test
    void getNotViewedLifestyleCount() {
        Bundle bundle = new Bundle();
        bundle.setEntry(new ArrayList<>());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);

        int result = patientNutritionLifestyleService.getNotViewedLifestyleCount(1L);

        assertEquals(0, result);
    }

    @Test
    void getNotViewedLifestyleCount_nullPatientId() {
        assertThrows(DataNotFoundException.class, () -> patientNutritionLifestyleService.getNotViewedLifestyleCount(null));
    }

    @Test
    void updatePatientNutritionLifestyle() {
        PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles = new PatientNutritionLifestyleUpdateDTO();
        patientNutritionLifestyles.setLifestyles(new ArrayList<>());
        PatientNutritionLifestyle patientNutritionLifestyle = TestDataProvider.getPatientNutritionLifestyle();
        patientNutritionLifestyle.setId("obs-id");
        patientNutritionLifestyles.getLifestyles().add(patientNutritionLifestyle);
        Bundle bundle = new Bundle();
        bundle.setEntry(new ArrayList<>());
        ResponseEntity<FhirResponseDTO> responseEntity = ResponseEntity.ok(new FhirResponseDTO());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        PatientNutritionLifestyleUpdateDTO result = patientNutritionLifestyleService.updatePatientNutritionLifestyle(patientNutritionLifestyles);

        assertEquals("obs-id", result.getLifestyles().getFirst().getId());
    }

    @Test
    void updatePatientNutritionLifestyleNonNull() {
        PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles = new PatientNutritionLifestyleUpdateDTO();
        patientNutritionLifestyles.setIsNutritionist(true);
        patientNutritionLifestyles.setProvenance(new ProvenanceDTO());
        patientNutritionLifestyles.setAssessedBy(TestConstants.BLANK_STRING);
        patientNutritionLifestyles.setReferredBy(TestConstants.BLANK_STRING);
        patientNutritionLifestyles.setReferredDate(new Date());
        patientNutritionLifestyles.setAssessedDate(new Date());
        patientNutritionLifestyles.setMemberReference(TestConstants.BLANK_STRING);
        patientNutritionLifestyles.setPatientReference(TestConstants.BLANK_STRING);
        patientNutritionLifestyles.setLifestyles(new ArrayList<>());
        PatientNutritionLifestyle patientNutritionLifestyle = TestDataProvider.getPatientNutritionLifestyle();
        patientNutritionLifestyle.setId("obs-id");
        patientNutritionLifestyles.getLifestyles().add(patientNutritionLifestyle);
        Bundle bundle = TestDataProvider.getObservationBundle();
        ResponseEntity<FhirResponseDTO> responseEntity = ResponseEntity.ok(new FhirResponseDTO());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        PatientNutritionLifestyleUpdateDTO result = patientNutritionLifestyleService.updatePatientNutritionLifestyle(patientNutritionLifestyles);

        assertEquals("obs-id", result.getLifestyles().getFirst().getId());
    }

    @Test
    void updatePatientNutritionLifestyle_nullId() {
        PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles = new PatientNutritionLifestyleUpdateDTO();
        patientNutritionLifestyles.setLifestyles(new ArrayList<>());
        PatientNutritionLifestyle patientNutritionLifestyle = TestDataProvider.getPatientNutritionLifestyle();
        patientNutritionLifestyle.setId(null);
        patientNutritionLifestyles.getLifestyles().add(patientNutritionLifestyle);
        assertThrows(DataNotFoundException.class, () -> patientNutritionLifestyleService.updatePatientNutritionLifestyle(patientNutritionLifestyles));
    }

    @Test
    void removePatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = TestDataProvider.getPatientNutritionLifestyle();
        patientNutritionLifestyle.setId("obs-id");
        Bundle bundle = new Bundle();
        bundle.setEntry(new ArrayList<>());
        ResponseEntity<FhirResponseDTO> responseEntity = ResponseEntity.ok(new FhirResponseDTO());
        when(restApiUtil.getBatchRequest(any())).thenReturn(bundle);
        when(restApiUtil.postBatchRequest(any(), any())).thenReturn(responseEntity);

        PatientNutritionLifestyle result = patientNutritionLifestyleService.removePatientNutritionLifestyle(patientNutritionLifestyle);
        Assertions.assertNotNull(result);
    }

    @Test
    void removePatientNutritionLifestyle_nullId() {
        PatientNutritionLifestyle patientNutritionLifestyle = TestDataProvider.getPatientNutritionLifestyle();
        patientNutritionLifestyle.setId(null);

        assertThrows(DataNotFoundException.class, () -> patientNutritionLifestyleService.removePatientNutritionLifestyle(patientNutritionLifestyle));
    }

}