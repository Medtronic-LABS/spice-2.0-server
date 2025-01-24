package com.mdtlabs.coreplatform.fhirmapper.patienttransfer.service.impl;

import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientTransferServiceImplTest {

    @InjectMocks
    private PatientTransferServiceImpl patientTransferService;

    @Mock
    private PatientService patientService;

    @Mock
    private FhirUtils fhirUtils;

    @Mock
    private RestApiUtil restApiUtil;

    @Test
    void validatePatientTransferAndIdNull() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //then
        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.validatePatientTransfer(requestDTO));
    }

    @Test
    void validatePatientTransferAndEntryIsEmpty() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);

        //then
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference())).thenReturn(new Bundle());

        //then
        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.validatePatientTransfer(requestDTO));
    }

    @Test
    void validatePatientTransferAndPatientIsNull() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        Bundle bundle = TestDataProvider.getRelatedPersonBundle();
        bundle.getEntry().getFirst().setResource(null);

        //then
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference())).thenReturn(bundle);

        //then
        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.validatePatientTransfer(requestDTO));
    }

    @Test
    void validatePatientTransfer() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);

        //then
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference())).thenReturn(TestDataProvider.getRelatedPersonBundle());

        //when
        Map<String, String> response = patientTransferService.validatePatientTransfer(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updatePatientRecordsAndIdNull() {
        //given
        RequestDTO requestDTO = new RequestDTO();

        //then
        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientRecords(requestDTO));
    }

    @Test
    void updatePatientRecords() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        requestDTO.setTransferSite("chery hospital");
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference())).thenReturn(TestDataProvider.getRelatedPersonBundle());
        when(restApiUtil.getBatchRequest(anyString())).thenReturn(bundle);

        //then
        patientTransferService.updatePatientRecords(requestDTO);
        verify(restApiUtil, atLeastOnce()).getBatchRequest(anyString());
    }

    @Test
    void updatePatientRecordsThrowDataNotAcceptableException() {
        //given
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);

        //when
        Bundle patientBundle = removePatientLinkFromBundle(TestDataProvider.getRelatedPersonBundle());
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference())).thenReturn(patientBundle);

        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientRecords(requestDTO));
    }

    @Test
    void validatePatientTransferWhenNotEnrolled() {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientReference(TestConstants.PATIENT_REFERENCE);
        RelatedPerson relatedPerson = TestDataProvider.getRelatedPerson();
        Bundle bundle = new Bundle();
        bundle.addEntry()
                .setFullUrl(relatedPerson.getId())
                .setResource(relatedPerson)
                .getRequest()
                .setUrl(String.format(FhirConstants.RELATED_PERSON_ID, relatedPerson.getIdPart()))
                .setMethod(Bundle.HTTPVerb.PUT);
        Bundle patientBundle = changePatientIdentifierFromBundle(TestDataProvider.getRelatedPersonBundle());
        when(patientService.getPatientDetailsByPatientReference(requestDTO.getPatientReference())).thenReturn(patientBundle);
        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.validatePatientTransfer(requestDTO));
    }

    private Bundle changePatientIdentifierFromBundle(Bundle bundle) {
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (entry.getResource() instanceof Patient patient) {
                for (Identifier identifier : patient.getIdentifier()) {
                    identifier.setValue("SCREENING");
                    identifier.setSystem("patient-status");
                }
                entry.setResource(patient);
            }
        }
        return bundle;
    }

    private Bundle removePatientLinkFromBundle(Bundle bundle) {
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (entry.getResource() instanceof Patient patient) {
                patient.getLink().clear();
                entry.setResource(patient);
            }
        }
        return bundle;
    }
}