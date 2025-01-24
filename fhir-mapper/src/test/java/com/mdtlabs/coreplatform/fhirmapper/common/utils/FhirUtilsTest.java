package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.validation.ValidationResult;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FhirUtilsTest {

    @InjectMocks
    FhirUtils fhirUtils;

    @Test
    void getFhirIdsFromResponse() {
        FhirResponseDTO fhirResponseDTO = new FhirResponseDTO();
        Map<String, Object> fhirRes = new HashMap<>();
        Map<String, Object> res = new HashMap<>();

        fhirResponseDTO.setEntry(new ArrayList<>());
        Map<String, List<String>> result = fhirUtils.getFhirIdsFromResponse(fhirResponseDTO);
        Assertions.assertNotNull(result);

        fhirResponseDTO.setEntry(List.of(fhirRes));
        Map<String, List<String>> firstResult = fhirUtils.getFhirIdsFromResponse(fhirResponseDTO);
        Assertions.assertNotNull(firstResult);

        res.put(Constants.STATUS, Constants.CREATED);
        res.put(Constants.LOCATION, "one/two");
        fhirRes.put(Constants.RESPONSE, res);
        fhirResponseDTO.setEntry(List.of(fhirRes));
        Map<String, List<String>> secondResult = fhirUtils.getFhirIdsFromResponse(fhirResponseDTO);
        Assertions.assertNotNull(secondResult);
    }

    @Test
    void getClient() {
        IGenericClient result = fhirUtils.getClient("", "", "");
        Assertions.assertNotNull(result);
    }

    @Test
    void getFhirValidationResult() {
        ValidationResult result = fhirUtils.getFhirValidationResult(new Encounter());
        Assertions.assertNotNull(result);
    }

    @Test
    void setBundle() {
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        fhirUtils.setBundle("url", "value", Bundle.HTTPVerb.POST, new Encounter(), new Bundle(), provenanceDTO);
        Assertions.assertNotNull(provenanceDTO);
        fhirUtils.setBundle("url", "value", Bundle.HTTPVerb.POST, new Encounter(), new Bundle());
    }

    @Test
    void setBundleUsingId() {
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        fhirUtils.setBundleUsingId("url", "value", Bundle.HTTPVerb.POST, new Encounter(), new Bundle(), provenanceDTO);
        Assertions.assertNotNull(provenanceDTO);
    }

}