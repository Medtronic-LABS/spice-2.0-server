package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import org.hl7.fhir.r4.model.Bundle;
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
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;

/**
 * <p>
 * RestApiUtilTest class used to test all possible positive
 * and negative cases for all methods and conditions used in HouseholdController class.
 * </p>
 *
 * @author Nandhakumar
 * @since Feb 8, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RestApiUtilTest {

    @Mock
    private RestTemplate restTemplate;

    @InjectMocks
    private RestApiUtil restApiUtil;

    @Test
    void postBatchRequest() {
        ReflectionTestUtils.setField(restApiUtil, TestConstants.FHIR_SERVER_URL, TestConstants.FHIR_SERVER_URL);
        HttpHeaders headers = new HttpHeaders();
        HttpEntity requestEntity = new HttpEntity<>(headers);
        when(restTemplate.postForEntity(TestConstants.FHIR_SERVER_URL, requestEntity, FhirResponseDTO.class))
                .thenReturn(new ResponseEntity<FhirResponseDTO>(HttpStatus.OK));
        ResponseEntity<FhirResponseDTO> response = restApiUtil.postBatchRequest(TestConstants.FHIR_SERVER_URL, requestEntity);
        assertNotNull(response);
    }

    @Test
    void constructRequestEntity() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        HttpEntity requestEntity = restApiUtil.constructRequestEntity(new Bundle());
        TestDataProvider.cleanUp();
        assertNotNull(requestEntity);
    }

}
