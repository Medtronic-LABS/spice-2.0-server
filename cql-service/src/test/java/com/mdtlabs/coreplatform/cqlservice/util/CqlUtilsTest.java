package com.mdtlabs.coreplatform.cqlservice.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = org.mockito.quality.Strictness.LENIENT)
class CqlUtilsTest {

    @Test
    void getFhirContextReturnsExpectedResult() {
        //given
        FhirContext expectedFhirContext = FhirContext.forCached(FhirVersionEnum.R4);

        //then
        FhirContext actualResponse = CqlUtils.getFhirContext();
        assertEquals(expectedFhirContext.getVersion().getVersion(), actualResponse.getVersion().getVersion());
        assertNotNull(actualResponse);
    }

    @Test
    void getFhirClientReturnsExpectedResult() {
        //then
        IGenericClient actualResponse = CqlUtils.getFhirClient(TestConstants.URL, TestConstants.CLIENT_VALUE,
                TestConstants.TOKEN);
        assertNotNull(actualResponse);
        assertNotNull(actualResponse.getHttpClient());
    }

    @Test
    void getIdFromHistoryUrlReturnsExpectedResult() {
        //given
        String url = TestConstants.HISTORY_URL;
        String expectedResponse = TestConstants.ONE;

        //then
        String actualResponse = CqlUtils.getIdFromHistoryUrl(url);
        assertEquals(expectedResponse, actualResponse);
    }

    @Test
    void getIdFromReferenceReturnsExpectedResult() {
        //given
        String url = TestConstants.REFERENCE_URL;
        String expectedResponse = TestConstants.ONE;

        //then
        String actualResponse = CqlUtils.getIdFromReference(url);
        assertEquals(expectedResponse, actualResponse);
    }

}