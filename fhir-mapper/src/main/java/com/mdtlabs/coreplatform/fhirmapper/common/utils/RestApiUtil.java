package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;

/**
 * Utility class for interacting with external REST APIs, specifically for FHIR server communications.
 * <p>
 * This class provides methods to send batch requests to a FHIR server and to construct HTTP entities
 * for these requests. It uses Spring's {@link RestTemplate} for HTTP operations and HAPI FHIR's {@link FhirContext}
 * for FHIR resource parsing.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Feb 05, 2024
 */
@Component
public class RestApiUtil {

    private final RestTemplate restTemplate;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    public RestApiUtil(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    /**
     * Sends a POST request to the specified URL with the given request entity and returns the response.
     * <p>
     * This method is used to send batch requests to the FHIR server. It expects the URL of the FHIR server
     * endpoint and an {@link HttpEntity} containing the request details. The response is wrapped in a
     * {@link ResponseEntity} with a generic type of {@link FhirResponseDTO}.
     * </p>
     *
     * @param url           The URL of the FHIR server endpoint.
     * @param requestEntity The request details wrapped in an {@link HttpEntity}.
     * @return A {@link ResponseEntity} containing the response from the FHIR server.
     */
    public ResponseEntity<FhirResponseDTO> postBatchRequest(String url, HttpEntity requestEntity) {
        return restTemplate.postForEntity(url, requestEntity, FhirResponseDTO.class);
    }

    /**
     * Sends a GET request to the specified URL and returns a {@link Bundle} object containing the response.
     * <p>
     * This method constructs a GET request to the FHIR server, appending the provided URL to the base FHIR server URL.
     * It uses {@link StringUtil} to concatenate the URLs. The response is parsed into a {@link Bundle} object using
     * HAPI FHIR's context and parser.
     * </p>
     *
     * @param url The URL of the FHIR server endpoint, appended to the base FHIR server URL.
     * @return A {@link Bundle} object containing the response from the FHIR server.
     */
    public Bundle getBatchRequest(String url) {
        HttpEntity<String> res = restTemplate.exchange(StringUtil.concatString(fhirServerUrl, url), HttpMethod.GET, constructRequestEntityWithoutBundle(),
                String.class);
        FhirContext ctx = FhirContext.forR4();
        // Parse the JSON string
        return ctx.newJsonParser().parseResource(Bundle.class, res.getBody());
    }

    /**
     * Constructs an {@link HttpEntity} without a body, containing only headers necessary for the request.
     * <p>
     * This method prepares an {@link HttpEntity} with headers including client identification and authorization token,
     * both retrieved from {@link CommonUtil}, and sets the content type to JSON. This entity can be used for GET requests
     * where a request body is not required.
     * </p>
     *
     * @return An {@link HttpEntity} with necessary headers and no body.
     */
    public HttpEntity constructRequestEntityWithoutBundle() {
        HttpHeaders headers = new HttpHeaders();
        headers.set(Constants.CLIENT, CommonUtil.getClient());
        headers.set(Constants.AUTHORIZATION, CommonUtil.getAuthToken());
        headers.setContentType(MediaType.APPLICATION_JSON);
        return new HttpEntity<>(headers);
    }

    /**
     * Constructs an {@link HttpEntity} with a FHIR {@link Bundle} as the body and necessary headers.
     * <p>
     * This method serializes a FHIR {@link Bundle} into a JSON string using HAPI FHIR's {@link IParser}.
     * It then constructs an {@link HttpEntity} object containing this JSON string as the body and sets
     * the necessary HTTP headers, including client identification and authorization token (retrieved from
     * {@link CommonUtil}), and sets the content type to JSON. This {@link HttpEntity} is suitable for
     * sending FHIR resources to a server via POST requests.
     * </p>
     *
     * @param bundle The FHIR {@link Bundle} to be serialized and sent in the request body.
     * @return An {@link HttpEntity} containing the serialized FHIR {@link Bundle} and necessary headers.
     */
    public HttpEntity constructRequestEntity(Bundle bundle) {
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        String bundleDto = parser.encodeResourceToString(bundle);
        HttpHeaders headers = new HttpHeaders();
        headers.set(Constants.CLIENT, CommonUtil.getClient());
        headers.set(Constants.AUTHORIZATION, CommonUtil.getAuthToken());
        headers.setContentType(MediaType.APPLICATION_JSON);
        return new HttpEntity<>(bundleDto, headers);
    }

    public void delete() {
        HttpHeaders headers = new HttpHeaders();
        headers.set(Constants.CLIENT, CommonUtil.getClient());
        headers.set(Constants.AUTHORIZATION, CommonUtil.getAuthToken());
        headers.setContentType(MediaType.APPLICATION_JSON);
    }

    public Patient getPatientById(String url) {
        // Create HttpHeaders and set the required headers
        HttpHeaders headers = new HttpHeaders();
        headers.set(Constants.CLIENT, CommonUtil.getClient());
        headers.set(Constants.AUTHORIZATION, CommonUtil.getAuthToken());
        headers.setContentType(MediaType.APPLICATION_JSON);

        // Create an HttpEntity object with the headers
        HttpEntity<Object> entity = new HttpEntity<>(headers);

        // Use the exchange method to send the GET request with headers
        ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.GET, entity, String.class);

        // Return the body of the response
        FhirContext ctx = FhirContext.forR4();
        // Parse the JSON string
        return Objects.isNull(response.getBody()) ?
                null : ctx.newJsonParser().parseResource(Patient.class, response.getBody());
    }

    /**
     * Get the encounter by encounter id from fhir server
     *
     * @param id - the unique id of the encounter.
     * @return An {@link Encounter} the encounter of the incoming id
     */
    public Encounter getEncounterById(String id) {
        // Create HttpHeaders and set the required headers
        HttpHeaders headers = new HttpHeaders();
        headers.set(Constants.CLIENT, CommonUtil.getClient());
        headers.set(Constants.AUTHORIZATION, CommonUtil.getAuthToken());
        headers.setContentType(MediaType.APPLICATION_JSON);

        // Create an HttpEntity object with the headers
        HttpEntity<Object> entity = new HttpEntity<>(headers);

        // Use the exchange method to send the GET request with headers
        ResponseEntity<String> response = restTemplate.exchange(StringUtil.concatString(fhirServerUrl, "Encounter/", id), HttpMethod.GET, entity, String.class);

        // Return the body of the response
        FhirContext ctx = FhirContext.forR4();
        // Parse the JSON string
        return Objects.isNull(response.getBody()) ?
                null : ctx.newJsonParser().parseResource(Encounter.class, response.getBody());
    }

    /**
     * Get the observation by observation id from fhir server
     *
     * @param id - the unique id of the observation.
     * @return An {@link org.hl7.fhir.r4.model.Observation} the observation of the incoming id
     */
    public Observation getObservationById(String id) {
        // Create HttpHeaders and set the required headers
        HttpHeaders headers = new HttpHeaders();
        headers.set(Constants.CLIENT, CommonUtil.getClient());
        headers.set(Constants.AUTHORIZATION, CommonUtil.getAuthToken());
        headers.setContentType(MediaType.APPLICATION_JSON);

        // Create an HttpEntity object with the headers
        HttpEntity<Object> entity = new HttpEntity<>(headers);

        // Use the exchange method to send the GET request with headers
        ResponseEntity<String> response = restTemplate.exchange(StringUtil.concatString(fhirServerUrl, "Observation/", id), HttpMethod.GET, entity, String.class);

        // Return the body of the response
        FhirContext ctx = FhirContext.forR4();
        // Parse the JSON string
        return Objects.isNull(response.getBody()) ?
                null : ctx.newJsonParser().parseResource(Observation.class, response.getBody());
    }

    /**
     * Get bundle from the fhir server based on request bundle
     *
     * @param requestBundle The request FHIR Bundle entity
     * @return response FHIR Bundle entity
     */
    public Bundle getBundle(Bundle requestBundle) {
        HttpEntity<Bundle> entity = constructRequestEntity(requestBundle);
        ResponseEntity<String> response = restTemplate.exchange(
                fhirServerUrl,
                HttpMethod.POST,
                entity,
                String.class
        );
        // Return the body of the response
        FhirContext ctx = FhirContext.forR4();
        // Parse the JSON string
        return Objects.isNull(response.getBody()) ?
                null : ctx.newJsonParser().parseResource(Bundle.class, response.getBody());
    }

    /**
     * <p>
     * Get Site related Organization details of a Patient.
     * </p>
     *
     * @param memberId {@link String} - Patient Member ID
     * @return A {@link Map} containing patient details and its correspondent organization details.
     */
    public Map<String, Object> getSiteDetails(String memberId) {
        Map<String, Object> response = new HashMap<>();
        if (Objects.nonNull(memberId)) {
            String url = String.format(Constants.GET_PATIENT_BY_PATIENT_REFERENCE, memberId)
                    .concat(Constants.INCLUDE_PATIENT_ORGANIZATION).concat(Constants.PATIENT_ACTIVE_STATUS);
            Bundle bundle = getBatchRequest(url);
            Patient patient = null;
            Organization organization = null;
            if (!bundle.getEntry().isEmpty()) {
                for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                    if (entry.getResource().getResourceType().equals(ResourceType.Organization)) {
                        organization = (Organization) entry.getResource();
                    }
                    if (entry.getResource().getResourceType().equals(ResourceType.Patient)) {
                        patient = (Patient) entry.getResource();
                    }
                }
            }
            response.put(FhirConstants.PATIENT, patient);
            response.put(FhirConstants.ORGANIZATION, organization);
        }
        return response;
    }

    /**
     * <p>
     * Get Related person details based on the given URL.
     * </p>
     *
     * @param url - A {@link String} variable with FHIR url.
     * @return {@link RelatedPerson} Retrieved Related person details.
     */
    public RelatedPerson getRelatedPerson(String url) {
        RelatedPerson relatedPersonbyId = null;
        Bundle bundle = getBatchRequest(url);
        if (Objects.nonNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
            relatedPersonbyId = (RelatedPerson) bundle.getEntry().getFirst().getResource();
        }
        return relatedPersonbyId;
    }
}
