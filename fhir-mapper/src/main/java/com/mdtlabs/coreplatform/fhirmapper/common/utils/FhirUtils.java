package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.SimpleRequestHeaderInterceptor;
import ca.uhn.fhir.validation.FhirValidator;
import ca.uhn.fhir.validation.ValidationResult;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import static java.util.Objects.nonNull;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.respository.MetaCodeDetailsRepository;

/**
 * <p>
 * Utility class for FHIR-related operations.
 * Provides methods for interacting with FHIR resources, including creating clients,
 * parsing and validating resources, and managing FHIR bundles.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Feb 09, 2024
 */
@Slf4j
@Component
public class FhirUtils {

    private final MetaCodeDetailsRepository metaCodeDetailsRepository;

    private final RedisTemplate<String, Map<String, MetaCodeDetails>> redisTemplate;

    private Map<String, MetaCodeDetails> codeDetails = new HashMap<>();

    @Getter
    private static String fhirIdentifierUrl;

    /**
     * This setter is used to set job username
     */
    @Value("${app.fhir-identifier-url:}")
    private void setFhirIdentifierUrl(String url) {
        fhirIdentifierUrl = url;
    }

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    public FhirUtils(MetaCodeDetailsRepository metaCodeDetailsRepository,
                     RedisTemplate<String, Map<String, MetaCodeDetails>> redisTemplate) {
        this.metaCodeDetailsRepository = metaCodeDetailsRepository;
        this.redisTemplate = redisTemplate;
    }

    /**
     * Extracts FHIR IDs from a FHIR response DTO.
     * Parses the response to extract resource IDs and categorizes them by resource type.
     *
     * @param fhirResponseDTO The FHIR response DTO containing the response entries.
     * @return A map with resource types as keys and lists of IDs as values.
     */
    public Map<String, List<String>> getFhirIdsFromResponse(FhirResponseDTO fhirResponseDTO) {
        Map<String, List<String>> fhirIds = new HashMap<>();
        fhirResponseDTO.getEntry().forEach(entry -> {
            Map<String, Object> fhirRes = (Map<String, Object>) entry;
            if (!Objects.isNull(fhirRes.get(Constants.RESPONSE))) {
                Map<String, Object> res = (Map<String, Object>) fhirRes.get(Constants.RESPONSE);
                if (!Objects.isNull(res.get(Constants.STATUS)) && res.get(Constants.STATUS).toString().contains(Constants.CREATED)) {
                    String locationString = !Objects.isNull(res.get(Constants.LOCATION)) ? res.get(Constants.LOCATION)
                            .toString() : "";
                    String[] parts = locationString.split(Constants.FORWARD_SLASH);
                    if (Objects.isNull(fhirIds.get(parts[0]))) {
                        fhirIds.put(parts[0], new ArrayList<>());
                    }
                    fhirIds.get(parts[0]).add(parts[1]);
                }
            }
        });
        return fhirIds;
    }

    /**
     * Creates a FHIR client configured with the specified server URL, client value, and token.
     * Registers request interceptors for client identification and authorization.
     *
     * @param url         The FHIR server URL.
     * @param clientValue The client identifier.
     * @param token       The authorization token.
     * @return An instance of IGenericClient configured for the specified server.
     */
    public IGenericClient getClient(String url, String clientValue, String token) {
        FhirContext fhirContext = FhirContext.forR4();
        // Create a FHIR client
        IGenericClient client = fhirContext.newRestfulGenericClient(url);
        client.registerInterceptor(new SimpleRequestHeaderInterceptor(Constants.CLIENT, clientValue));
        client.registerInterceptor(new SimpleRequestHeaderInterceptor(Constants.AUTHORIZATION, token));
        return client;
    }

    /**
     * Validates a FHIR resource using the FHIR validator and returns the validation result.
     * This method is useful for ensuring that a FHIR resource conforms to the FHIR standard
     * and any additional constraints that may be applied.
     *
     * @param resource The FHIR resource to validate.
     * @return A ValidationResult object containing the outcome of the validation.
     */
    public ValidationResult getFhirValidationResult(Resource resource) {
        FhirContext fhirContext = FhirContext.forR4();
        FhirValidator validator = fhirContext.newValidator();
        return validator.validateWithResult(resource);
    }

    /**
     * Sets a bundle with a resource, full URL, HTTP verb, and provenance information.
     * This method is used to add a new entry to a FHIR Bundle with the specified parameters,
     * including creating and adding a Provenance resource based on the operation performed.
     *
     * @param url        The URL of the resource within the FHIR server.
     * @param fullUrl    The full URL of the resource.
     * @param verb       The HTTP verb used for the request (e.g., POST, PUT).
     * @param resource   The FHIR resource to be added to the bundle.
     * @param bundle     The FHIR Bundle object to which the resource will be added.
     * @param provenance The provenance information related to the operation.
     */
    public void setBundle(String url,
                          String fullUrl,
                          Bundle.HTTPVerb verb,
                          Resource resource,
                          Bundle bundle,
                          ProvenanceDTO provenance) {
        if (!Objects.isNull(provenance)) {
            setCreatedBy(url, bundle, provenance, verb, resource.getMeta().getVersionId());
        }
        bundle.addEntry().setFullUrl(fullUrl).setResource(resource).getRequest().setMethod(verb).setUrl(url);
    }

    /**
     * Adds a new entry to a FHIR Bundle using the resource ID. This method creates an entry
     * with the provided FHIR Resource, HTTP verb, and provenance information, and sets the
     * resource-specific and full URLs for the entry.
     *
     * @param id         The ID of the FHIR Resource to be added to the Bundle.
     * @param fullUrl    The full URL of the FHIR Resource.
     * @param verb       The HTTP verb representing the type of operation to be performed on the Resource.
     * @param resource   The FHIR Resource to be added to the Bundle.
     * @param bundle     The FHIR Bundle to which the Resource is to be added.
     * @param provenance The Provenance data to be associated with the operation.
     */
    public void setBundleUsingId(String id,
                                 String fullUrl,
                                 Bundle.HTTPVerb verb,
                                 Resource resource,
                                 Bundle bundle,
                                 ProvenanceDTO provenance) {
        String url = resource.getResourceType().toString().concat(Constants.FORWARD_SLASH).concat(id);
        setCreatedBy(url, bundle, provenance, verb, resource.getMeta().getVersionId());
        bundle.addEntry().setFullUrl(fullUrl).setResource(resource).getRequest().setMethod(verb).setUrl(url);
    }

    /**
     * Adds a resource to a FHIR Bundle with specified URL, full URL, HTTP verb, and resource.
     * This method is used to construct a bundle entry for a given resource, setting the request
     * method and URL for the resource within the bundle.
     *
     * @param url      The URL of the resource within the FHIR server, used in the bundle entry request URL.
     * @param fullUrl  The full URL of the resource, used as the entry's fullUrl property.
     * @param verb     The HTTP verb (e.g., POST, PUT) indicating the intended action for the resource.
     * @param resource The FHIR resource to be added to the bundle.
     * @param bundle   The FHIR Bundle object to which the resource is being added.
     */
    public void setBundle(String url, String fullUrl, Bundle.HTTPVerb verb, Resource resource, Bundle bundle) {
        bundle.addEntry().setFullUrl(fullUrl).setResource(resource).getRequest().setMethod(verb).setUrl(url);
    }

    /**
     * Creates and adds a Provenance resource to a FHIR Bundle.
     * This method constructs a Provenance resource for a given operation, identified by the HTTP verb,
     * and adds it to the specified FHIR Bundle. The Provenance resource includes details such as the
     * operation's target, the acting agent, and the operation's role, based on the provided parameters.
     *
     * @param url               The URL of the resource the Provenance is associated with.
     * @param bundle            The FHIR Bundle object to which the Provenance resource is being added.
     * @param provenanceDetails The details of the provenance, including modified date, organization ID, and user ID.
     * @param verb              The HTTP verb (e.g., POST, PUT) indicating the action performed on the resource.
     */
    public void setCreatedBy(String url, Bundle bundle, ProvenanceDTO provenanceDetails, Bundle.HTTPVerb verb, String versionId) {
        Provenance provenance = new Provenance();
        String uuid = getUniqueId();
        Map<String, MetaCodeDetails> codes = getCodeDetails();
        versionId = Objects.isNull(versionId) ? Constants.ZERO_STRING : versionId;
        List<Coding> coding = new ArrayList<>();
        if (!Objects.isNull(codes.get(verb.toString()))) {
            codes.get(verb.toString()).getCodes().forEach(code -> coding.add(new Coding()
                        .setCode(code.getCode()).setDisplay(code.getDisplay()).setSystem(code.getSystem()))
            );
        }
        provenance.getActivity().setText(versionId);
        provenance.setRecorded(nonNull(provenanceDetails.getModifiedDate()) ? provenanceDetails.getModifiedDate() : new Date());
        provenance.addTarget(new Reference(url));
        provenance.addAgent().setOnBehalfOf(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                        Constants.FORWARD_SLASH, provenanceDetails.getOrganizationId())))
                .setWho(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Practitioner),
                        Constants.FORWARD_SLASH, provenanceDetails.getUserId()))).setRole(List.of(new CodeableConcept().setCoding(coding).setText(verb.toString())));
        bundle.addEntry()
                .setFullUrl(StringUtil.concatString(Constants.FHIR_BASE_URL, uuid))
                .setResource(provenance)
                .getRequest()
                .setMethod(Bundle.HTTPVerb.POST)
                .setUrl(StringUtil.concatString(String.valueOf(ResourceType.Provenance), Constants.FORWARD_SLASH,
                        Constants.FHIR_BASE_URL, uuid));
    }

    /**
     * Extracts the ID from a FHIR history URL.
     * <p>
     * This method parses a FHIR history URL to extract the resource ID. FHIR history URLs typically
     * follow a standard format where the resource ID is followed by the "/_history" segment. This method
     * splits the URL based on this segment and further splits the result to isolate the ID.
     * </p>
     *
     * @param url The full URL of the FHIR resource history.
     * @return The extracted ID as a String.
     */
    public String getIdFromHistoryUrl(String url) {
        String[] paths = url.split(Constants.FHIR_HISTORY_ID_REGEX)[Constants.ZERO].split(Constants.FORWARD_SLASH);
        return paths[paths.length - Constants.ONE];
    }

    /**
     * Extracts the ID from a FHIR reference.
     * <p>
     * This method takes a FHIR reference string and extracts the resource ID. FHIR references are
     * typically URLs or URIs that point to a resource, and the ID is the last segment of the path.
     * This method splits the reference string by "/" and returns the last segment as the ID.
     * </p>
     *
     * @param reference The FHIR reference string.
     * @return The extracted ID as a String.
     */
    public String getIdFromReference(String reference) {
        return Objects.nonNull(reference)
                ? reference.split(Constants.FORWARD_SLASH)[reference.split(Constants.FORWARD_SLASH).length - Constants.ONE]
                : null;
    }

    /**
     * Extracts the ID from a resource URL.
     * <p>
     * Similar to {@link #getIdFromReference(String)}, this method extracts the ID from a resource URL.
     * However, it accounts for URLs that may not follow the standard FHIR reference format and ensures
     * that an ID is returned even if the URL structure is unconventional.
     * </p>
     *
     * @param reference The resource URL.
     * @return The extracted ID as a String, or the original reference if no ID is discernible.
     */
    public String getIdFromResourceUrl(String reference) {
        String[] referencePaths = reference.split(Constants.FORWARD_SLASH);
        return referencePaths.length > 1 ? referencePaths[Constants.ONE] : reference;
    }

    /**
     * Generates a unique identifier using UUID.
     * <p>
     * This method leverages the {@link UUID#randomUUID()} method to generate a universally unique identifier (UUID).
     * The generated UUID is returned as a string and can be used as a unique identifier for resources or entities
     * within the application.
     * </p>
     *
     * @return A string representation of a unique UUID.
     */
    public String getUniqueId() {
        return UUID.randomUUID().toString();
    }

    /**
     * Retrieves all FHIR codes from the repository and organizes them by name.
     * <p>
     * This method fetches all instances of {@link MetaCodeDetails} from the repository and categorizes them into a map
     * where each key is the name of the code, and the value is a list of {@link MetaCodeDetails} associated with that name.
     * This organization facilitates easy retrieval of codes based on their name.
     * </p>
     *
     * @return A map with code names as keys and lists of {@link MetaCodeDetails} as values.
     */
    public Map<String, List<MetaCodeDetails>> getFhirCodes() {
        List<MetaCodeDetails> codes = metaCodeDetailsRepository.findAllByIsActiveTrueAndIsDeletedFalse();
        Map<String, List<MetaCodeDetails>> codesMap = new HashMap<>();
        for (MetaCodeDetails code : codes) {
            if (!codesMap.containsKey(code.getName())) {
                codesMap.put(code.getName(), new ArrayList<>());
            }
            codesMap.get(code.getName()).add(code);
        }
        return codesMap;
    }

    /**
     * Initializes the roles map in Redis with MetaCodeDetails.
     * <p>
     * This method attempts to retrieve a list of {@link MetaCodeDetails} from Redis. If the retrieval is successful but
     * the list is empty, it proceeds to load the data into Redis using {@link #loadRedisData(List)}. The method updates
     * the local {@code codeDetails} map with the first item from the retrieved or loaded list.
     * </p>
     *
     * @return A map of {@link MetaCodeDetails} representing the roles, fetched or loaded into Redis.
     */
    public Map<String, MetaCodeDetails> initiateCodesMap() {
        List<Map<String, MetaCodeDetails>> roleListRedis = new ArrayList<>();
        try {
            roleListRedis = redisTemplate.opsForList()
                    .range(Constants.META_CODE_DETAILS_REDIS_KEY, Constants.ZERO, Constants.ONE);

        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        if (!Objects.isNull(roleListRedis) && roleListRedis.isEmpty()) {
            roleListRedis = loadRedisData(roleListRedis);
        }
        if (!Objects.isNull(roleListRedis) && !roleListRedis.isEmpty()) {
            codeDetails = roleListRedis.getFirst();
            return roleListRedis.getFirst();
        }
        return new HashMap<>();
    }

    /**
     * Loads MetaCodeDetails data into Redis.
     * <p>
     * This method clears the existing list of MetaCodeDetails in Redis, generates a new map of MetaCodeDetails
     * by calling {@link #initiateMap()}, and pushes this new map into Redis. It then retrieves and returns
     * the updated list of MetaCodeDetails from Redis. This ensures that the Redis cache is refreshed with the
     * latest MetaCodeDetails data from the database.
     * </p>
     *
     * @param roleListRedis The current list of MetaCodeDetails in Redis to be refreshed.
     * @return The refreshed list of MetaCodeDetails from Redis.
     */
    private List<Map<String, MetaCodeDetails>> loadRedisData(List<Map<String, MetaCodeDetails>> roleListRedis) {
        try {
            roleListRedis.clear();
            Map<String, MetaCodeDetails> codes = initiateMap();
            redisTemplate.opsForList().leftPush(Constants.META_CODE_DETAILS_REDIS_KEY, codes);
            roleListRedis = redisTemplate.opsForList()
                    .range(Constants.META_CODE_DETAILS_REDIS_KEY, Constants.ZERO, Constants.ONE);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }

        return roleListRedis;
    }

    /**
     * Initializes a map of MetaCodeDetails.
     * <p>
     * Retrieves all MetaCodeDetails from the database and organizes them into a map where each key is the
     * name of the MetaCodeDetails and the value is the corresponding MetaCodeDetails object. This map is
     * used for quick lookup of MetaCodeDetails by name.
     * </p>
     *
     * @return A map of MetaCodeDetails organized by their name.
     */
    public Map<String, MetaCodeDetails> initiateMap() {
        List<MetaCodeDetails> codes = metaCodeDetailsRepository.findAllByIsActiveTrueAndIsDeletedFalse();
        Map<String, MetaCodeDetails> codesMap = new HashMap<>();
        for (MetaCodeDetails code : codes) {
            codesMap.put(code.getName(), code);
        }
        return codesMap;
    }

    /**
     * Retrieves the current map of MetaCodeDetails.
     * <p>
     * This method returns the current map of MetaCodeDetails that has been loaded into memory. If the map
     * has not been initialized yet, it should be initialized first by calling {@link #initiateCodesMap()}.
     * However, this call is commented out, implying that the initialization should be handled externally
     * before calling this method.
     * </p>
     *
     * @return The current map of MetaCodeDetails.
     */
    public Map<String, MetaCodeDetails> getCodeDetails() {
        return codeDetails;
    }

    /**
     * Retrieves the base URL for the FHIR server.
     * <p>
     * This method returns the FHIR server URL configured in the application properties,
     * allowing for dynamic changes to the server's base URL.
     * </p>
     *
     * @return The FHIR server base URL as a String.
     */
    public String getFhirBaseUrl() {
        return fhirServerUrl;
    }

    /**
     * Creates a {@link CodeableConcept} object with {@link Coding} objects based on a specified key.
     * <p>
     * This method retrieves a list of {@link MetaCodeDetails} based on the provided key and constructs a
     * {@link CodeableConcept} object. Each {@link MetaCodeDetails} object is converted into a {@link Coding}
     * object, which is then added to the {@link CodeableConcept}. The method ensures that the {@link CodeableConcept}
     * is populated with all relevant codings, making it ready for use in FHIR resources.
     * </p>
     *
     * @param key The key used to retrieve corresponding {@link MetaCodeDetails} from a preloaded map.
     * @return A {@link CodeableConcept} object populated with {@link Coding} objects.
     */
    public CodeableConcept setCodes(String key) {
        MetaCodeDetails metaCodeDetails = getCodeDetails().get(key);
        CodeableConcept codeableConcept = new CodeableConcept();
        if (!Objects.isNull(metaCodeDetails) && !Objects.isNull(metaCodeDetails.getCodes())) {
            metaCodeDetails.getCodes().forEach(code -> {
                Coding coding = new Coding();
                coding.setSystem(code.getSystem());
                coding.setCode(code.getCode());
                coding.setDisplay(code.getDisplay());
                codeableConcept.addCoding(coding);
            });
        }
        codeableConcept.setText(key);
        return codeableConcept;
    }

    /**
     * Retrieves the display text for a given key from the loaded {@link MetaCodeDetails}.
     * <p>
     * This method looks up {@link MetaCodeDetails} based on the provided key and returns the display text
     * associated with it. If the key does not exist or the {@link MetaCodeDetails} does not have a text value,
     * null is returned. This method is useful for obtaining human-readable text for codes stored in {@link MetaCodeDetails}.
     * </p>
     *
     * @param key The key used to retrieve the display text from {@link MetaCodeDetails}.
     * @return The display text associated with the key, or null if not found.
     */
    public String getText(String key) {
        MetaCodeDetails metaCodeDetails = getCodeDetails().get(key);
        return !Objects.isNull(metaCodeDetails) ? metaCodeDetails.getText() : key;
    }

    /**
     * Generates a string representation of a date in FHIR search format.
     * <p>
     * This method formats a given date string into the FHIR search date format. It supports a special case where
     * if the input string matches a predefined constant for "today", it uses the current date. Otherwise, it adds
     * one day to the current date before formatting. This is useful for generating date strings that comply with
     * FHIR's search requirements.
     * </p>
     *
     * @param date The date string to be formatted. Special values like "today" are handled specifically.
     * @return A string formatted in the FHIR search date format.
     */
    public String getFhirDateString(String date) {
        return new SimpleDateFormat(Constants.FHIR_SEARCH_DATE_FORMAT).format(
                DateUtil.addDate(new Date(), Constants.TODAY.equals(date) ? Constants.ZERO : Constants.ONE));
    }

    /**
     * Converts a FHIR Bundle object to its JSON string representation.
     * <p>
     * This method utilizes the FHIRContext's JSON parser to convert a FHIR Bundle object into a JSON string.
     * This is particularly useful for serializing FHIR Bundle objects to JSON format for storage, transmission,
     * or logging purposes.
     * </p>
     *
     * @param bundle The FHIR Bundle object to be converted to a JSON string.
     * @return The JSON string representation of the FHIR Bundle.
     */
    public String toString(Bundle bundle) {
        FhirContext fhirContext = FhirContext.forR4();
        IParser parser = fhirContext.newJsonParser();
        return parser.encodeResourceToString(bundle);
    }

    /**
     * Creates a CodeableConcept object based on a specified key.
     * <p>
     * This method retrieves MetaCodeDetails based on the provided key and constructs a CodeableConcept object
     * populated with Coding objects derived from those details. It sets the text of the CodeableConcept to the
     * provided key. This method is useful for creating CodeableConcept objects that are ready to be used in FHIR
     * resources.
     * </p>
     *
     * @param mapName The key used to retrieve corresponding MetaCodeDetails and to set as the text of the CodeableConcept.
     * @return A CodeableConcept object populated with Coding objects and text.
     */
    public CodeableConcept createCodeableConcept(String mapName) {
        CodeableConcept codeableConcept = setCodes(mapName);
        codeableConcept.setText(mapName);
        return codeableConcept;
    }

    /**
     * Converts a Boolean pregnancy status into a String representation.
     * <p>
     * This method maps a Boolean value representing pregnancy status to a predefined String value.
     * True is mapped to "Yes", false is mapped to "No", and null or any other value is mapped to "N/A".
     * </p>
     *
     * @param status The pregnancy status as a Boolean value.
     * @return A String representation of the pregnancy status ("Yes", "No", or "N/A").
     */
    public String convertPregnancyType(Boolean status) {
        if (Boolean.TRUE.equals(status)) {
            return Constants.YES;
        }
        if (Boolean.FALSE.equals(status)) {
            return Constants.NO;
        }
        return Constants.NA;
    }

    public Reference getReferenceUrl(ResourceType resourceType, String id) {
        return new Reference(StringUtil.concatString(String.valueOf(resourceType), Constants.FORWARD_SLASH, id));
    }

    /**
     * Gets Oldest Encounter from a bundle.
     * <p>
     * This method retrieves the earliest Encounter resource from the provided bundle by examining the Encounter periods
     * and selecting the one with the earliest start date. It assumes that the bundle contains a collection of Encounter
     * resources and compares each Encounter’s period.start to determine which one occurred first. The method returns
     * this Encounter resource as the earliest documented encounter from the bundle.
     * </p>
     *
     * @param encounterBundle {@link Bundle} - Bundle with encounter details
     * @return The {@link Encounter} is the earliest documented encounter.
     */
    public Encounter getOldestEncounter(Bundle encounterBundle) {
        Optional<Bundle.BundleEntryComponent> earliestEncounter = encounterBundle.getEntry().stream()
                .min(Comparator.comparing(encounter -> ((Encounter) encounter.getResource()).getPeriod().getStart()));
        return earliestEncounter.map(bundleEntryComponent -> (Encounter) bundleEntryComponent.getResource())
                .orElse(null);

    }

    /**
     * Gets a Related person's ID from Encounter resource.
     * <p>
     * This method extracts the ID of a RelatedPerson associated with a specific Encounter resource. It navigates
     * through the Encounter resource’s participant or subject fields, depending on how the RelatedPerson is linked
     * within the Encounter, to locate the reference to the RelatedPerson resource. Once the reference is found, it
     * parses the ID from the reference URL, isolating the unique identifier of the RelatedPerson associated with the
     * Encounter.
     * </p>
     *
     * @param encounter {@link Encounter} - encounter resource
     * @return {@link String} of Releted person ID from Encounter.
     */
    public String getRelatedPersonIdFromEncounter(Encounter encounter) {
        String stringId = encounter.getParticipant().getFirst().getIndividual().getReference();
        return Objects.nonNull(stringId) && !stringId.isBlank() ? getIdFromReference(stringId) : null;
    }
}
