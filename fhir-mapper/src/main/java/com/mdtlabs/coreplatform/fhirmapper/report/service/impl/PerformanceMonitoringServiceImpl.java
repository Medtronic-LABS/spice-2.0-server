package com.mdtlabs.coreplatform.fhirmapper.report.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.report.service.PerformanceMonitoringService;

/**
 * <p>
 * This service class contains all the business logic for the performance monitoring module and performs
 * all the performance monitoring operations.
 * </p>
 * <p>
 * Created by Nandhakumar Karthikeyan on July 24, 2024
 * </p>
 */
@Service
public class PerformanceMonitoringServiceImpl implements PerformanceMonitoringService {

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    @Autowired
    public PerformanceMonitoringServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
    }

    /**
     * Get performance monitoring report for the given request.
     *
     * @param request - This is the request to get the performance monitoring report detail.
     * @return Map - Report data in the desired format.
     */
    @Override
    public Map<String, Map<String, PerformanceReport>> getChwPerformanceReport(FilterRequestDTO request) {

        Map<String, Map<String, PerformanceReport>> reports = new HashMap<>();
        List<String> villageIdentifiers = request.getVillageIds().stream()
                .map(village -> StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL,
                        Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        requestByUrl(
                String.format(Constants.PERFORMANCE_QUERY_ENCOUNTER, String.join(Constants.COMMA, request.getFhirIds()),
                        String.join(Constants.COMMA, villageIdentifiers)), Constants.ASSESSMENT, reports,
                request.getFromDate(), request.getToDate());
        requestByUrl(String.format(Constants.PERFORMANCE_QUERY_HOUSEHOLD_MEMBER,
                        String.join(Constants.COMMA, request.getFhirIds()), String.join(Constants.COMMA, villageIdentifiers)),
                Constants.HOUSEHOLD_MEMBER, reports, request.getFromDate(), request.getToDate());
        requestByUrl(
                String.format(Constants.PERFORMANCE_QUERY_GROUP, String.join(Constants.COMMA, request.getFhirIds()),
                        String.join(Constants.COMMA, villageIdentifiers)), Constants.HOUSEHOLD, reports,
                request.getFromDate(), request.getToDate());
        requestByUrl(String.format(Constants.PERFORMANCE_QUERY_SERVICE_REQUEST,
                        String.join(Constants.COMMA, request.getFhirIds()), String.join(Constants.COMMA, request.getVillageIds())),
                Constants.PATIENT_STATUS, reports, request.getFromDate(), request.getToDate());
        return reports;
    }

    /**
     * Makes a request to the FHIR server using the provided URL and processes the response.
     *
     * @param url      the URL to request
     * @param type     the type of resource to handle
     * @param reports  the map to store performance reports
     * @param fromDate the start date for filtering
     * @param toDate   the end date for filtering
     */
    private void requestByUrl(String url, String type, Map<String, Map<String, PerformanceReport>> reports,
                              Date fromDate, Date toDate) {
        int skip = Constants.ZERO;
        int limit = Constants.PERFORMANCE_SEARCH_LIMIT_VALUE;
        Bundle bundle;
        if (Objects.nonNull(fromDate) && Objects.nonNull(toDate)) {
            String fromDateString = new SimpleDateFormat(Constants.FHIR_SEARCH_DATE_FORMAT).format(fromDate);
            String toDateString = new SimpleDateFormat(Constants.FHIR_SEARCH_DATE_FORMAT).format(toDate);

            url = StringUtil.concatString(url,
                    String.format(Constants.PERFORMANCE_QUERY_DATE_FILTER, fromDateString, toDateString));
        }
        do {
            bundle = restApiUtil.getBatchRequest(
                    StringUtil.concatString(url, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, limit, skip)));
            skip += limit;
            handleBundle(bundle, type, reports, fromDate, toDate);
        } while (!bundle.getEntry().isEmpty());
    }

    /**
     * Handles the FHIR bundle response based on the resource type.
     *
     * @param bundle   the FHIR bundle response
     * @param type     the type of resource to handle
     * @param reports  the map to store performance reports
     * @param fromDate the start date for filtering
     * @param toDate   the end date for filtering
     */
    public void handleBundle(Bundle bundle, String type, Map<String, Map<String, PerformanceReport>> reports,
                             Date fromDate, Date toDate) {
        switch (type) {
            case Constants.ASSESSMENT -> handleEncounters(bundle, reports);
            case Constants.PATIENT_STATUS -> handleServiceRequests(bundle, reports, fromDate, toDate);
            case Constants.HOUSEHOLD -> handleGroups(bundle, reports);
            case Constants.HOUSEHOLD_MEMBER -> handleMembers(bundle, reports);
            default -> Logger.logInfo("No match found");
        }
    }

    /**
     * Handles the Encounter resources in the FHIR bundle.
     *
     * @param bundle  the FHIR bundle response
     * @param reports the map to store performance reports
     */
    private void handleEncounters(Bundle bundle, Map<String, Map<String, PerformanceReport>> reports) {
        Map<String, String> providenceResourceMap = new HashMap<>();
        constructProvenanceResourceMap(bundle, providenceResourceMap);
        bundle.getEntry().stream()
                .filter(entry -> ResourceType.Encounter.equals(entry.getResource().getResourceType()))
                .map(entry -> (Encounter) entry.getResource())
                .forEach(encounter -> processEncounter(encounter, providenceResourceMap, reports));
    }

    /**
     * Handles the Encounter resources in the FHIR bundle.
     *
     * @param encounter             the Encounter object
     * @param providenceResourceMap the map to store provenance resource IDs
     * @param reports               the map to store performance reports
     */
    private void processEncounter(Encounter encounter, Map<String, String> providenceResourceMap,
                                  Map<String, Map<String, PerformanceReport>> reports) {
        Map<String, String> identifiers = extractIdentifierValue(encounter.getIdentifier());
        String encounterType = identifiers.get(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL);
        String villageId = identifiers.get(FhirIdentifierConstants.VILLAGE_SYSTEM_URL);
        boolean hasFollowUpId = identifiers.containsKey(FhirIdentifierConstants.FOLLOW_UP_ID_SYSTEM_URL);
        boolean hasProvenanceResource = providenceResourceMap.containsKey(encounter.getIdPart());

        if (!Objects.isNull(villageId) && hasFollowUpId && hasProvenanceResource) {
            setValue(reports, villageId, Constants.FOLLOW_UP_COND_VISIT, encounterType,
                    providenceResourceMap.get(encounter.getIdPart()));
        }

        if (!Objects.isNull(villageId) && hasProvenanceResource) {
            setValue(reports, villageId, Constants.ASSESSMENT, encounterType,
                    providenceResourceMap.get(encounter.getIdPart()));
        }
    }

    /**
     * Handles the Group resources in the FHIR bundle.
     *
     * @param bundle  the FHIR bundle response
     * @param reports the map to store performance reports
     */
    private void handleGroups(Bundle bundle, Map<String, Map<String, PerformanceReport>> reports) {
        Map<String, String> providenceResourceMap = new HashMap<>();
        constructProvenanceResourceMap(bundle, providenceResourceMap);
        bundle.getEntry().forEach(entry -> {
            if (ResourceType.Group.equals(entry.getResource().getResourceType())) {
                Group group = (Group) entry.getResource();
                String villageId = extractVillageId(group.getIdentifier());
                if (!Objects.isNull(villageId) && !Objects.isNull(providenceResourceMap.get(group.getIdPart()))) {
                    setValue(reports, villageId, Constants.HOUSEHOLD, null,
                            providenceResourceMap.get(group.getIdPart()));
                }
            }
        });
    }

    /**
     * Handles the RelatedPerson resources in the FHIR bundle.
     *
     * @param bundle  the FHIR bundle response
     * @param reports the map to store performance reports
     */
    private void handleMembers(Bundle bundle, Map<String, Map<String, PerformanceReport>> reports) {
        Map<String, String> providenceResourceMap = new HashMap<>();
        constructProvenanceResourceMap(bundle, providenceResourceMap);
        bundle.getEntry().forEach(entry -> {
            if (ResourceType.RelatedPerson.equals(entry.getResource().getResourceType())) {
                RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
                String villageId = extractVillageId(relatedPerson.getIdentifier());
                if (!Objects.isNull(villageId) &&
                        !Objects.isNull(providenceResourceMap.get(relatedPerson.getIdPart()))) {
                    setValue(reports, villageId, Constants.HOUSEHOLD_MEMBER, null,
                            providenceResourceMap.get(relatedPerson.getIdPart()));
                }
            }
        });
    }

    /**
     * Extracts the village ID from a list of identifiers.
     *
     * @param identifiers the list of identifiers
     * @return the village ID, or null if not found
     */
    private String extractVillageId(List<Identifier> identifiers) {
        return identifiers.stream()
                .filter(identifier -> FhirIdentifierConstants.VILLAGE_SYSTEM_URL.equals(identifier.getSystem()))
                .map(Identifier::getValue).findFirst().orElse(null);
    }

    /**
     * Handles the ServiceRequest resources in the FHIR bundle.
     *
     * @param bundle   the FHIR bundle response
     * @param reports  the map to store performance reports
     * @param fromDate the start date for filtering
     * @param toDate   the end date for filtering
     */
    private void handleServiceRequests(Bundle bundle, Map<String, Map<String, PerformanceReport>> reports,
                                       Date fromDate, Date toDate) {
        Map<String, String> villageMap = new HashMap<>();
        Map<String, String> providenceResourceMap = new HashMap<>();
        List<String> relatedPersonIds = new ArrayList<>();
        Map<String, Map<String, ServiceRequest>> serviceResourceMap = new HashMap<>();
        constructResourceMap(bundle, villageMap, providenceResourceMap, serviceResourceMap, relatedPersonIds);
        serviceResourceMap.keySet().forEach(key -> {
            Set<String> patientStatusSet = new HashSet<>();
            for (ServiceRequest serviceRequest : serviceResourceMap.get(key).values()) {
                String villageId = villageMap.get(key);
                Map<String, String> identifiers = extractIdentifierValue(serviceRequest.getIdentifier());
                String status = identifiers.get(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL);
                String currentStatus = identifiers.get(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL);

                Date lastUpdatedTime = serviceRequest.getMeta().getLastUpdated();
                boolean isCurrentServiceRequest = isInDateRange(lastUpdatedTime, fromDate, toDate);
                boolean containsOnTreatment = status.toLowerCase().contains(Constants.ON_TREATMENT.toLowerCase());
                String value = containsOnTreatment ? Constants.ON_TREATMENT : null;

                if (containsOnTreatment && patientStatusSet.add(value) && !Objects.isNull(
                        providenceResourceMap.get(serviceRequest.getIdPart()))) {
                    setValue(reports, villageId, Constants.PATIENT_STATUS, value,
                            providenceResourceMap.get(serviceRequest.getIdPart()));
                }
                boolean isAutoReferral = serviceRequest.getNote().stream().anyMatch(
                        note -> !Objects.isNull(note.getText()) &&
                                note.getText().toLowerCase().contains(Constants.AUTO_REFERRAL.toLowerCase()));

                boolean isAutoClose = serviceRequest.getNote().stream().anyMatch(
                        note -> !Objects.isNull(note.getText()) &&
                                note.getText().toLowerCase().contains(Constants.AUTO_CLOSE.toLowerCase()));

                if (status.toLowerCase().contains(Constants.REFERRED.toLowerCase()) && !isAutoReferral) {
                    value = Constants.REFERRED;
                } else if (containsOnTreatment &&
                        currentStatus.toLowerCase().contains(Constants.REFERRED.toLowerCase()) &&
                        isCurrentServiceRequest && !isAutoClose) {
                    value = Constants.WORSENED;
                } else if (containsOnTreatment &&
                        currentStatus.toLowerCase().contains(Constants.RECOVERED.toLowerCase()) &&
                        isCurrentServiceRequest && !isAutoClose) {
                    value = Constants.RECOVERED;
                }
                if (!Objects.isNull(value) && patientStatusSet.add(value) && !Objects.isNull(villageId) &&
                        !Objects.isNull(providenceResourceMap.get(serviceRequest.getIdPart()))) {
                    setValue(reports, villageId, Constants.PATIENT_STATUS, value,
                            providenceResourceMap.get(serviceRequest.getIdPart()));
                }
                if (patientStatusSet.size() == Constants.THREE) break;
            }
        });
    }

    /**
     * Checks if the last updated time is within the date range.
     *
     * @param lastUpdatedTime the last updated time
     * @param fromDate        the start date
     * @param toDate          the end date
     * @return true if the last updated time is within the date range, false otherwise
     */
    private boolean isInDateRange(Date lastUpdatedTime, Date fromDate, Date toDate) {
        return (fromDate == null && toDate == null) ||
                (lastUpdatedTime.after(fromDate) && lastUpdatedTime.before(toDate));
    }

    /**
     * Extracts the identifier value from a list of identifiers based on the system URL.
     *
     * @param identifiers the list of identifiers
     * @return the identifier value, or an empty string if not found
     */
    private Map<String, String> extractIdentifierValue(List<Identifier> identifiers) {
        return identifiers.stream()
                .filter(identifier -> identifier.getSystem() != null) // Filter out identifiers with null system
                .collect(Collectors.toMap(Identifier::getSystem,
                        identifier -> identifier.getValue() != null ? identifier.getValue() : Constants.EMPTY_STRING
                        // Handle null values for values
                ));
    }

    /**
     * Sets the value in the performance report map based on the type and value.
     *
     * @param reports   the map to store performance reports
     * @param villageId the village ID
     * @param type      the type of resource
     * @param value     the value to set
     * @param userId    the user ID
     */
    private void setValue(Map<String, Map<String, PerformanceReport>> reports, String villageId, String type,
                          String value, String userId) {
        if (Objects.nonNull(villageId)) {
            reports.computeIfAbsent(villageId, k -> new HashMap<>()).computeIfAbsent(userId, k -> new PerformanceReport());
            PerformanceReport report = reports.get(villageId).get(userId);
            switch (type) {
                case Constants.HOUSEHOLD_MEMBER -> report.incrementHouseholdMember();
                case Constants.PATIENT_STATUS -> report.incrementTicketStatus(value);
                case Constants.ASSESSMENT -> report.incrementEncounterType(value);
                case Constants.HOUSEHOLD -> report.incrementHousehold();
                case Constants.FOLLOW_UP_COND_VISIT -> report.incrementFollowUpCondVisit();
                default -> Logger.logInfo("No match found");
            }
        }
    }

    /**
     * Constructs a resource map from the FHIR bundle.
     *
     * @param bundle                 the FHIR bundle response
     * @param villageMap             the map to store village IDs
     * @param providenceResourceMap  the map to store provenance resource IDs
     * @param serviceRequestByMember the map to store service requests by member
     * @param relatedPersonIds       the list to store related person IDs
     */
    private void constructResourceMap(Bundle bundle, Map<String, String> villageMap,
                                      Map<String, String> providenceResourceMap,
                                      Map<String, Map<String, ServiceRequest>> serviceRequestByMember,
                                      List<String> relatedPersonIds) {
        bundle.getEntry().forEach(entry -> {
            ResourceType resourceType = entry.getResource().getResourceType();
            if (resourceType == ResourceType.Provenance) {
                handleProvenance((Provenance) entry.getResource(), providenceResourceMap);
            } else if (resourceType == ResourceType.ServiceRequest) {
                handleServiceRequest((ServiceRequest) entry.getResource(), serviceRequestByMember, relatedPersonIds);
            }
        });
        setRelatedPerson(relatedPersonIds, villageMap);
    }

    /**
     * set Related person values
     *
     * @param relatedPersonIds ids
     * @param villageMap       village map
     */
    private void setRelatedPerson(List<String> relatedPersonIds, Map<String, String> villageMap) {
        List<String> requestIds = new ArrayList<>();
        int totalCount = relatedPersonIds.size();
        for (String id : relatedPersonIds) {
            requestIds.add(id);
            if (totalCount == requestIds.size() || requestIds.size() == Constants.PERFORMANCE_SEARCH_LIMIT_VALUE) {
                Bundle bundlePersons = restApiUtil.getBatchRequest(
                        String.format(Constants.PERFORMANCE_QUERY_RELATED_PERSON,
                                String.join(Constants.COMMA, requestIds)));
                bundlePersons.getEntry().forEach(person -> {
                    if (person.getResource().getResourceType() == ResourceType.RelatedPerson) {
                        handleRelatedPerson((RelatedPerson) person.getResource(), villageMap);
                    }
                });
                requestIds.clear();
                totalCount = totalCount - Constants.PERFORMANCE_SEARCH_LIMIT_VALUE;
            }
        }
    }

    /**
     * Constructs a provenance resource map from the FHIR bundle.
     *
     * @param bundle                the FHIR bundle response
     * @param providenceResourceMap the map to store provenance resource IDs
     */
    private void constructProvenanceResourceMap(Bundle bundle, Map<String, String> providenceResourceMap) {
        bundle.getEntry().forEach(entry -> {
            ResourceType resourceType = entry.getResource().getResourceType();
            if (resourceType == ResourceType.Provenance) {
                handleProvenance((Provenance) entry.getResource(), providenceResourceMap);
            }
        });
    }

    /**
     * Handles the RelatedPerson resources and stores the village ID in the map.
     *
     * @param relatedPerson the RelatedPerson object
     * @param villageMap    the map to store village IDs
     */
    private void handleRelatedPerson(RelatedPerson relatedPerson, Map<String, String> villageMap) {
        String village = relatedPerson.getIdentifier().stream()
                .filter(identifier -> FhirIdentifierConstants.VILLAGE_SYSTEM_URL.equals(identifier.getSystem()))
                .map(Identifier::getValue).findFirst().orElse(null);

        if (villageMap != null) {
            villageMap.put(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                    relatedPerson.getIdPart()), village);
        }
    }

    /**
     * Handles the Provenance resources and stores the resource ID in the map.
     *
     * @param provenance            the Provenance object
     * @param providenceResourceMap the map to store provenance resource IDs
     */
    private void handleProvenance(Provenance provenance, Map<String, String> providenceResourceMap) {
        if (!provenance.getAgent().isEmpty() && !provenance.getAgent().getFirst().getRole().isEmpty() &&
                Constants.POST.equals(provenance.getAgent().getFirst().getRole().getFirst().getText())) {

            providenceResourceMap.put(provenance.getTarget().getFirst().getReferenceElement().getIdPart(),
                    fhirUtils.getIdFromResourceUrl(provenance.getAgent().getFirst().getWho().getReference()));
        }
    }

    /**
     * Handles the ServiceRequest resources and stores them in the map.
     *
     * @param serviceRequest         the ServiceRequest object
     * @param serviceRequestByMember the map to store service requests by member
     * @param relatedPersonIds       the list to store related person IDs
     */
    private void handleServiceRequest(ServiceRequest serviceRequest,
                                      Map<String, Map<String, ServiceRequest>> serviceRequestByMember,
                                      List<String> relatedPersonIds) {
        String relatedPerson = serviceRequest.getPerformer().stream().map(Reference::getReference)
                .filter(reference -> reference.contains(String.valueOf(ResourceType.RelatedPerson))).findFirst()
                .orElse(null);
        relatedPersonIds.add(fhirUtils.getIdFromResourceUrl(relatedPerson));
        serviceRequestByMember.computeIfAbsent(relatedPerson, k -> new HashMap<>());
        serviceRequestByMember.get(relatedPerson)
                .put(StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH,
                        serviceRequest.getIdPart()), serviceRequest);
    }
}
