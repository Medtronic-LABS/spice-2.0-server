package com.mdtlabs.coreplatform.fhirmapper.report.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Group;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PerformanceMonitoringServiceImplTest {

    @InjectMocks
    private PerformanceMonitoringServiceImpl performanceMonitoringService;

    @Mock
    private RestApiUtil restApiUtil;

    @Mock
    private FhirUtils fhirUtils;

    @Test
    void testGetPerformanceMonitoringReport() {
        FilterRequestDTO filterRequestDTO = new FilterRequestDTO();
        filterRequestDTO.setVillageIds(Set.of("village1"));
        filterRequestDTO.setFhirIds(Set.of("village1"));
        filterRequestDTO.setUserIds(Set.of(1l, 2l));

        List<Identifier> identifiers = new ArrayList<>();
        List<String> villageIdentifiers = filterRequestDTO.getVillageIds().stream()
                .map(village -> StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL,
                        Constants.VERTICAL_BAR, String.valueOf(village))).toList();

        String urlHouseHoldMember = String.format(Constants.PERFORMANCE_QUERY_HOUSEHOLD_MEMBER,
                String.join(Constants.COMMA, filterRequestDTO.getFhirIds()),
                String.join(Constants.COMMA, villageIdentifiers));
        String urlEncounter = String.format(Constants.PERFORMANCE_QUERY_ENCOUNTER,
                String.join(Constants.COMMA, filterRequestDTO.getFhirIds()),
                String.join(Constants.COMMA, villageIdentifiers));
        String urlHouseHold = String.format(Constants.PERFORMANCE_QUERY_GROUP,
                String.join(Constants.COMMA, filterRequestDTO.getFhirIds()),
                String.join(Constants.COMMA, villageIdentifiers));
        String urlPatientStatus = String.format(Constants.PERFORMANCE_QUERY_SERVICE_REQUEST,
                String.join(Constants.COMMA, filterRequestDTO.getFhirIds()),
                String.join(Constants.COMMA, filterRequestDTO.getVillageIds()));

        Bundle bundle = new Bundle();
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlEncounter, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlPatientStatus, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlHouseHoldMember, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlHouseHold, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL).setValue(Constants.ANC));
        identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.VILLAGE_SYSTEM_URL).setValue(Constants.ZERO_STRING));
        identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.FOLLOW_UP_ID_SYSTEM_URL).setValue(Constants.ZERO_STRING));
        identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue(Constants.ZERO_STRING));
        identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL).setValue(Constants.ZERO_STRING));
        identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_CLOSED_REASON).setValue(Constants.ZERO_STRING));

        performanceMonitoringService.getChwPerformanceReport(filterRequestDTO);

        filterRequestDTO.setFromDate(new Date());
        filterRequestDTO.setToDate(new Date());
        Encounter encounter = new Encounter();
        encounter.setIdentifier(identifiers);
        bundle.addEntry().setResource(encounter);

        Group group = new Group();
        group.setIdentifier(identifiers);
        bundle.addEntry().setResource(group);

        RelatedPerson relatedPerson  = new RelatedPerson();
        relatedPerson.setIdentifier(identifiers);
        bundle.addEntry().setResource(relatedPerson);


        ServiceRequest serviceRequest = new ServiceRequest();
        serviceRequest.getMeta().setLastUpdated(new Date());
        serviceRequest.setIdentifier(identifiers);
        serviceRequest.addPerformer(new Reference(String.valueOf(ResourceType.RelatedPerson) + "/" +"12345"));
        bundle.addEntry().setResource(serviceRequest);
        bundle.addEntry().setResource(new RelatedPerson().setIdentifier(identifiers));

        if (Objects.nonNull(filterRequestDTO.getFromDate()) && Objects.nonNull(filterRequestDTO.getToDate())) {
            String fromDateString = new SimpleDateFormat(Constants.FHIR_SEARCH_DATE_FORMAT).format(filterRequestDTO.getFromDate());
            String toDateString = new SimpleDateFormat(Constants.FHIR_SEARCH_DATE_FORMAT).format(filterRequestDTO.getToDate());
            urlEncounter = StringUtil.concatString(urlEncounter,
                    String.format(Constants.PERFORMANCE_QUERY_DATE_FILTER, fromDateString, toDateString));
            urlPatientStatus = StringUtil.concatString(urlPatientStatus,
                    String.format(Constants.PERFORMANCE_QUERY_DATE_FILTER, fromDateString, toDateString));
            urlHouseHoldMember = StringUtil.concatString(urlHouseHoldMember,
                    String.format(Constants.PERFORMANCE_QUERY_DATE_FILTER, fromDateString, toDateString));
            urlHouseHold = StringUtil.concatString(urlHouseHold,
                    String.format(Constants.PERFORMANCE_QUERY_DATE_FILTER, fromDateString, toDateString));
        }
        when(restApiUtil.getBatchRequest("RelatedPerson?_id=null&_count=999")).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlEncounter, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlPatientStatus, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlHouseHoldMember, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlHouseHold, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.ZERO)))).thenReturn(bundle);
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlEncounter, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE)))).thenReturn(new Bundle());
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlPatientStatus, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE)))).thenReturn(new Bundle());
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlHouseHoldMember, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE)))).thenReturn(new Bundle());
        when(restApiUtil.getBatchRequest(StringUtil.concatString(urlHouseHold, String.format(Constants.PERFORMANCE_QUERY_LIMIT_SKIP, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE, Constants.PERFORMANCE_SEARCH_LIMIT_VALUE)))).thenReturn(new Bundle());
        when(fhirUtils.getIdFromReference("")).thenReturn("");

        Map<String, Map<String, PerformanceReport>> result = performanceMonitoringService.getChwPerformanceReport(filterRequestDTO);
        Assertions.assertNotNull(result);
    }

}
