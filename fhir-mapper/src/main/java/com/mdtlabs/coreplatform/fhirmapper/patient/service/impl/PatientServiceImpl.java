package com.mdtlabs.coreplatform.fhirmapper.patient.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Annotation;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Coverage;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Provenance;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.codesystems.SearchComparator;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.apiinterface.CqlApiInterface;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientFilterDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.VitalSignsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.fhir.SearchPersonDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.CommonUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.ConditionConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.EncounterConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PregnancyConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SearchPersonDetailsConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.VitalSignsConverter;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.patientvisit.service.PatientVisitService;

/**
 * <p>
 * This class is a service class to perform operation on Patient
 * operations.
 * </p>
 *
 * @author Nanthinee sugumar created on Feb 28, 2024
 */
@Service
public class PatientServiceImpl implements PatientService {

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    private final FhirMapper fhirMapper;

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final HouseholdService householdService;

    private final CqlApiInterface cqlApiInterface;

    private final SearchPersonDetailsConverter searchPersonDetailsConverter;

    private final PatientConverter patientConverter;

    private final SpiceConverter spiceConverter;

    private final PregnancyConverter pregnancyConverter;

    private final EncounterConverter encounterConverter;

    private final ConditionConverter conditionConverter;

    private final CommonConverter commonConverter;

    private final VitalSignsConverter vitalSignsConverter;

    private final PatientVisitService patientVisitService;

    @Value("${app.smart-anc}")
    private Boolean isSmartAnc;

    public PatientServiceImpl(FhirMapper fhirMapper, FhirAssessmentMapper fhirAssessmentMapper, FhirUtils fhirUtils, RestApiUtil restApiUtil,
                              HouseholdService householdService, CqlApiInterface cqlApiInterface, SearchPersonDetailsConverter searchPersonDetailsConverter,
                              PatientConverter patientConverter, SpiceConverter spiceConverter, PregnancyConverter pregnancyConverter, EncounterConverter encounterConverter,
                              ConditionConverter conditionConverter, CommonConverter commonConverter, VitalSignsConverter vitalSignsConverter,
                              PatientVisitService patientVisitService) {
        this.fhirMapper = fhirMapper;
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.householdService = householdService;
        this.cqlApiInterface = cqlApiInterface;
        this.searchPersonDetailsConverter = searchPersonDetailsConverter;
        this.patientConverter = patientConverter;
        this.spiceConverter = spiceConverter;
        this.pregnancyConverter = pregnancyConverter;
        this.encounterConverter = encounterConverter;
        this.conditionConverter = conditionConverter;
        this.commonConverter = commonConverter;
        this.vitalSignsConverter = vitalSignsConverter;
        this.patientVisitService = patientVisitService;
    }

    /**
     * Search Patient
     *
     * @param patientRequestDTO patientRequestDTO
     * @return PatientRequest Object
     */
    @Override
    public Map<String, Object> getPatientList(PatientRequestDTO patientRequestDTO) {
        Bundle bundle = new Bundle();
        Map<String, Object> responseMap = new HashMap<>();
        String url = String.format(Constants.PATIENT_LIST_PARAMS, patientRequestDTO.getLimit(),
                patientRequestDTO.getSkip(), Constants.MEDICAL_REVIEW);
        List<String> villageIdentifiers = patientRequestDTO.getVillageIds().stream().map(village ->
                StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL, Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        url = StringUtil.concatString(url, String.format(Constants.PATIENT_SUBJECT_IDENTIFIER, String.join(Constants.COMMA, villageIdentifiers)));
        List<String> excludedServiceRequestList = new ArrayList<>();
        if (!Objects.isNull(patientRequestDTO.getReferencePatientId())) {
            excludedServiceRequestList = getServiceRequestByPatientId(patientRequestDTO.getReferencePatientId(), null, false, null, false)
                    .getEntry().stream()
                    .map(entry -> entry.getResource().getIdPart())
                    .toList();
            url = StringUtil.concatString(url, String.format(Constants.PATIENT_REFERENCE_FILTER_PARAM,
                    String.join(Constants.COMMA, excludedServiceRequestList)));
        }
        String filterUrl = constructUrlWithFilter(patientRequestDTO);
        if (Constants.ZERO == patientRequestDTO.getSkip()) {
            responseMap.put(Constants.TOTAL_COUNT, getPatientCount(patientRequestDTO, excludedServiceRequestList, filterUrl));
        }
        if (Objects.isNull(patientRequestDTO.getFilter())) {
            String pregnancyUrl = String.format(Constants.PREGNANT_PATIENT_LIST_PARAMS, patientRequestDTO.getLimit(),
                    patientRequestDTO.getSkip());
            pregnancyUrl = StringUtil.concatString(pregnancyUrl, String.format(Constants.PATIENT_IDENTIFIER,
                    String.join(Constants.COMMA, villageIdentifiers)), Constants.PATIENT_ACTIVE_STATUS);
            List<PatientDTO> pregnantPatientList = getPregnantPatients(pregnancyUrl);
            Integer pregnantCount = getPregnantPatientCount(villageIdentifiers);
            if (Constants.ZERO == patientRequestDTO.getSkip()) {
                responseMap.put(Constants.TOTAL_COUNT, (Integer) responseMap.get(Constants.TOTAL_COUNT) + pregnantCount);
            }
            if (pregnantPatientList.size() > Constants.ZERO) {
                responseMap.put(Constants.PATIENT_LIST, pregnantPatientList);
                responseMap.put(Constants.REFERENCE_PATIENT_ID, null);
                return responseMap;
            } else {
                double limit = patientRequestDTO.getLimit();
                patientRequestDTO.setSkip(patientRequestDTO.getSkip() - (int) ((Math.ceil(pregnantCount / limit)) * limit));
                url = String.format(Constants.PATIENT_LIST_PARAMS, patientRequestDTO.getLimit(),
                        patientRequestDTO.getSkip(), Constants.MEDICAL_REVIEW);
                url = StringUtil.concatString(url, Constants.PREGNANT_SEARCH_PARAM, String.format(Constants.PATIENT_SUBJECT_IDENTIFIER,
                        String.join(Constants.COMMA, villageIdentifiers)));
                if (!Objects.isNull(patientRequestDTO.getReferencePatientId())) {
                    url = StringUtil.concatString(url, String.format(Constants.PATIENT_REFERENCE_FILTER_PARAM,
                            String.join(Constants.COMMA, excludedServiceRequestList)));
                }
            }
        }
        url = !Constants.EMPTY_STRING.equals(filterUrl)
                ? StringUtil.concatString(url, filterUrl)
                : StringUtil.concatString(url, String.format(Constants.PATIENT_STATUS_FILTER_PARAM,
                String.join(Constants.COMMA, Constants.ACTIVE, Constants.ON_HOLD)));
        patientSearchWithFilter(url, patientRequestDTO, bundle);
        List<PatientDTO> patientList = bundle.getEntry().stream()
                .filter(entry -> ResourceType.Patient.equals(entry.getResource().getResourceType()))
                .map(entry -> {
                    PatientDTO patientDTO = new PatientDTO();
                    Patient patient = (Patient) entry.getResource();
                    return fhirMapper.toPatient(patient, patientDTO);
                })
                .toList();
        responseMap.put(Constants.PATIENT_LIST, patientList);
        responseMap.put(Constants.REFERENCE_PATIENT_ID, patientList.isEmpty() ? null : patientList.getLast().getId());
        return responseMap;
    }

    /**
     * <p>
     * This method retrieves the total count of patients based on the given criteria.
     * </p>
     *
     * @param patientRequestDTO Contains the request parameters for filtering patients.
     * @param existsIds         A list of existing patient IDs to exclude from the count.
     * @param filterUrl         An optional URL for additional filtering of patients.
     * @return The total count of patients based on the given criteria.
     */
    private Integer getPatientCount(PatientRequestDTO patientRequestDTO, List<String> existsIds, String filterUrl) {
        String url = String.format(Constants.PATIENT_TOTAL_PARAMS, Constants.MEDICAL_REVIEW);
        if (!existsIds.isEmpty()) {
            url = StringUtil.concatString(url, String.format(Constants.PATIENT_REFERENCE_FILTER_PARAM,
                    String.join(Constants.COMMA, existsIds)));
        }
        if (Objects.isNull(patientRequestDTO.getFilter())) {
            url = StringUtil.concatString(url, Constants.PREGNANT_SEARCH_PARAM);
        }
        url = !Constants.EMPTY_STRING.equals(filterUrl)
                ? StringUtil.concatString(url, filterUrl)
                : StringUtil.concatString(url, String.format(Constants.PATIENT_STATUS_FILTER_PARAM,
                String.join(Constants.COMMA, Constants.ACTIVE, Constants.ON_HOLD)));
        Bundle bundle = new Bundle();
        for (String village : patientRequestDTO.getVillageIds()) {
            String finalUrl = StringUtil.concatString(url, String.format(Constants.PATIENT_SUBJECT_IDENTIFIER,
                    StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL, Constants.VERTICAL_BAR,
                            village)));
            patientSearchWithFilter(finalUrl, patientRequestDTO, bundle);
        }
        Set<String> patientList = new HashSet<>();
        bundle.getEntry().forEach(entry -> {
            if (entry.getResource() instanceof ServiceRequest serviceRequest) {
                patientList.add(serviceRequest.getSubject().getReference());
            }
        });
        return patientList.size();
    }

    /**
     * GET Patient In FHIR based on searchText and status
     *
     * @param url               Server Url
     * @param patientRequestDTO request Details
     * @param bundle            Bundle Object
     */
    private void patientSearchWithFilter(String url, PatientRequestDTO patientRequestDTO, Bundle bundle) {
        if (!Objects.isNull(patientRequestDTO.getSearchText())) {
            if (CommonUtil.isAllAlphabetic(patientRequestDTO.getSearchText())) {
                url = StringUtil.concatString(url,
                        String.format(Constants.PATIENT_NAME_SEARCH_PARAM, patientRequestDTO.getSearchText()));
                getMergedBundle(getPatientListByUrl(url), bundle);
            } else if (CommonUtil.isAllNumeric(patientRequestDTO.getSearchText())) {
                String phoneUrl = url;
                url = StringUtil.concatString(url,
                        String.format(Constants.PATIENT_PHONE_SEARCH_PARAM, patientRequestDTO.getSearchText()));
                phoneUrl = StringUtil.concatString(phoneUrl, String.format(Constants.PATIENT_IDENTIFIER_SEARCH_PARAM,
                        patientRequestDTO.getSearchText()));
                getMergedBundle(getPatientListByUrl(url), bundle);
                getMergedBundle(getPatientListByUrl(phoneUrl), bundle);
            }
        } else {
            getMergedBundle(getPatientListByUrl(url), bundle);
        }
    }

    /**
     * Add Filter to patientList Query
     *
     * @param patientRequestDTO RequestDetails
     * @return Url to search
     */
    private String constructUrlWithFilter(PatientRequestDTO patientRequestDTO) {
        String url = Constants.EMPTY_STRING;
        if (!Objects.isNull(patientRequestDTO.getFilter())) {
            PatientFilterDTO filterDTO = patientRequestDTO.getFilter();
            if (!Objects.isNull(filterDTO.getPatientStatus()) && !filterDTO.getPatientStatus().isEmpty()) {
                url = StringUtil.concatString(url, String.format(Constants.PATIENT_STATUS_FILTER_PARAM,
                        String.join(Constants.COMMA, filterDTO.getPatientStatus())));
            }
            if (!Objects.isNull(filterDTO.getVisitDate()) && !filterDTO.getVisitDate().isEmpty()) {
                String startDate;
                String endDate;
                if (Constants.ONE == filterDTO.getVisitDate().size()) {
                    startDate = fhirUtils.getFhirDateString(filterDTO.getVisitDate().getFirst());
                    endDate = fhirUtils.getFhirDateString(filterDTO.getVisitDate().getFirst());
                } else {
                    startDate = fhirUtils.getFhirDateString(Constants.TODAY);
                    endDate = fhirUtils.getFhirDateString(Constants.TOMORROW);
                }
                url = StringUtil.concatString(url,
                        String.format(Constants.PATIENT_VISIT_DATE_FILTER, startDate, endDate));
            }
        }
        return url;
    }

    /**
     * Get Patient by URL params
     *
     * @param url url details
     * @return Bundle
     */
    private Bundle getPatientListByUrl(String url) {
        return restApiUtil.getBatchRequest(url);
    }


    /**
     * Get Patient details
     *
     * @param requestDTO patient Details
     * @return PatientRequest Object
     */
    @Override
    public PatientDetailsDTO getPatientDetails(RequestDTO requestDTO) {
        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();
        Bundle bundle = getPatientDetailsByPatientId(requestDTO.getPatientId());
        HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberByPatientId(requestDTO.getPatientId());
        if (Objects.isNull(householdMemberDTO)) {
            throw new DataNotFoundException(1007);
        }
        if (Constants.ZERO == bundle.getTotal()) {
            //Set patient Details
            patientDetailsDTO.setPatientId(householdMemberDTO.getPatientId());
            patientDetailsDTO.setName(householdMemberDTO.getName());
            patientDetailsDTO.setBirthDate(householdMemberDTO.getDateOfBirth());
            patientDetailsDTO.setPhoneNumber(householdMemberDTO.getPhoneNumber());
            patientDetailsDTO.setPhoneNumberCategory(householdMemberDTO.getPhoneNumberCategory());
            patientDetailsDTO.setGender(householdMemberDTO.getGender().toLowerCase());
            patientDetailsDTO.setMemberId(householdMemberDTO.getId());
            patientDetailsDTO.setIsPregnant(householdMemberDTO.getIsPregnant());
            patientDetailsDTO.setVillageId(householdMemberDTO.getVillageId());
            patientDetailsDTO.setIsActive(householdMemberDTO.getIsActive());
        } else {
            PatientDTO patientDTO = new PatientDTO();
            bundle.getEntry().forEach(entry -> {
                Patient patient = (Patient) entry.getResource();
                Identifier identifier = patient.getIdentifier().stream()
                        .filter(value -> value.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL))
                        .findFirst().orElse(null);
                fhirMapper.toPatient(patient, patientDTO);
                if (Objects.nonNull(identifier) && Objects.nonNull(identifier.getValue())) {
                    patientDetailsDTO.setPregnancyStatus(fhirAssessmentMapper.extractStatus(identifier.getValue(),
                            List.of(Constants.REFERRED, Constants.ON_TREATMENT, Constants.RECOVERED)));
                }
            });
            //Set patient Details
            patientDetailsDTO.setPatientId(patientDTO.getPatientId());
            patientDetailsDTO.setName(patientDTO.getName());
            patientDetailsDTO.setBirthDate(patientDTO.getBirthDate());
            patientDetailsDTO.setPhoneNumber(patientDTO.getPhoneNumber());
            patientDetailsDTO.setPhoneNumberCategory(householdMemberDTO.getPhoneNumberCategory());
            patientDetailsDTO.setGender(patientDTO.getGender().toLowerCase());
            patientDetailsDTO.setId(patientDTO.getId());
            patientDetailsDTO.setMemberId(householdMemberDTO.getId());
            patientDetailsDTO.setIsPregnant(householdMemberDTO.getIsPregnant());
            patientDetailsDTO.setVillageId(householdMemberDTO.getVillageId());
            patientDetailsDTO.setIsActive(householdMemberDTO.getIsActive());
            requestDTO.setPatientReference(patientDTO.getId());
            requestDTO.setType(null);
            patientDetailsDTO.setDiagnosis(getPatientDiagnosis(requestDTO, Boolean.TRUE, Boolean.FALSE));
        }
        patientDetailsDTO.setVillage(householdMemberDTO.getVillage());
        patientDetailsDTO.setRelationship(householdMemberDTO.getHouseholdHeadRelationship());
        patientDetailsDTO.setMotherPatientId(householdMemberDTO.getMotherPatientId());
        if (Objects.nonNull(householdMemberDTO.getHouseholdId())) {
            RequestDTO householdRequestDTO = new RequestDTO();
            householdRequestDTO.setId(householdMemberDTO.getHouseholdId());
            HouseholdDTO householdDTO =
                    householdService.getHouseholdList(householdRequestDTO).stream().findFirst().orElse(null);
            if (Objects.nonNull(householdDTO)) {
                patientDetailsDTO.setHouseHoldId(householdMemberDTO.getHouseholdId());
                patientDetailsDTO.setHouseHoldNumber(householdDTO.getHouseholdNo());
                patientDetailsDTO.setLandmark(householdDTO.getLandmark());
            }
        }
        patientDetailsDTO.setPregnancyDetails(getPatientPregnancyDetails(patientDetailsDTO.getId(),
                patientDetailsDTO.getMemberId(), patientDetailsDTO.getPatientId()));
        return patientDetailsDTO;
    }

    /**
     * get PatientPregnancyDetails
     *
     * @param id patient Id
     * @return PregnancyDetailsDTO
     */
    public PregnancyDetailsDTO getPatientPregnancyDetails(String id, String memberId, String patientId) {
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        if (Boolean.TRUE.equals(isSmartAnc) && Objects.nonNull(patientId)) {
            try {
                Map<String, Object> results = cqlApiInterface.getAncResultByPatientId(patientId,
                        CommonUtil.getAuthToken(), CommonUtil.getClient()).getBody();
                pregnancyDetailsDTO.setSmartAncContactDetails(Objects.isNull(results) || results.isEmpty()
                        ? null : new ObjectMapper().writeValueAsString(results));
            } catch (JsonProcessingException e) {
                throw new ServicesException(1005);
            }
        }
        Map<String, PregnancyInfo> pregnancyInfo = getPregnancyInfoFromPatientVitals(List.of(memberId));
        pregnancyDetailsDTO.setLastMenstrualPeriod(pregnancyInfo.get(memberId).getLastMenstrualPeriod());
        pregnancyDetailsDTO.setEstimatedDeliveryDate(pregnancyInfo.get(memberId).getEstimatedDeliveryDate());
        pregnancyDetailsDTO.setAncVisitMedicalReview(pregnancyInfo.get(memberId).getAncMedicalReviewVisitNo());
        pregnancyDetailsDTO.setPncVisitMedicalReview(pregnancyInfo.get(memberId).getPncMotherMedicalReviewVisitNo());
        pregnancyDetailsDTO.setDateOfDelivery(pregnancyInfo.get(memberId).getDateOfDelivery());
        pregnancyDetailsDTO.setPncCreatedDate(pregnancyInfo.get(memberId).getPncCreatedDate());
        pregnancyDetailsDTO.setNoOfNeonates(pregnancyInfo.get(memberId).getNoOfNeonates());
        pregnancyDetailsDTO.setNeonatePatientId(pregnancyInfo.get(memberId).getNeonatePatientId());
        pregnancyDetailsDTO.setNeonatalOutcomes(pregnancyInfo.get(memberId).getNeonateOutcome());
        return pregnancyDetailsDTO;
    }

    /**
     * Get Patient details by Identifier patientId
     *
     * @param id patientId value
     * @return Bundle Object
     */
    public Bundle getPatientDetailsByPatientId(String id) {
        return restApiUtil.getBatchRequest(
                String.format(Constants.GET_PATIENT_BY_ID, FhirIdentifierConstants.PATIENT_ID_SYSTEM_URL, id));
    }

    /**
     * Get Patient details by Identifier patientId
     *
     * @param id patientId value
     * @return Bundle Object
     */
    public Bundle getPatientDetailsByPatientReference(String id) {
        return restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_PATIENT_REFERENCE, id).concat(Constants.PATIENT_ACTIVE_STATUS));
    }

    /**
     * search Patient
     *
     * @param patientRequestDTO patientRequestDTO
     * @return List<PatientDTO>
     */
    public Map<String, Object> searchPatient(PatientRequestDTO patientRequestDTO) {
        Bundle groupValue = new Bundle();
        List<String> villageIdentifiers = patientRequestDTO.getVillageIds().stream().map(village ->
                StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL, Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        String villageUrl = String.format(Constants.IDENTIFIER_QUERY, String.join(Constants.COMMA, villageIdentifiers));
        if (CommonUtil.isAllAlphabetic(patientRequestDTO.getSearchText())) {
            String nameUrl = String.format(Constants.PATIENT_PARAMS_NAME, patientRequestDTO.getSearchText());
            String url = String.format(Constants.RELATED_PERSON_LIST_PARAMS,
                    patientRequestDTO.getLimit(), patientRequestDTO.getSkip());
            url = StringUtil.concatString(url, nameUrl, villageUrl);
            getMergedBundle(getPatientListByUrl(url), groupValue);
        } else if (CommonUtil.isAllNumeric(patientRequestDTO.getSearchText())) {
            String patientIdParam = StringUtil.concatString(String.format(Constants.RELATED_PERSON_LIST_PARAMS,
                            patientRequestDTO.getLimit(), patientRequestDTO.getSkip()),
                    String.format(Constants.PATIENT_PARAMS_PATIENT_ID, patientRequestDTO.getSearchText()), villageUrl);
            String telecomParam = StringUtil.concatString(String.format(Constants.RELATED_PERSON_LIST_PARAMS,
                            patientRequestDTO.getLimit(), patientRequestDTO.getSkip()),
                    String.format(Constants.PATIENT_PARAMS_TELECOM, patientRequestDTO.getSearchText()), villageUrl);
            getMergedBundle(getPatientListByUrl(patientIdParam), groupValue);
            getMergedBundle(getPatientListByUrl(telecomParam), groupValue);
        }
        List<PatientDTO> patient = new ArrayList<>();
        groupValue.getEntry().forEach(entry -> {
            PatientDTO patientDTO = new PatientDTO();
            RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
            fhirMapper.relatedPersonToPatient(relatedPerson, patientDTO);
            patient.add(patientDTO);
        });
        List<PatientDTO> filteredPatient = new ArrayList<>();
        patient.forEach(myPatient -> {
            if (!filteredPatient.contains(myPatient)) {
                filteredPatient.add(myPatient);
            }
        });
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put(Constants.PATIENT_LIST, filteredPatient);
        responseMap.put(Constants.REFERENCE_PATIENT_ID, filteredPatient.isEmpty() ? null : filteredPatient.getLast().getMemberId());
        return responseMap;
    }

    /**
     * Merge a bundle
     *
     * @param bundleValue  bundle
     * @param mergedBundle bundle
     */
    public void getMergedBundle(Bundle bundleValue, Bundle mergedBundle) {
        bundleValue.getEntry().forEach(mergedBundle::addEntry);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getPatientStatus(RequestDTO requestDTO) {
        Map<String, Object> responseMap = new HashMap<>();
        Patient patient = getPatientById(requestDTO.getPatientId());
        String existingStatus = Constants.EMPTY_STRING;
        String patientStatus = Constants.EMPTY_STRING;
        if (!Objects.isNull(patient)) {
            existingStatus = patient.getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(
                            identifier.getSystem()) && Objects.nonNull(identifier.getValue())).map(Identifier::getValue).findFirst()
                    .orElse(Constants.EMPTY_STRING);
            patientStatus = existingStatus;
            existingStatus = fhirAssessmentMapper.extractStatus(existingStatus, Constants.SKIP_STATUS);
        }
        if (!List.of(Constants.REFERRED, Constants.ON_TREATMENT).contains(existingStatus)) {
            if (checkActiveTicketStatus(requestDTO.getMemberId())) {
                existingStatus = Constants.REFERRED;
            } else if (checkOnTreatmentStatus(requestDTO.getMemberId())) {
                existingStatus = Constants.ON_TREATMENT;
            } else {
                existingStatus = Constants.EMPTY_STRING;
            }
        }
        responseMap.put(Constants.STATUS, existingStatus);
        if (!Objects.isNull(requestDTO.getEncounterType()) && requestDTO.getEncounterType().equals(Constants.PREGNANCY_ANC_MEDICAL_REVIEW)) {
            responseMap.put(Constants.STATUS, fhirAssessmentMapper.setIccmStatus(existingStatus, Constants.PREGNANCY));
        } else if (Constants.FEMALE.equalsIgnoreCase(requestDTO.getGender())) {
            Map<String, PregnancyInfo> pregnancyInfoMap = getPregnancyInfoFromPatientVitals(
                    List.of(requestDTO.getMemberId()));
            PregnancyInfo pregnancyInfo = pregnancyInfoMap.get(requestDTO.getMemberId());
            Date pncDeliveryDate = Objects.nonNull(pregnancyInfo.getDateOfDelivery()) ? pregnancyInfo.getDateOfDelivery() :
                    pregnancyInfo.getPncCreatedDate();
            String status = getPregnancyDetails(pncDeliveryDate, existingStatus,
                    requestDTO.getIsPregnant());
            responseMap.put(Constants.STATUS, status);
        }

        if (!Objects.isNull(responseMap.get(Constants.STATUS)) &&
                !String.valueOf(responseMap.get(Constants.STATUS)).equalsIgnoreCase(patientStatus)) {
            Bundle bundle = new Bundle().setType(BundleType.TRANSACTION);
            updatePatientOverallStatus(bundle, String.valueOf(responseMap.get(Constants.STATUS)),
                    requestDTO.getProvenance(), requestDTO.getMemberId());
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        }

        if (!Objects.isNull(responseMap.get(Constants.STATUS))) {
            responseMap.put(Constants.STATUS, fhirAssessmentMapper.changePatientStatusToDisplayFormat(
                    String.valueOf(responseMap.get(Constants.STATUS))));
        }
        return responseMap;
    }

    /**
     * Update Patient status Based on PregnancyDetails
     *
     * @param dateValue deliveryDate
     * @param status    Current Status
     * @return Status
     */
    private String getPregnancyDetails(Date dateValue, String status, Boolean isPregnant) {
        String iccmStatus = fhirAssessmentMapper.extractStatus(status, Constants.PREGNANCY_STATUS);

        if (Boolean.TRUE.equals(isPregnant)) {
            if (Objects.isNull(dateValue)) {
                status = fhirAssessmentMapper.setIccmStatus(iccmStatus, Constants.PREGNANCY);
            }
        } else {
            long weeks = DateUtil.daysSincePastInWeeks(dateValue);
            if (!Objects.isNull(dateValue) && weeks < Constants.SEVEN) {
                status = fhirAssessmentMapper.setIccmStatus(iccmStatus, Constants.POSTPARTUM);
            } else if (!Objects.isNull(dateValue) && DateUtil.daysSincePastInMonths(dateValue) < Constants.SIXTEEN) {
                status = fhirAssessmentMapper.setIccmStatus(iccmStatus, Constants.LACTATING);
            } else {
                status = iccmStatus;
            }
        }
        return status;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseEntity<FhirResponseDTO> updateStatusOfServiceRequest(RequestDTO requestDTO) {
        Bundle bundle = new Bundle().setType(BundleType.TRANSACTION);
        Bundle responseBundle = getServiceRequestByPatientId(requestDTO.getCategory(),
                requestDTO.getTicketType(), Constants.COMPLETED_SMALLER_CASE, requestDTO.getMemberId());
        responseBundle.getEntry().forEach(entry -> {
            if (String.valueOf(ResourceType.ServiceRequest).equals(String.valueOf(entry.getResource().getResourceType()))) {
                ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
                String id = fhirUtils.getIdFromHistoryUrl(serviceRequest.getId());
                serviceRequest.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())) {
                        identifier.setValue(requestDTO.getPatientStatus());
                    }
                });

                if (Constants.RECOVERED.equals(requestDTO.getPatientStatus())) {
                    serviceRequest.setStatus(ServiceRequest.ServiceRequestStatus.COMPLETED);
                    serviceRequest.setPriority(ServiceRequest.ServiceRequestPriority.ROUTINE);
                }

                fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH, id),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, id),
                        Bundle.HTTPVerb.PUT, serviceRequest, bundle, requestDTO.getProvenance());
            }
        });
        return bundle.getEntry().isEmpty() ? null : restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * Create New patient in FHIR
     *
     * @param patientId patientId Details
     * @param bundle    Bundle Object
     * @return String Patient Reference
     */
    public Map<String, String> createPatient(String patientId, Bundle bundle, ProvenanceDTO provenance) {
        Map<String, String> patientDetail = new HashMap<>();
        HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberByPatientId(patientId);
        patientDetail.put(Constants.MEMBER_ID, householdMemberDTO.getId());
        validatePatient(householdMemberDTO);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH);
        if (!Objects.isNull(householdMemberDTO.getPatientReference())) {
            url = StringUtil.concatString(url, householdMemberDTO.getPatientReference());
        } else {
            String uuid = fhirUtils.getUniqueId();
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
            url = StringUtil.concatString(url, Constants.FHIR_BASE_URL, uuid);
            Patient patient = fhirAssessmentMapper.setPatient(new Patient(),
                    householdMemberDTO,
                    provenance.getUserId());
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, patient, bundle, provenance);
        }
        patientDetail.put(String.valueOf(ResourceType.Patient), url);
        return patientDetail;
    }

    /**
     * {@inheritDoc}
     */
    public EnrollmentResponseDTO updatePatient(EnrollmentRequestDTO enrollmentRequestDto) {
        Bundle transcationBundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        Bundle bundle = null;
        EnrollmentResponseDTO enrollmentResponseDTO = new EnrollmentResponseDTO();
        if (!Objects.isNull(enrollmentRequestDto.getPatientReference())) {
            bundle = restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_PATIENT_REFERENCE,
                    enrollmentRequestDto.getPatientReference()));
        }
        if (!Objects.isNull(bundle) && !bundle.getEntry().isEmpty()) {
            Patient patient = (Patient) bundle.getEntry().getFirst().getResource();
            patientConverter.mapPatient(patient, enrollmentRequestDto, Constants.PATIENT_UPDATION_TYPE);
            updateCoverage(patient, enrollmentRequestDto, transcationBundle);
            fhirUtils.setBundle(StringUtil.concatString(ResourceType.Patient.toString(), Constants.FORWARD_SLASH,
                            patient.getIdPart()), StringUtil.concatString(Constants.FHIR_BASE_URL, patient.getIdPart()),
                    Bundle.HTTPVerb.PUT, patient, transcationBundle, enrollmentRequestDto.getProvenance()
            );
            if (patient.getIdentifier().stream().anyMatch(patientIdentifier -> Objects.equals(patientIdentifier.getValue(),
                    Constants.ENROLLED))) {
                bundle = restApiUtil.getBatchRequest(String.format(Constants.GET_MEMBER_ID,
                        fhirUtils.getIdFromReference(patient.getLink().getFirst().getOther().getReference())));
                RelatedPerson relatedPerson = (RelatedPerson) bundle.getEntry().getFirst().getResource();
                patientConverter.mapRelatedPerson(relatedPerson, enrollmentRequestDto, Constants.PATIENT_UPDATION_TYPE);
                fhirUtils.setBundle(
                        StringUtil.concatString(ResourceType.RelatedPerson.toString(), Constants.FORWARD_SLASH,
                                relatedPerson.getIdPart()),
                        StringUtil.concatString(Constants.FHIR_BASE_URL, relatedPerson.getIdPart()),
                        Bundle.HTTPVerb.PUT, relatedPerson, transcationBundle, enrollmentRequestDto.getProvenance()
                );
            }
            patientConverter.convertToEnrollmentResponseDTO(patient, enrollmentResponseDTO);
        }
        return enrollmentResponseDTO;
    }

    public void updateCoverage(Patient patient, EnrollmentRequestDTO enrollmentRequestDTO, Bundle transactionBundle) {
        if (Objects.nonNull(enrollmentRequestDTO.getBioData().getInsuranceStatus())) {
            Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.COVERAGE_BY_PATIENT_PARAMS,
                    patient.getIdPart()));
            if (!Objects.isNull(bundle) && !bundle.getEntry().isEmpty()) {
                Coverage coverage = (Coverage) bundle.getEntry().getFirst().getResource();
                String url = StringUtil.concatString(String.valueOf(ResourceType.Coverage), Constants.FORWARD_SLASH,
                        coverage.getIdPart());
                if (Boolean.TRUE.equals(enrollmentRequestDTO.getBioData().getInsuranceStatus())) {
                    coverage.setStatus(Coverage.CoverageStatus.ACTIVE);
                    coverage.setSubscriberId(enrollmentRequestDTO.getBioData().getInsuranceId());
                    if (Objects.nonNull(enrollmentRequestDTO.getBioData().getInsuranceType())) {
                        coverage.setType(enrollmentRequestDTO.getBioData().getInsuranceType().equalsIgnoreCase(MetaCodeConstants.OTHER) ?
                                fhirUtils.createCodeableConcept(MetaCodeConstants.OTHER + Constants.HIGHFIN + enrollmentRequestDTO.getBioData().getOtherInsurance()) :
                                fhirUtils.createCodeableConcept(enrollmentRequestDTO.getBioData().getInsuranceType()));
                    }

                    coverage.setBeneficiary(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                            Constants.FORWARD_SLASH, patient.getIdPart())));
                } else {
                    coverage.setStatus(Coverage.CoverageStatus.CANCELLED);
                }
                fhirUtils.setBundle(url, Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, coverage, transactionBundle,
                        enrollmentRequestDTO.getProvenance());
            } else {
                createCoverage(enrollmentRequestDTO, patient.getIdPart(), transactionBundle);
            }
        }
    }

    /**
     * Get Patient and validate and set FHIRId.
     *
     * @param householdMemberDTO patient Details
     */
    private void validatePatient(HouseholdMemberDTO householdMemberDTO) {
        Bundle bundle = getPatientDetailsByPatientId(householdMemberDTO.getPatientId());
        if (!bundle.getEntry().isEmpty()) {
            bundle.getEntry().forEach(entry -> {
                Patient patient = (Patient) entry.getResource();
                householdMemberDTO.setPatientReference(fhirUtils.getIdFromHistoryUrl(patient.getId()));
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ReferralTicketDTO> getReferralTicketes(String patientId, String serviceRequestId, String type,
                                                       String assessmentType) {
        List<ReferralTicketDTO> referralTicketDTOS = new ArrayList<>();
        Bundle bundle = getServiceRequestByPatientId(patientId, serviceRequestId, Boolean.TRUE, type, Boolean.TRUE);
        Map<String, Resource> resourceMap = new HashMap<>();
        Map<String, Provenance> providencdeResourceMap = new TreeMap<>();
        List<Map<String, Object>> referredDates = new ArrayList<>();
        //Create Resource Map
        bundle.getEntry().forEach(entry -> {
            if (ResourceType.Provenance.equals(entry.getResource().getResourceType())) {
                Provenance provenance = (Provenance) entry.getResource();
                if (!provenance.getAgent().getFirst().isEmpty() &&
                        !provenance.getAgent().getFirst().getRole().isEmpty() &&
                        Constants.POST.equals(provenance.getAgent().getFirst().getRole().getFirst().getText())) {
                    providencdeResourceMap.put(provenance.getTarget().getFirst().getReferenceElement().getIdPart(),
                            provenance);
                }
            } else {
                String resourceType = String.valueOf(entry.getResource().getResourceType());
                resourceMap.put(StringUtil.concatString(resourceType, Constants.FORWARD_SLASH,
                        fhirUtils.getIdFromHistoryUrl(entry.getResource().getId())), entry.getResource());
            }
        });

        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (String.valueOf(ResourceType.ServiceRequest).equals(String.valueOf(entry.getResource().getResourceType()))) {
                ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
                serviceRequest.getIdentifier().forEach(value -> {
                    if (value.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL) &&
                            value.getValue().contains(Constants.REFERRED)) {
                        String id = fhirUtils.getIdFromHistoryUrl(serviceRequest.getId());
                        Map<String, Object> referredDate = new HashMap<>();
                        referredDate.put(Constants.ID, id);
                        Date initialReferredDate = providencdeResourceMap.get(serviceRequest.getIdPart()).getRecorded();
                        referredDate.put(Constants.DATE, initialReferredDate);
                        referredDates.add(referredDate);
                    }
                });
            }
        }
        if (!referredDates.isEmpty()) {
            referredDates.sort(
                    Collections.reverseOrder(Comparator.comparingInt(o -> Integer.parseInt((String) o.get(Constants.ID)))));
            ServiceRequest serviceRequest = (ServiceRequest) resourceMap.get(
                    StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH,
                            String.valueOf(referredDates.getFirst().get(Constants.ID))));
            ReferralTicketDTO referralTicketDTO = new ReferralTicketDTO();
            Date initialReferredDate = providencdeResourceMap.get(serviceRequest.getIdPart()).getRecorded();
            referralTicketDTO = fhirAssessmentMapper.setReferralTicket(serviceRequest, referralTicketDTO, resourceMap,
                    providencdeResourceMap);
            referralTicketDTO.setReferredDate(initialReferredDate);
            referralTicketDTO.setDateOfOnset(serviceRequest.getAuthoredOn());
            referralTicketDTOS.add(referralTicketDTO);
            referralTicketDTOS.getFirst().setReferredDates(referredDates);
        }
        return referralTicketDTOS;
    }

    /**
     * Create Referral Ticket For Patient
     *
     * @param referralTicketDTO referralTicket Details
     */
    @Override
    public ReferralDetailsDTO createReferralTicket(ReferralDetailsDTO referralTicketDTO, Bundle bundle,
                                                   boolean createRecord, boolean updatePrevious, Boolean isPregnant) {
        if (Constants.REFERRED.equalsIgnoreCase(referralTicketDTO.getPatientStatus()) && !Objects.isNull(
                referralTicketDTO.getPatientId())) {
            Map<String, String> patientRefDetails = createPatient(referralTicketDTO.getPatientId(), bundle, referralTicketDTO.getProvenance());
            referralTicketDTO.setPatientReference(patientRefDetails.get(String.valueOf(ResourceType.Patient)));
        }

        String uuidServiceRequest = fhirUtils.getUniqueId();
        if (Objects.isNull(isPregnant)) {
            HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberById(
                    referralTicketDTO.getMemberId());
            if (Constants.FEMALE.equalsIgnoreCase(householdMemberDTO.getGender())) {
                Map<String, PregnancyInfo> pregnancyInfoMap = getPregnancyInfoFromPatientVitals(List.of(householdMemberDTO.getId()));
                PregnancyInfo pregnancyInfo = pregnancyInfoMap.get(householdMemberDTO.getId());
                Date pncDeliveryDate = Objects.nonNull(pregnancyInfo.getDateOfDelivery()) ? pregnancyInfo.getDateOfDelivery() :
                        pregnancyInfo.getPncCreatedDate();
                String status = getPregnancyDetails(pncDeliveryDate, referralTicketDTO.getPatientStatus(),
                        householdMemberDTO.getIsPregnant());
                referralTicketDTO.setPatientStatus(status);
            }
        } else if (Boolean.TRUE.equals(isPregnant)) {
            referralTicketDTO.setPatientStatus(
                    fhirAssessmentMapper.setIccmStatus(referralTicketDTO.getPatientStatus(), Constants.PREGNANCY));
        } else {
            String status = getPregnancyDetails(referralTicketDTO.getDateOfDelivery(), referralTicketDTO.getPatientStatus(),
                    Boolean.FALSE);
            referralTicketDTO.setPatientStatus(status);
        }

        if (Constants.MEDICAL_REVIEW.equalsIgnoreCase(referralTicketDTO.getType()) &&
                !Constants.SKIP_STATUS_CATEGORY.contains(referralTicketDTO.getCategory())) {
            updatePatientOverallStatus(bundle, referralTicketDTO.getPatientStatus(), referralTicketDTO.getProvenance(),
                    referralTicketDTO.getMemberId());
        }

        String id = StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, uuidServiceRequest);
        ServiceRequest serviceRequest = fhirAssessmentMapper.createReferralTicket(referralTicketDTO);
        if (updatePrevious) {
            updatePreviousServiceRequest(referralTicketDTO, serviceRequest);
        }
        fhirUtils.setBundle(id, StringUtil.concatString(Constants.FHIR_BASE_URL, uuidServiceRequest),
                Bundle.HTTPVerb.POST, serviceRequest, bundle, referralTicketDTO.getProvenance());
        if (createRecord) {
            restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        }
        return referralTicketDTO;
    }

    /**
     * Create Referral Ticket For Patient
     *
     * @param referralTicketDTO referralTicket Details
     */
    public ReferralDetailsDTO createReferralTicketForMedicalReview(ReferralDetailsDTO referralTicketDTO, Bundle bundle,
                                                                   boolean createRecord, boolean updatePrevious) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientStatus(Constants.REFERRED);
        requestDTO.setMemberId(referralTicketDTO.getMemberId());
        requestDTO.setTicketType(Constants.MEDICAL_REVIEW);
        requestDTO.setProvenance(referralTicketDTO.getProvenance());
        requestDTO.setCategory(!referralTicketDTO.getCategory().equals(Constants.RMNCH) ?
                StringUtil.concatString(Constants.ICCM, Constants.COMMA, Constants.RMNCH, Constants.COMMA,
                        Constants.CHILDHOOD_VISIT) :
                StringUtil.concatString(Constants.ICCM, Constants.COMMA, Constants.RMNCH, Constants.COMMA,
                        Constants.CHILDHOOD_VISIT, Constants.COMMA, Constants.RMNCH_VISIT));
        requestDTO.setReason(null);
        requestDTO.setEncounterId(StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH,
                referralTicketDTO.getEncounterId()));
        requestDTO.setCloseReferralTicket(Boolean.TRUE);
        requestDTO.setClosedReason(Constants.REFERRED);
        requestDTO.setClosedEncounterType(referralTicketDTO.getAssessmentName());
        referralTicketDTO.setEncounterType(referralTicketDTO.getAssessmentName());
        updateReferralTicketByMemberId(requestDTO, bundle);
        if (!Objects.isNull(referralTicketDTO.getEncounterId())) {
            fhirAssessmentMapper.updateEncounterStatusDetails(
                    fhirUtils.getIdFromResourceUrl(referralTicketDTO.getEncounterId()),
                    referralTicketDTO.getProvenance(), Constants.REFERRED, bundle);
        }
        return createReferralTicket(referralTicketDTO, bundle, createRecord, updatePrevious, null);
    }


    /**
     * Update Previous ServiceRequest status and authored on
     *
     * @param referralDetailsDTO    ticket Details
     * @param currentServiceRequest new ServiceRequest
     */
    private ResponseEntity<FhirResponseDTO> updatePreviousServiceRequest(ReferralDetailsDTO referralDetailsDTO,
                                                                         ServiceRequest currentServiceRequest) {
        Bundle bundle = new Bundle().setType(BundleType.TRANSACTION);
        String requestUrl = String.format(Constants.PREVIOUS_SERVICE_REQUEST,
                FhirIdentifierConstants.TICKET_CATEGORY_SYSTEM_URL,
                referralDetailsDTO.getCategory(), FhirIdentifierConstants.TICKET_TYPE_SYSTEM_URL,
                referralDetailsDTO.getType(), referralDetailsDTO.getMemberId());
        Bundle bundleEncounter = restApiUtil.getBatchRequest(requestUrl);
        bundleEncounter.getEntry().forEach(entry -> {
            ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
            currentServiceRequest.setAuthoredOn(
                    !Objects.isNull(serviceRequest.getAuthoredOn()) ? serviceRequest.getAuthoredOn() :
                            referralDetailsDTO.getProvenance().getModifiedDate());
            serviceRequest.setPriority(ServiceRequest.ServiceRequestPriority.ROUTINE);
            String id = serviceRequest.getIdPart();
            String url = StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH,
                    id);
            String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, id);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.PUT, serviceRequest, bundle,
                    referralDetailsDTO.getProvenance());
        });
        return bundleEncounter.getEntry().isEmpty() ? null :
                restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * Return ServiceRequest Based on patientId and req Id
     *
     * @param patientFhirId    patient FHIR id
     * @param serviceRequestId Service request Id
     * @return Bundle Object
     */
    private Bundle getServiceRequestByPatientId(String patientFhirId, String serviceRequestId, Boolean includeResources,
                                                String type, boolean skipRecovered) {
        String url = String.format(Constants.SERVICE_REQUEST_QUERY,
                StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, patientFhirId));

        if (Boolean.TRUE.equals(includeResources)) {
            url = StringUtil.concatString(url, Constants.INCLUDE_SUB_RESOURCES);
        }

        if (Boolean.TRUE.equals(skipRecovered)) {
            url = StringUtil.concatString(url,
                    String.format(Constants.IDNTIFIER_NOT, FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL,
                            Constants.RECOVERED));
        }

        if (!Objects.isNull(serviceRequestId)) {
            url = StringUtil.concatString(url, String.format(Constants.ID_WHERE,
                    StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH,
                            serviceRequestId)));
        }

        if (!Objects.isNull(type)) {
            url = StringUtil.concatString(url, String.format(Constants.SERVICE_REQUEST_STATUS_REQUISITION_QUERY,
                    FhirIdentifierConstants.TICKET_TYPE_SYSTEM_URL, type));
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Return ServiceRequest Based on patientId and req Id
     *
     * @param category Service request category
     * @param type     Type of service request
     * @param status   Service request status
     * @param memberId Member Id
     * @return Bundle Object
     */
    private Bundle getServiceRequestByPatientId(String category, String type, String status,
                                                String memberId) {
        String url = String.format(Constants.SERVICE_REQUEST_QUERY_PERFORMER,
                StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                        memberId));
        if (!Objects.isNull(status)) {
            url = String.format(StringUtil.concatString(url, Constants.SERVICE_REQUEST_STATUS_NOT_COMPLETE_QUERY));
        }

        if (!Objects.isNull(category)) {
            url = StringUtil.concatString(url, String.format(Constants.SERVICE_REQUEST_STATUS_ENCOUNTER_TYPE_QUERY,
                    FhirIdentifierConstants.TICKET_CATEGORY_SYSTEM_URL, category));
        }

        if (!Objects.isNull(type)) {
            url = StringUtil.concatString(url, String.format(Constants.SERVICE_REQUEST_STATUS_REQUISITION_QUERY,
                    FhirIdentifierConstants.TICKET_TYPE_SYSTEM_URL, type));
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * {@inheritDoc}
     */
    public void createDiagnosis(DiagnosisDTO diagnosis) {
        fhirUtils.initiateCodesMap();
        if (Objects.isNull(diagnosis.getPatientId())) {
            throw new SpiceValidation(2002);
        }
        if (StringUtils.isNotBlank(diagnosis.getOtherNotes())) {
            DiseaseDTO diseaseDTO = new DiseaseDTO();
            diseaseDTO.setDiseaseCategory(Constants.OTHER_NOTES);
            diseaseDTO.setDiseaseCondition(diagnosis.getOtherNotes());
            diseaseDTO.setType(diagnosis.getType());
            diagnosis.getDiseases().add(diseaseDTO);
        }
        Bundle bundle = new Bundle().setType(BundleType.TRANSACTION);
        String patientId;
        Map<String, Condition> patientConditions = new HashMap<>();
        if (Objects.isNull(diagnosis.getPatientReference()) && !Objects.isNull(diagnosis.getPatientId())) {
            Map<String, String> patient = createPatient(diagnosis.getPatientId(), bundle, diagnosis.getProvenance());
            patientId = patient.get(String.valueOf(ResourceType.Patient));
        } else {
            patientId = StringUtil.concatString(String.valueOf(ResourceType.Patient),
                    Constants.FORWARD_SLASH, diagnosis.getPatientReference());
            patientConditions = getPatientConditions(diagnosis.getPatientReference(), diagnosis.getType());
        }
        List<String> dieasesList = new ArrayList<>();
        for (DiseaseDTO diseaseCondition : diagnosis.getDiseases()) {
            String diseaseName = StringUtils.isEmpty(diseaseCondition.getDiseaseCondition()) ? diseaseCondition.getDiseaseCategory()
                    : diseaseCondition.getDiseaseCondition();
            if (!patientConditions.containsKey(diseaseName)) {
                Condition condition = new Condition();
                condition.addIdentifier().setSystem(FhirIdentifierConstants.CONDITION_SYSTEM_URL).setValue("diagnosis");
                condition.addIdentifier().setSystem(FhirIdentifierConstants.CONDITION_DISEASE_TYPE_URL).setValue(diagnosis.getType());
                condition.setCategory(List.of(fhirUtils.createCodeableConcept(diseaseCondition.getDiseaseCategory())));
                condition.setCode(fhirUtils.createCodeableConcept(diseaseName));
                CodeableConcept codeableConcept = new CodeableConcept();
                Coding coding = new Coding(Constants.CONDITION_CLINCAL_STATUS_SYSTEM, Constants.CLINCAL_STATUS_CODE_ACTIVE, Constants.CLINCAL_STATUS_DISPLAY_ACTIVE);
                codeableConcept.setCoding(List.of(coding));
                condition.setClinicalStatus(codeableConcept);
                condition.setRecordedDate(new Date());
                condition.setSubject(new Reference(patientId));

                String uuid = fhirUtils.getUniqueId();
                String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
                String url = StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
                fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, condition, bundle, diagnosis.getProvenance());
            }
            dieasesList.add(diseaseName);
        }

        for (Map.Entry<String, Condition> entry : patientConditions.entrySet()) {
            if (!dieasesList.contains(entry.getKey())) {
                updateDiagnosisStatus(entry.getValue(), diagnosis, bundle);
            }
        }
        restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * {@inheritDoc}
     */
    public List<DiseaseDTO> getPatientDiagnosis(RequestDTO request, Boolean isMedicalReviewSummary,
                                                boolean includeAllCategories) {
        if (Objects.isNull(request.getPatientReference())) {
            return new ArrayList<>();
        }
        fhirUtils.initiateCodesMap();
        Bundle bundle = restApiUtil.getBatchRequest(!Objects.isNull(request.getType()) ?
                String.format(Constants.CONDITION_QUERY, request.getPatientReference(), request.getType()) :
                String.format(Constants.CONDITION_QUERY_WITHOUT_TYPE, request.getPatientReference()));
        List<DiagnosisDTO.DiseaseDTO> diseases = new ArrayList<>();
        Set<String> diseaseCategory = new HashSet<>();
        if (!Objects.isNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
            bundle.getEntry().forEach(resource -> {
                DiseaseDTO diseaseDTO = fhirAssessmentMapper.mapToDiagnosis((Condition) resource.getResource(),
                        isMedicalReviewSummary, request.getType());
                if (includeAllCategories || !diseaseCategory.contains(diseaseDTO.getDiseaseCategory())) {
                    diseaseCategory.add(diseaseDTO.getDiseaseCategory());
                    diseases.add(diseaseDTO);
                }
            });
        }
        return diseases;
    }

    /**
     * Gets patient conditions.
     *
     * @param patientId .
     * @return Map<String, Condition>
     */
    private Map<String, Condition> getPatientConditions(String patientId, String type) {
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.CONDITION_QUERY, patientId, type));
        Map<String, Condition> patientConditions = new HashMap<>();
        if (!Objects.isNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
            bundle.getEntry().forEach(resource -> {
                Condition condition = (Condition) resource.getResource();
                patientConditions.put(condition.getCode().getText(), condition);
            });
        }
        return patientConditions;
    }

    /**
     * {@inheritDoc}
     */
    public void setPatientReferenceInEncounterDetails(EncounterDetailsDTO encounterDetails, Bundle bundle) {
        if (Objects.isNull(encounterDetails.getPatientReference()) || Objects.isNull(encounterDetails.getMemberId())) {
            Map<String, String> patientRefDetails = createPatient(encounterDetails.getPatientId(),
                    bundle, encounterDetails.getProvenance());
            encounterDetails.setMemberId(patientRefDetails.get(Constants.MEMBER_ID));
            encounterDetails.setPatientReference(patientRefDetails.get(String.valueOf(ResourceType.Patient)));
        } else {
            encounterDetails.setPatientReference(StringUtil.concatString(String.valueOf(ResourceType.Patient),
                    Constants.FORWARD_SLASH,
                    encounterDetails.getPatientReference()));
        }
    }

    /**
     * {@inheritDoc}
     */
    public String createOrUpdateMedicalReviewEncounter(String encounterId, EncounterDetailsDTO encounterDetailsDTO,
                                                       String encounterType, String partOfEncounter, Bundle bundle) {
        String encounterReference;
        if (StringUtils.isEmpty(encounterId)) {
            encounterReference = fhirAssessmentMapper.createEncounter(encounterDetailsDTO, bundle,
                    encounterType, partOfEncounter);
        } else {
            fhirAssessmentMapper.updateEncounter(encounterDetailsDTO, bundle,
                    encounterType, partOfEncounter);
            encounterReference = StringUtil.concatString(Constants.FHIR_RESOURCE_ENCOUNTER,
                    Constants.FORWARD_SLASH, encounterId);
        }
        encounterDetailsDTO.setId(encounterReference);
        return encounterReference;
    }

    /**
     * Updates diagnosis status.
     *
     * @param condition
     * @param diagnosis
     * @param bundle
     */
    private void updateDiagnosisStatus(Condition condition, DiagnosisDTO diagnosis, Bundle bundle) {
        CodeableConcept codeableConcept = new CodeableConcept();
        Coding coding = new Coding(Constants.CONDITION_CLINCAL_STATUS_SYSTEM, Constants.CLINCAL_STATUS_CODE_INACTIVE, Constants.CLINCAL_STATUS_DISPLAY_INACTIVE);
        codeableConcept.setCoding(List.of(coding));
        condition.setClinicalStatus(codeableConcept);

        String id = fhirUtils.getIdFromHistoryUrl(condition.getId());
        fhirUtils.setBundle(StringUtil.concatString(String.valueOf(ResourceType.Condition), Constants.FORWARD_SLASH, id),
                StringUtil.concatString(Constants.FHIR_BASE_URL, id),
                Bundle.HTTPVerb.PUT, condition, bundle, diagnosis.getProvenance());
    }

    /**
     * {@inheritDoc}
     */
    public List<PregnancyInfo> getPregnancyInfoByVillages(RequestDTO request) {
        List<PregnancyInfo> pregnancyInfoList = new ArrayList<>();
        List<String> memberIds = householdService.getHouseholdMemberByVillagesWithPatientVitals(request.getVillageIds(),
                request.getLastSyncTime(), request.getCurrentSyncTime(), List.of(Constants.ID), request.getSkip(), request.getLimit());
        if (!memberIds.isEmpty()) {
            Map<String, PregnancyInfo> pregnancyInfoMap = getPregnancyInfoFromPatientVitals(memberIds);
            pregnancyInfoMap.entrySet().forEach(pregnancyInfoEntry -> {
                PregnancyInfo pregnancyInfo = pregnancyInfoEntry.getValue();
                pregnancyInfo.setHouseholdMemberId(pregnancyInfoEntry.getKey());
                pregnancyInfoList.add(pregnancyInfo);
            });
        }
        return pregnancyInfoList;
    }

    /**
     * Get patient vitals information from member
     *
     * @param request - request data
     * @return Pregnancy Info map
     */
    public PregnancyInfo getPatientVitals(RequestDTO request) {
        Map<String, PregnancyInfo> pregnancyInfo = getPregnancyInfoFromPatientVitals(List.of(request.getMemberId()));
        return pregnancyInfo.get(request.getMemberId());
    }

    /**
     * Get pregnancy information from patient vital observations
     *
     * @param memberIds List of member Ids
     * @return Pregnancy Info map
     */
    public Map<String, PregnancyInfo> getPregnancyInfoFromPatientVitals(List<String> memberIds) {
        Map<String, PregnancyInfo> pregnancyInfoMap = new HashMap<>();
        memberIds.forEach(memberId -> pregnancyInfoMap.put(memberId, new PregnancyInfo()));
        Bundle observationBundle = getPatientVitalObservations(memberIds);
        observationBundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(Observation.class::isInstance)
                .map(Observation.class::cast)
                .forEach(observation -> {
                    String observationCode = observation.getCode().getText();
                    String memberId = fhirUtils.getIdFromReference(observation.getPerformer().getFirst().getReference());
                    PregnancyInfo pregnancyInfo = pregnancyInfoMap.get(memberId);
                    switch (observationCode) {
                        case Constants.ANC_VISIT_NUMBER:
                            pregnancyInfo.setAncVisitNo(observation.getValueIntegerType().getValue());
                            break;
                        case Constants.WEIGHT:
                            pregnancyInfo.setWeight(observation.getValueQuantity().getValue().doubleValue());
                            break;
                        case Constants.LAST_MENSTRUAL_PERIOD:
                            pregnancyInfo.setLastMenstrualPeriod(observation.getValueDateTimeType().getValue());
                            break;
                        case Constants.ESTIMATED_DELIVERY_DATE:
                            pregnancyInfo.setEstimatedDeliveryDate(observation.getValueDateTimeType().getValue());
                            break;
                        case Constants.PNC_VISIT_NUMBER:
                            pregnancyInfo.setPncVisitNo(observation.getValueIntegerType().getValue());
                            break;
                        case Constants.DATE_OF_DELIVERY:
                            String type = observation.getIdentifier().stream()
                                    .filter(identifier -> FhirIdentifierConstants.TYPE_SYSTEM_URL.equals(identifier.getSystem()))
                                    .map(Identifier::getValue).findFirst().orElse(Constants.EMPTY_SPACE);
                            pregnancyInfo.setIsDeliveryAtHome(Constants.ASSESSMENT.equals(type));
                            pregnancyInfo.setDateOfDelivery(observation.getValueDateTimeType().getValue());
                            break;
                        case Constants.PNC_CREATED_DATE:
                            pregnancyInfo.setPncCreatedDate(observation.getValueDateTimeType().getValue());
                            break;
                        case Constants.NO_OF_NEONATES:
                            pregnancyInfo.setNoOfNeonates(observation.getValueIntegerType().getValue());
                            break;
                        case Constants.NEONATE_OUTCOME:
                            pregnancyInfo.setNeonateOutcome(observation.getValueStringType().getValue());
                            break;
                        case Constants.NEONATE_PATIENT_ID:
                            pregnancyInfo.setNeonatePatientId(observation.getValueStringType().getValue());
                            break;
                        case Constants.CHILDHOOD_VISIT_NUMBER:
                            pregnancyInfo.setChildVisitNo(observation.getValueIntegerType().getValue());
                            break;
                        case Constants.PREGNANCY_ANC_MEDICAL_REVIEW:
                            pregnancyInfo.setAncMedicalReviewVisitNo(observation.getValueIntegerType().getValue());
                            break;
                        case Constants.PNC_MOTHER_MEDICAL_REVIEW:
                            pregnancyInfo.setPncMotherMedicalReviewVisitNo(observation.getValueIntegerType().getValue());
                            break;
                        case Constants.NEONATE_DEATH_RECORDED_BY_PHU:
                            pregnancyInfo.setIsNeonateDeathRecordedByPHU(observation.getValueBooleanType().getValue());
                            break;
                        default:
                            break;
                    }
                });
        return pregnancyInfoMap;
    }

    /**
     * Get patient vital observations by member Ids
     *
     * @param memberIds Member Ids
     * @return - Bundle Object
     */
    private Bundle getPatientVitalObservations(List<String> memberIds) {
        memberIds = memberIds.stream().map(memberId ->
                StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, memberId)).toList();
        String url = String.format(Constants.PATIENT_VITAL_OBSERVATION_LIST_PARAMS, FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                Constants.PATIENT_VITAL_INFORMATION, String.join(Constants.COMMA, memberIds));
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * {@inheritDoc}
     */
    public Patient getPatientById(String patientId) {
        Patient patient = null;
        Bundle bundle = getPatientDetailsByPatientId(patientId);
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            patient = (Patient) entry.getResource();
        }
        return patient;
    }


    public void deletePatientByPatientId(RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getPatientId())) {
            throw new DataNotAcceptableException(2002);
        }
        Bundle patientBundle = restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_PATIENT_REFERENCE, requestDTO.getPatientId()));
        Patient patient = (Patient) patientBundle.getEntry().getFirst().getResource();
        Bundle relatedPersonBundle = restApiUtil.getBatchRequest(String.format(Constants.GET_MEMBER_ID, patient.getLink().get(Constants.ZERO).getOther().getReferenceElement().getIdPart()));
        RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();

        if (!Objects.isNull(requestDTO.getReason())) {
            patient.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_DELETE_REASON_IDENTIFIER_URL)
                    .setValue(Objects.nonNull(requestDTO.getOtherReason()) ? StringUtil.concatString(Constants.OTHER ,
                            Constants.EMPTY_SPACE,  Constants.HYPHEN, Constants.EMPTY_SPACE,
                            requestDTO.getOtherReason()) : requestDTO.getReason());
            if(Constants.PATIENT_DEAD.equals(requestDTO.getReason())) {
                patient.setDeceased(new BooleanType(Boolean.TRUE));
            }
        }
        patient.setActive(Boolean.FALSE);
        relatedPerson.setActive(Boolean.FALSE);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        fhirUtils.setBundleUsingId(patient.getIdPart(),
                Constants.EMPTY_SPACE,
                Bundle.HTTPVerb.PUT,
                patient,
                bundle,
                requestDTO.getProvenance());
        fhirUtils.setBundleUsingId(relatedPerson.getIdPart(),
                Constants.EMPTY_SPACE,
                Bundle.HTTPVerb.PUT,
                relatedPerson,
                bundle,
                requestDTO.getProvenance());
    }


    /**
     * {@inheritDoc}
     */
    public String createPatientByPatientId(RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getPatientId())) {
            throw new DataNotAcceptableException(2002);
        }
        Bundle bundle = new Bundle().setType(BundleType.TRANSACTION);
        Map<String, String> patientRefDetails = createPatient(requestDTO.getPatientId(),
                bundle, requestDTO.getProvenance());
        if (bundle.getEntry().isEmpty()) {
            return fhirUtils.getIdFromResourceUrl(patientRefDetails.get(String.valueOf(ResourceType.Patient)));
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        return response.get(String.valueOf(ResourceType.Patient)).get(Constants.ZERO);
    }

    /**
     * {@inheritDoc}
     */
    public void updatePatientStatus(Bundle bundle, Boolean pregnancyStatus, ProvenanceDTO provenanceDTO,
                                    String patientId, Boolean isActive) {
        Patient patient = null;
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (entry.getResource() instanceof Patient patientResource) {
                patient = patientResource;
                if (Objects.nonNull(pregnancyStatus)) {
                    patient = (Patient) fhirAssessmentMapper.setPregnancyStatus(pregnancyStatus, patient);
                }
                if (Objects.nonNull(isActive)) {
                    patient.setActive(isActive);
                }
            }
        }
        if (Objects.isNull(patient)) {
            patient = getPatientById(patientId);
            if (Objects.nonNull(patient)) {
                if (Objects.nonNull(pregnancyStatus)) {
                    patient = (Patient) fhirAssessmentMapper.setPregnancyStatus(pregnancyStatus, patient);
                }
                if (Objects.nonNull(isActive)) {
                    patient.setActive(isActive);
                }
                fhirUtils.setBundleUsingId(patient.getIdPart(),
                        Constants.EMPTY_SPACE,
                        Bundle.HTTPVerb.PUT,
                        patient,
                        bundle,
                        provenanceDTO);
            }
        }
        RelatedPerson relatedPerson = householdService.getRelatedPersonByPatientId(patientId);
        if (Objects.nonNull(pregnancyStatus)) {
            relatedPerson = (RelatedPerson) fhirAssessmentMapper.setPregnancyStatus(pregnancyStatus, relatedPerson);
        }
        if (Objects.nonNull(isActive)) {
            relatedPerson.setActive(isActive);
        }
        fhirUtils.setBundleUsingId(relatedPerson.getIdPart(),
                Constants.EMPTY_SPACE,
                Bundle.HTTPVerb.PUT,
                relatedPerson,
                bundle,
                provenanceDTO);
    }

    /**
     * Get active service request bundle by member id
     *
     * @param requestDTO Request Object
     * @return Bundle Object
     */
    private Bundle getActiveServiceRequestByMemberId(RequestDTO requestDTO) {
        List<String> statuses = Objects.nonNull(requestDTO.getTicketStatuses()) ? requestDTO.getTicketStatuses() :
                List.of(Constants.ACTIVE, Constants.DRAFT, Constants.ON_HOLD);
        String url = String.format(Constants.GET_SERVICE_REQUEST_BY_PERFORMER,
                StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH,
                        requestDTO.getMemberId()),
                String.join(Constants.COMMA, statuses));
        if (Objects.nonNull(requestDTO.getTicketType())) {
            url = StringUtil.concatString(url, String.format(Constants.SERVICE_REQUEST_STATUS_REQUISITION_QUERY,
                    FhirIdentifierConstants.TICKET_TYPE_SYSTEM_URL, requestDTO.getTicketType()));
        }
        if (Objects.nonNull(requestDTO.getPriority())) {
            url = StringUtil.concatString(url,
                    String.format(Constants.PRIORITY_QUERY_PARAMS, requestDTO.getPriority()));
        }
        if (Objects.nonNull(requestDTO.getCategory())) {
            url = StringUtil.concatString(url,
                    String.format(Constants.IDNTIFIER_WHERE, FhirIdentifierConstants.TICKET_CATEGORY_SYSTEM_URL,
                            requestDTO.getCategory()));
        }
        if (Objects.nonNull(requestDTO.getEncounterType())) {
            url = StringUtil.concatString(url,
                    String.format(Constants.IDNTIFIER_WHERE, FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL,
                            requestDTO.getEncounterType()));
        }
        return restApiUtil.getBatchRequest(url);
    }

    /**
     * Create Referral Ticket  for Recovered status
     *
     * @param requestDTO Request Details
     * @param bundle     Bundle Object
     */
    private String createReferralTicketForRecovered(RequestDTO requestDTO, Bundle bundle) {
        String uuidServiceRequest = fhirUtils.getUniqueId();
        String id = StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL, uuidServiceRequest);
        ReferralDetailsDTO referralDetailsDTO = new ReferralDetailsDTO();
        referralDetailsDTO.setEncounterType(requestDTO.getClosedEncounterType());
        referralDetailsDTO.setType(requestDTO.getTicketType());
        referralDetailsDTO.setPatientReference(requestDTO.getPatientReference());
        referralDetailsDTO.setCategory(requestDTO.getCategory());
        referralDetailsDTO.setMemberId(requestDTO.getMemberId());
        referralDetailsDTO.setPatientStatus(requestDTO.getPatientStatus());
        referralDetailsDTO.setProvenance(requestDTO.getProvenance());
        referralDetailsDTO.setReferredReason(requestDTO.getClosedReason());
        ServiceRequest serviceRequest = fhirAssessmentMapper.createReferralTicket(referralDetailsDTO);
        fhirUtils.setBundle(id, StringUtil.concatString(Constants.FHIR_BASE_URL, uuidServiceRequest),
                Bundle.HTTPVerb.POST, serviceRequest, bundle, referralDetailsDTO.getProvenance());
        return id;
    }

    /**
     * {@inheritDoc}
     */
    public void updateReferralTicketByMemberId(RequestDTO requestDTO, Bundle bundle) {
        Bundle referralBundle = Objects.isNull(bundle) ? new Bundle().setType(BundleType.TRANSACTION) : bundle;
        String uuid = fhirUtils.getUniqueId();
        Bundle serviceRequestBundle = getActiveServiceRequestByMemberId(requestDTO);
        if (Constants.MEDICAL_REVIEW.equalsIgnoreCase(requestDTO.getTicketType()) &&
                requestDTO.getPatientStatus().toLowerCase().contains(Constants.RECOVERED.toLowerCase()) &&
                !Constants.SKIP_STATUS_CATEGORY.contains(requestDTO.getCategory())) {
            String status = fhirAssessmentMapper.extractStatus(requestDTO.getPatientStatus(),
                    List.of(Constants.RECOVERED));
            updatePatientOverallStatus(referralBundle, status, requestDTO.getProvenance(), requestDTO.getMemberId());
        }
        String recoveredTicketId = (!serviceRequestBundle.getEntry().isEmpty() &&
                Constants.RECOVERED.toLowerCase().contains(requestDTO.getPatientStatus().toLowerCase())) ?
                createReferralTicketForRecovered(requestDTO, referralBundle) : null;
        AtomicBoolean isPreviousStatus = new AtomicBoolean(Boolean.FALSE);
        serviceRequestBundle.getEntry().forEach(entry -> {
            ServiceRequest serviceRequest = (ServiceRequest) entry.getResource();
            String id = fhirUtils.getIdFromHistoryUrl(serviceRequest.getId());
            String instructions = serviceRequest.getPatientInstruction();
            if (Objects.nonNull(requestDTO.getReason())) {
                List<String> reasons = Arrays.stream(requestDTO.getReason().split(Constants.COMMA)).map(String::trim)
                        .sorted().toList();
                if (Objects.nonNull(instructions) && !reasons.equals(
                        Arrays.stream(instructions.split(Constants.COMMA)).map(String::trim).sorted().toList())) {
                    return;
                }
            }
            serviceRequest.getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL.equals(
                            identifier.getSystem()))
                    .forEach(identifier -> identifier.setValue(requestDTO.getPatientStatus()));
            if (requestDTO.getPatientStatus().toLowerCase().contains(Constants.RECOVERED.toLowerCase()) ||
                    Boolean.TRUE.equals(requestDTO.getCloseReferralTicket())) {
                if (Boolean.TRUE.equals(requestDTO.getUpdatePriorityStatus()) && Boolean.FALSE.equals(isPreviousStatus.get())
                        && Objects.nonNull(serviceRequest.getPriority()) && Constants.URGENT.equals(serviceRequest.getPriority().toCode())) {
                    updatePreviousServiceRequest(requestDTO.getMemberId(), requestDTO.getCategory(),
                            referralBundle, requestDTO.getProvenance());
                    isPreviousStatus.set(Boolean.TRUE);
                }
                serviceRequest.setStatus(ServiceRequest.ServiceRequestStatus.COMPLETED);
                serviceRequest.setPriority(ServiceRequest.ServiceRequestPriority.ROUTINE);
                if (Constants.AUTO_CLOSE_REASONS.contains(requestDTO.getClosedReason())) {
                    serviceRequest.getNote().add(new Annotation().setText(Constants.AUTO_CLOSED_TICKET)
                            .setTime(requestDTO.getProvenance().getModifiedDate()));
                }
                serviceRequest.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_CURRENT_STATUS_CLOSED_REASON)
                        .setValue(requestDTO.getClosedReason());
                if (!Objects.isNull(requestDTO.getEncounterId())) {
                    serviceRequest.addSupportingInfo(new Reference(
                            !requestDTO.getEncounterId().contains(ResourceType.Encounter.toString()) ?
                                    StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH,
                                            requestDTO.getEncounterId()) : requestDTO.getEncounterId()));
                }

                if (!Objects.isNull(recoveredTicketId)) {
                    serviceRequest.addSupportingInfo(new Reference(recoveredTicketId));
                }
            }

            fhirUtils.setBundle(
                    StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH, id),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, uuid, id), Bundle.HTTPVerb.PUT, serviceRequest, referralBundle,
                    requestDTO.getProvenance());
            if (Objects.nonNull(bundle)) {
                return;
            }
            if (!referralBundle.getEntry().isEmpty()) {
                restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(referralBundle));
            }

        });
    }

    /**
     * Check Active referral Ticket exists or not for update
     * Patient status
     *
     * @param memberId member Id
     * @return Exists or not
     */
    private boolean checkActiveTicketStatus(String memberId) {
        Bundle bundle = restApiUtil.getBatchRequest(
                String.format(Constants.GET_ACTIVE_STATUS_SERVICE_REQUEST, memberId, Constants.ACTIVE, FhirIdentifierConstants.TICKET_CATEGORY_SYSTEM_URL, StringUtil.concatString(Constants.ICCM, Constants.COMMA, Constants.RMNCH, Constants.COMMA, Constants.CHILDHOOD_VISIT)));
        return bundle.getTotal() > Constants.ZERO;
    }

    /**
     * Check on Treatment referral Ticket exists or not for update
     * Patient status
     *
     * @param memberId member Id
     * @return Exists or not
     */
    private boolean checkOnTreatmentStatus(String memberId) {
        Bundle bundle = restApiUtil.getBatchRequest(
                String.format(Constants.GET_ACTIVE_STATUS_SERVICE_REQUEST, memberId, Constants.ON_HOLD, FhirIdentifierConstants.TICKET_CATEGORY_SYSTEM_URL, StringUtil.concatString(Constants.ICCM, Constants.COMMA, Constants.RMNCH, Constants.COMMA, Constants.CHILDHOOD_VISIT)));
        return bundle.getTotal() > Constants.ZERO;
    }

    /**
     * Update previous service request for medical review tickets
     *
     * @param memberId      - Member ID
     * @param category      - Category
     * @param bundle        - Bundle
     * @param provenanceDTO - Provenance
     */
    private void updatePreviousServiceRequest(String memberId, String category, Bundle bundle, ProvenanceDTO provenanceDTO) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPriority(Constants.ROUTINE);
        requestDTO.setCategory(category);
        requestDTO.setMemberId(memberId);
        requestDTO.setTicketType(Constants.MEDICAL_REVIEW);
        requestDTO.setProvenance(provenanceDTO);
        Bundle serviceBundle = getActiveServiceRequestByMemberId(requestDTO);
        ServiceRequest serviceRequest = !serviceBundle.getEntry().isEmpty() ? (ServiceRequest) serviceBundle.getEntry().getFirst().getResource() : null;
        if (Objects.nonNull(serviceRequest)) {
            String uuid = fhirUtils.getUniqueId();
            String id = fhirUtils.getIdFromHistoryUrl(serviceRequest.getId());
            serviceRequest.setPriority(ServiceRequest.ServiceRequestPriority.URGENT);
            Identifier patientStatusIdentifier = serviceRequest.getIdentifier().stream()
                    .filter(identifier -> FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(
                            identifier.getSystem())).findFirst().orElse(null);
            if (Objects.nonNull(patientStatusIdentifier)) {
                serviceRequest.getIdentifier().stream()
                        .filter(identifier -> FhirIdentifierConstants.PATIENT_CURRENT_STATUS_SYSTEM_URL.equals(
                                identifier.getSystem()))
                        .forEach(identifier -> identifier.setValue(patientStatusIdentifier.getValue()));
            }
            fhirUtils.setBundle(
                    StringUtil.concatString(String.valueOf(ResourceType.ServiceRequest), Constants.FORWARD_SLASH, id),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, uuid, id), Bundle.HTTPVerb.PUT, serviceRequest, bundle,
                    requestDTO.getProvenance());
        }
    }

    /**
     * Gets pregnant patients from related person.
     *
     * @param pregnancyUrl url of pregnant patients.
     * @return Returns List of pregnant patients.
     */
    private List<PatientDTO> getPregnantPatients(String pregnancyUrl) {
        return getPatientListByUrl(pregnancyUrl).getEntry().stream()
                .filter(entry -> ResourceType.RelatedPerson.equals(entry.getResource().getResourceType()))
                .map(entry -> {
                    RelatedPerson relatedPerson = (RelatedPerson) entry.getResource();
                    return fhirMapper.relatedPersonToPatient(relatedPerson, new PatientDTO());
                })
                .toList();
    }

    /**
     * Gets pregnant patients count from related person.
     *
     * @param villageIdentifiers village identifier of the pregnant patients.
     * @return Returns count of the pregnant patients.
     */
    private Integer getPregnantPatientCount(List<String> villageIdentifiers) {
        String url = StringUtil.concatString(Constants.PREGNANT_TOTAL_PARAMS, String.format(Constants.PATIENT_IDENTIFIER,
                String.join(Constants.COMMA, villageIdentifiers)));
        Bundle bundle = getPatientListByUrl(url);
        return bundle.getTotal();
    }

    /**
     * Close previous childhood details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    public void closeChildhoodDetails(Bundle bundle, RequestDTO requestDTO) {
        fhirAssessmentMapper.updateVitalObservationsStatus(List.of(Constants.CHILDHOOD_VISIT_NUMBER), bundle,
                requestDTO.getMemberId(), requestDTO.getProvenance());
        requestDTO.setCategory(StringUtil.concatString(Constants.CHILDHOOD_VISIT, Constants.COMMA, Constants.CHILD_VISIT));
        closeRmnchReferralTickets(requestDTO, bundle, null);
    }

    /**
     * Close previous PNC details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    public void closePncDetails(Bundle bundle, RequestDTO requestDTO) {
        fhirAssessmentMapper.updateVitalObservationsStatus(!requestDTO.isAncStarted() ?
                        List.of(Constants.PNC_VISIT_NUMBER, Constants.PNC_CREATED_DATE, Constants.PNC_MOTHER_MEDICAL_REVIEW, Constants.NO_OF_NEONATES,
                                Constants.NEONATE_PATIENT_ID, Constants.NEONATE_OUTCOME, Constants.NEONATE_DEATH_RECORDED_BY_PHU) :
                        List.of(Constants.PNC_VISIT_NUMBER, Constants.PNC_CREATED_DATE, Constants.PNC_MOTHER_MEDICAL_REVIEW, Constants.NO_OF_NEONATES,
                                Constants.NEONATE_PATIENT_ID, Constants.DATE_OF_DELIVERY, Constants.NEONATE_OUTCOME,
                                Constants.NEONATE_DEATH_RECORDED_BY_PHU), bundle, requestDTO.getMemberId(),
                requestDTO.getProvenance());
        requestDTO.setCategory(StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.RMNCH_VISIT));
        closeRmnchReferralTickets(requestDTO, bundle, null);
    }

    /**
     * Close previous PNC Neonate details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    public void closePncNeonateDetails(Bundle bundle, RequestDTO requestDTO) {
        requestDTO.setCategory(StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.RMNCH_VISIT));
        closeRmnchReferralTickets(requestDTO, bundle, Constants.PNC_NEONATE);
    }

    /**
     * Close previous ANC details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    public void closeAncDetails(Bundle bundle, RequestDTO requestDTO) {
        fhirAssessmentMapper.updateVitalObservationsStatus(List.of(Constants.LAST_MENSTRUAL_PERIOD, Constants.ANC_VISIT_NUMBER, Constants.ESTIMATED_DELIVERY_DATE,
                        Constants.PREGNANCY_ANC_MEDICAL_REVIEW), bundle, requestDTO.getMemberId(),
                requestDTO.getProvenance());
        requestDTO.setCategory(StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.RMNCH_VISIT));
        closeRmnchReferralTickets(requestDTO, bundle, null);
    }

    /**
     * Close RMNCH referral Ticket by memberId
     *
     * @param encounterType Encounter details
     * @param bundle        Bundle object
     */
    private void closeRmnchReferralTickets(RequestDTO requestDTO, Bundle bundle, String encounterType) {
        requestDTO.setReason(null);
        requestDTO.setPatientStatus(
                Constants.MISCARRIAGE.equals(requestDTO.getClosedReason()) ? Constants.REFERRED : Constants.RECOVERED);
        requestDTO.setEncounterType(encounterType);
        requestDTO.setTicketType(
                StringUtil.concatString(Constants.MEDICAL_REVIEW, Constants.COMMA, Constants.ASSESSMENT));
        updateReferralTicketByMemberId(requestDTO, bundle);
    }

    /**
     * Close All referral tickets
     *
     * @param bundle         Bundle Object
     * @param memberId       MemberId
     * @param encounterId    EncounterId
     * @param provenance     provenanceDetails
     * @param assessmentType assessmentType
     */
    public void handlePatientDeath(String assessmentType, Bundle bundle, String memberId, String encounterId,
                                   ProvenanceDTO provenance) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(memberId);
        requestDTO.setEncounterId(encounterId);
        requestDTO.setProvenance(provenance);
        requestDTO.setPatientStatus(Constants.RECOVERED);
        requestDTO.setCategory(StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.ICCM,
                Constants.COMMA, Constants.CHILDHOOD_VISIT, Constants.COMMA, Constants.RMNCH_VISIT, Constants.COMMA,
                Constants.CHILD_VISIT));
        requestDTO.setTicketType(
                StringUtil.concatString(Constants.MEDICAL_REVIEW, Constants.COMMA, Constants.ASSESSMENT));
        requestDTO.setClosedEncounterType(assessmentType);
        requestDTO.setClosedReason(Constants.PATIENT_DEAD);
        requestDTO.setCloseReferralTicket(Boolean.TRUE);
        updateReferralTicketByMemberId(requestDTO, bundle);
        fhirAssessmentMapper.updateVitalObservationsStatus(
                List.of(Constants.LAST_MENSTRUAL_PERIOD, Constants.ANC_VISIT_NUMBER, Constants.ESTIMATED_DELIVERY_DATE,
                        Constants.PREGNANCY_ANC_MEDICAL_REVIEW, Constants.PNC_VISIT_NUMBER, Constants.DATE_OF_DELIVERY, Constants.PNC_MOTHER_MEDICAL_REVIEW,
                        Constants.NO_OF_NEONATES, Constants.NEONATE_PATIENT_ID, Constants.CHILDHOOD_VISIT_NUMBER,
                        Constants.NEONATE_OUTCOME, Constants.PNC_CREATED_DATE), bundle, requestDTO.getMemberId(),
                requestDTO.getProvenance());
    }

    /*
     * Update patient overall status
     *
     * @param bundle          Bundle object
     * @param patientStatus   Patient status
     * @param provenanceDTO   Provenance details
     * @param memberId Patient reference
     */
    public void updatePatientOverallStatus(Bundle bundle, String patientStatus, ProvenanceDTO provenanceDTO,
                                           String memberId) {
        Patient patient = null;
        for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
            if (entry.getResource() instanceof Patient patientResource) {
                patient = patientResource;
                setPatientOverallStatus(patient.getIdentifier(), patientStatus);
            }
        }
        if (Objects.isNull(patient)) {
            patient = fetchPatientByMemberId(memberId);
            if (Objects.nonNull(patient)) {
                setPatientOverallStatus(patient.getIdentifier(), patientStatus);
                fhirUtils.setBundleUsingId(patient.getIdPart(), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT, patient, bundle,
                        provenanceDTO);
            }
        }
    }

    /**
     * Get patient details by patient reference
     *
     * @param memberId patient reference
     * @return Patient
     */
    private Patient fetchPatientByMemberId(String memberId) {
        if (Objects.nonNull(memberId)) {
            Bundle patientBundle =
                    restApiUtil.getBatchRequest(String.format(Constants.GET_PATIENT_BY_MEMBER_ID, memberId));
            if (Objects.nonNull(patientBundle) && !patientBundle.getEntry().isEmpty()) {
                return (Patient) patientBundle.getEntry().getFirst().getResource();
            }
        }
        return null;
    }

    /**
     * Get patient details by patient reference
     *
     * @param identifiers   identifiers
     * @param patientStatus patient status
     */
    private void setPatientOverallStatus(List<Identifier> identifiers, String patientStatus) {
        if (identifiers.stream().noneMatch(
                identifier -> identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL))) {
            identifiers.add(new Identifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL)
                    .setValue(patientStatus));
        } else {
            if (patientStatus != null) {
                for (Identifier identifier : identifiers) {
                    if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())) {
                        identifier.setValue(patientStatus);
                    }
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> searchPatients(PatientRequestDTO requestDTO) {
        String url = constructSearchURL(requestDTO);
        Map<String, Object> response = new HashMap<>();
        if (Constants.ZERO == requestDTO.getSkip()) {
            response.put(Constants.TOTAL_COUNT, getCount(url));
        }
        List<PatientDetailsDTO> searchPersonDetails = new ArrayList<>();
        if (!response.containsKey(Constants.TOTAL_COUNT) ||
                !response.get(Constants.TOTAL_COUNT).equals(Constants.ZERO)) {
            if (Objects.isNull(requestDTO.getSort())) {
                url = StringUtil.concatString(url, Constants.LAST_UPDATED_DESC_SORT);
            } else {
                url = addSortInURL(url, requestDTO);
                url = url.replace(ResourceType.Patient.toString(), ResourceType.RelatedPerson.toString())
                        .replace(RelatedPerson.SP_PATIENT, Constants.SP_RELATED_PERSON);
                url = url.replace(StringUtil.concatString(Constants.SP_RELATED_PERSON, Constants.HYPHEN, Constants.STATUS),
                        Constants.SP_PATIENT_STATUS);
            }
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.PAGINATE_PARAMS,
                    requestDTO.getLimit(), requestDTO.getSkip()));
            Bundle bundle = restApiUtil.getBatchRequest(url);
            SearchPersonDetailsDTO personDetailsDTO;
            for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
                personDetailsDTO = processSearchPatients(component);
                if (Objects.nonNull(personDetailsDTO.getRelatedPerson())) {
                    searchPersonDetails.add(
                            searchPersonDetailsConverter.convertToPatientDetails(personDetailsDTO.getRelatedPerson(),
                                    new PatientDetailsDTO()));
                } else if (Objects.nonNull(personDetailsDTO.getPatient())) {
                    searchPersonDetails.add(
                            patientConverter.convertToPatientDetails(personDetailsDTO.getPatient(),
                                    new PatientDetailsDTO()));
                }
            }
            if (searchPersonDetails.isEmpty()) {
                Logger.logInfo("No mapping related person found for search Id" + requestDTO.getSearchText());
            }
        }
        response.put(Constants.PATIENT_LIST, searchPersonDetails);
        response.put(Constants.REFERENCE_PATIENT_ID, null);
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> listPatients(PatientRequestDTO requestDTO) {
        String url = constructListURL(requestDTO);
        Map<String, Object> response = new HashMap<>();
        if (requestDTO.getSkip() == Constants.ZERO) {
            response.put(Constants.TOTAL_COUNT, getCount(url));
        }
        List<PatientDetailsDTO> patientList = new ArrayList<>();
        if (!response.containsKey(Constants.TOTAL_COUNT) ||
                !response.get(Constants.TOTAL_COUNT).equals(Constants.ZERO)) {
            if (Objects.nonNull(requestDTO.getSort())) {
                url = addSortInURL(url, requestDTO);
            } else {
                url = StringUtil.concatString(url, Constants.LAST_UPDATED_DESC_SORT);
            }
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.PAGINATE_PARAMS,
                    requestDTO.getLimit(), requestDTO.getSkip()));
            Bundle bundle = restApiUtil.getBatchRequest(url);
            patientList = addPatients(new ArrayList<>(), bundle);
        }
        response.put(Constants.PATIENT_LIST, patientList);
        return response;
    }

    /**
     * Adds patients from the given FHIR Bundle to the provided patient list.
     *
     * @param patientList the list to which patients will be added
     * @param bundle      the FHIR Bundle containing patient resources
     * @return the updated list of patient details
     */
    private List<PatientDetailsDTO> addPatients(List<PatientDetailsDTO> patientList, Bundle bundle) {
        for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
            if (component.getResource() instanceof Patient patient) {
                patientList.add(patientConverter.convertToPatientDetails(patient, new PatientDetailsDTO()));
            }
        }
        return patientList;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, PatientDetailsDTO> listNcdPatients(PatientRequestDTO requestDTO) {
        String url = constructSearch(requestDTO);
        if (!(Constants.ZERO == requestDTO.getLimit() && Constants.ZERO == requestDTO.getSkip())) {
            url = StringUtil.concatString(url, Constants.AND, String.format(Constants.PAGINATE_PARAMS,
                    requestDTO.getLimit(), requestDTO.getSkip()));
        }
        Bundle bundle = restApiUtil.getBatchRequest(url);
        Map<String, PatientDetailsDTO> patientDetails = new HashMap<>();
        for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
            Patient patient = (Patient) component.getResource();
            patientDetails.put(patient.getIdPart(),
                    patientConverter.convertToPatientDetails(patient, new PatientDetailsDTO()));
        }
        Set<String> patientIds = patientDetails.keySet();
        if (requestDTO.isReferredReasonsRequired() && !patientIds.isEmpty()) {
            url = String.format(Constants.SERVICE_REQUEST_BY_PATIENT_ID_QUERY,
                    String.join(Constants.COMMA, patientDetails.keySet()));
            bundle = restApiUtil.getBatchRequest(url);
            for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
                ServiceRequest serviceRequest = (ServiceRequest) component.getResource();
                PatientDetailsDTO patientDetailsDTO = patientDetails.get(
                        serviceRequest.getSubject().getReference().split(Constants.FORWARD_SLASH)[1]);
                patientDetailsDTO.setReferredReasons(Arrays.stream(
                        serviceRequest.getPatientInstruction().split(Constants.FORWARD_SLASH)).toList());
                patientDetails.put(patientDetailsDTO.getPatientId(), patientDetailsDTO);
            }
        }
        return patientDetails;
    }

    private String constructSearch(PatientRequestDTO requestDTO) {
        String url = StringUtil.concatString(ResourceType.Patient.toString(), Constants.QUESTION_MARK,
                String.format(Constants.ID_WHERE, requestDTO.getId()));
        url = StringUtil.concatString(url, Constants.AND);
        if (Objects.nonNull(requestDTO.getSearchText()) && !requestDTO.getSearchText().isEmpty()) {
            requestDTO.setSearchText(requestDTO.getSearchText().replace(Constants.EMPTY_SPACE, Constants.EMPTY_STRING));
            if (CommonUtil.isAllNumeric(requestDTO.getSearchText())) {
                Logger.logInfo("Search the patient with phone number" + requestDTO.getSearchText());
                url = StringUtil.concatString(url, Constants.SP_NATIONAL_ID_VIRTUAL_ID_PHONE,
                        Constants.PARAMQUALIFIER_STRING_CONTAINS);
            } else if (CommonUtil.isAllAlphabetic(requestDTO.getSearchText())) {
                Logger.logInfo("Search the patient with name" + requestDTO.getSearchText());
                url = StringUtil.concatString(url, Constants.SP_NAME_NATIONAL_ID,
                        Constants.PARAMQUALIFIER_STRING_CONTAINS);
            } else {
                url = StringUtil.concatString(url, Constants.SP_NATIONAL_ID,
                        Constants.PARAMQUALIFIER_STRING_CONTAINS);
            }
            url = StringUtil.concatString(url, Constants.EQUAL_SYMBOL, Constants.SEARCH_TEXT_FILL_UP);
            url = url.replace(Constants.SEARCH_TEXT_FILL_UP, requestDTO.getSearchText());
        }
        return url;
    }

    /**
     * Constructs a search URL based on the given PatientRequestDTO.
     *
     * @param requestDTO the PatientRequestDTO containing search parameters
     * @return the constructed search URL as a String
     */
    private String constructSearchURL(PatientRequestDTO requestDTO) {
        requestDTO.setSearchText(requestDTO.getSearchText().replace(Constants.EMPTY_SPACE, Constants.EMPTY_STRING));
        String url = StringUtil.concatString(ResourceType.RelatedPerson.toString(), Constants.QUESTION_MARK,
                RelatedPerson.SP_ACTIVE, Constants.EQUAL_SYMBOL, String.valueOf(Boolean.TRUE));
        if (Objects.nonNull(requestDTO.getSiteId()) && (Constants.ASSESSMENT.equalsIgnoreCase(requestDTO.getType())
                || Constants.MT_PATIENTS.equalsIgnoreCase(requestDTO.getType())
                || Constants.INVESTIGATION.equalsIgnoreCase(requestDTO.getType())
                || Constants.NUTRITION_LIFESTYLE.equalsIgnoreCase(requestDTO.getType())
                || Constants.DISPENSE.equalsIgnoreCase(requestDTO.getType()))) {
            url = StringUtil.concatString(url, Constants.IDENTIFIER_QUERY);
            List<String> siteIds = Arrays.stream(requestDTO.getSiteId().split(","))
                    .map(id -> StringUtil.concatString(FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL,
                            Constants.VERTICAL_BAR, String.valueOf(id))).toList();
            url = String.format(url, String.join(Constants.COMMA, siteIds));
        }
        if (Objects.nonNull(requestDTO.getCountryId())
                && (Constants.REGISTRATION.equalsIgnoreCase(requestDTO.getType()))) {
            url = StringUtil.concatString(url, String.format(Constants.IDENTIFIER_QUERY,
                    StringUtil.concatString(FhirIdentifierConstants.COUNTRY_ID_SYSTEM_URL,
                            Constants.VERTICAL_BAR, requestDTO.getCountryId())));
        }
        url = getUrlBySearchType(url, requestDTO);
        if (Constants.REGISTRATION.equalsIgnoreCase(requestDTO.getType())) {
            url = StringUtil.concatString(url, Constants.AND,
                    Constants.SP_PATIENT_STATUS,
                    Constants.EQUAL_SYMBOL, FhirConstants.SCREENED, Constants.COMMA,
                    FhirConstants.ASSESSED, Constants.COMMA, FhirConstants.MEDICAL_REVIEWED);
        } else if (Constants.NUTRITION_LIFESTYLE.equalsIgnoreCase(requestDTO.getType())) {
            url = constructUrlForNutrition(url);
        } else if (Constants.INVESTIGATION.equalsIgnoreCase(requestDTO.getType())) {
            url = constructUrlForInvestigation(url);
        } else if (Constants.DISPENSE.equalsIgnoreCase(requestDTO.getType())) {
            url = constructUrlForDispense(url);
        }
        if (Objects.nonNull(requestDTO.getFilter()) || Objects.nonNull(requestDTO.getSort())) {
            if (!Constants.REGISTRATION.equalsIgnoreCase(requestDTO.getType())) {
                if (Objects.nonNull(requestDTO.getFilter())
                        && Constants.ENROLLED.equalsIgnoreCase(requestDTO.getFilter().getEnrollmentStatus())) {
                    url = StringUtil.concatString(url, Constants.AND,
                            Constants.SP_PATIENT_STATUS,
                            Constants.EQUAL_SYMBOL, Constants.ENROLLED);
                } else if (Objects.nonNull(requestDTO.getFilter())
                        && Constants.NOT_ENROLLED.equalsIgnoreCase(requestDTO.getFilter().getEnrollmentStatus())) {
                    url = StringUtil.concatString(url, Constants.AND,
                            Constants.SP_PATIENT_STATUS, Constants.EQUAL_SYMBOL,
                            FhirConstants.SCREENED, Constants.COMMA,
                            FhirConstants.ASSESSED, Constants.COMMA,
                            FhirConstants.MEDICAL_REVIEWED);
                }
            }
            if (Objects.nonNull(requestDTO.getFilter())) {
                url = addFilterInURL(url, requestDTO);
            }
        }
        url = url.replace(Constants.SEARCH_TEXT_FILL_UP, requestDTO.getSearchText());
        if (!Constants.INVESTIGATION.equalsIgnoreCase(requestDTO.getType())
                && !Constants.REGISTRATION.equalsIgnoreCase(requestDTO.getType())) {
            url = url.replace(ResourceType.Patient.toString(), ResourceType.RelatedPerson.toString())
                    .replace(RelatedPerson.SP_PATIENT, Constants.SP_RELATED_PERSON);
            url = url.replace(StringUtil.concatString(Constants.SP_RELATED_PERSON, Constants.HYPHEN, Constants.STATUS),
                    Constants.SP_PATIENT_STATUS);
        }
        return url;
    }

    /**
     * <p>
     * Adds filter parameters to the given URL based on the SearchType.
     * </p>
     *
     * @param url        The URL to which filter parameters will be added.
     * @param requestDTO The PatientRequestDTO containing filter parameters.
     * @return The URL with filter parameters added.
     */
    private String getUrlBySearchType(String url, PatientRequestDTO requestDTO) {
        url = StringUtil.concatString(url, Constants.AND);
        String searchType = Objects.nonNull(requestDTO.getSearchType())
                ? requestDTO.getSearchType().toLowerCase() : Constants.EMPTY_STRING;
        switch (searchType) {
            case RelatedPerson.SP_PHONE:
                url = StringUtil.concatString(url, Constants.SP_PHONE);
                break;
            case Constants.SP_NATIONAL_ID:
                url = StringUtil.concatString(url, Constants.SP_NATIONAL_ID);
                break;
            case Constants.SP_VIRTUAL_ID:
                url = StringUtil.concatString(url, Constants.SP_VIRTUAL_ID);
                break;
            case RelatedPerson.SP_NAME:
                url = StringUtil.concatString(url, RelatedPerson.SP_NAME);
                break;
            default:
                if (CommonUtil.isAllNumeric(requestDTO.getSearchText())) {
                    url = StringUtil.concatString(url, Constants.SP_NATIONAL_ID_VIRTUAL_ID_PHONE,
                            Constants.PARAMQUALIFIER_STRING_CONTAINS);
                } else if (CommonUtil.isAllAlphabetic(requestDTO.getSearchText())) {
                    url = StringUtil.concatString(url, Constants.SP_NAME_NATIONAL_ID,
                            Constants.PARAMQUALIFIER_STRING_CONTAINS);
                } else {
                    url = StringUtil.concatString(url, Constants.SP_NATIONAL_ID,
                            Constants.PARAMQUALIFIER_STRING_CONTAINS);
                }
                break;
        }
        return StringUtil.concatString(url, Constants.EQUAL_SYMBOL, Constants.SEARCH_TEXT_FILL_UP);
    }

    /**
     * <p>
     * Constructs a list URL based on the given PatientRequestDTO.
     * </p>
     *
     * @param requestDTO the PatientRequestDTO containing list parameters
     * @return the constructed list URL as a String
     */
    private String constructListURL(PatientRequestDTO requestDTO) {
        String url =
                StringUtil.concatString(ResourceType.Patient.toString(),
                        Constants.QUESTION_MARK, RelatedPerson.SP_ACTIVE, Constants.EQUAL_SYMBOL, String.valueOf(Boolean.TRUE),
                        Constants.AND, Constants.SP_LIST_PATIENT_REGISTRATION, Constants.EQUAL_SYMBOL,
                        requestDTO.getSiteId());
        if (Constants.NUTRITION_LIFESTYLE.equalsIgnoreCase(requestDTO.getType())) {
            url = constructUrlForNutrition(url);
        } else if (Constants.INVESTIGATION.equalsIgnoreCase(requestDTO.getType())) {
            if (Objects.nonNull(requestDTO.getFilter())
                    && Objects.nonNull(requestDTO.getFilter().getLabTestReferredOn())) {
                Map<String, String> datesForFilter = getDatesFilter(requestDTO.getFilter(), new HashMap<>());
                url = StringUtil.concatString(url,
                        Constants.AND,
                        Constants.PARAM_HAS, Constants.COLON,
                        ResourceType.DiagnosticReport.toString(), Constants.COLON,
                        DiagnosticReport.SP_PATIENT, Constants.COLON,
                        Constants.SP_REGISTERED_ISSUED, Constants.EQUAL_SYMBOL,
                        SearchComparator.GE.toCode(),
                        datesForFilter.get(Constants.LAB_TEST_REFERRED_START_DATE),
                        Constants.AND,
                        Constants.PARAM_HAS, Constants.COLON,
                        ResourceType.DiagnosticReport.toString(), Constants.COLON,
                        DiagnosticReport.SP_PATIENT, Constants.COLON,
                        Constants.SP_REGISTERED_ISSUED, Constants.EQUAL_SYMBOL,
                        SearchComparator.LE.toCode(),
                        datesForFilter.get(Constants.LAB_TEST_REFERRED_END_DATE));
            } else {
                url = constructUrlForInvestigation(url);
            }
        } else if (Constants.DISPENSE.equalsIgnoreCase(requestDTO.getType())) {
            if (Objects.nonNull(requestDTO.getFilter())
                    && Objects.nonNull(requestDTO.getFilter().getPrescriptionReferredOn())) {
                Map<String, String> datesForFilter = getDatesFilter(requestDTO.getFilter(), new HashMap<>());
                url = StringUtil.concatString(url,
                        Constants.AND,
                        Constants.PARAM_HAS, Constants.COLON,
                        ResourceType.MedicationRequest.toString(), Constants.COLON,
                        MedicationRequest.SP_PATIENT, Constants.COLON,
                        Constants.SP_ACTIVE_AUTHORED_ON, Constants.EQUAL_SYMBOL,
                        SearchComparator.GE.toCode(),
                        datesForFilter.get(Constants.PRESCRIPTION_REFERRED_START_DATE),
                        Constants.AND,
                        Constants.PARAM_HAS, Constants.COLON,
                        ResourceType.MedicationRequest.toString(), Constants.COLON,
                        MedicationRequest.SP_PATIENT, Constants.COLON,
                        Constants.SP_ACTIVE_AUTHORED_ON, Constants.EQUAL_SYMBOL,
                        SearchComparator.LE.toCode(),
                        datesForFilter.get(Constants.PRESCRIPTION_REFERRED_END_DATE));
            } else {
                url = constructUrlForDispense(url);
            }
        }
        if (Objects.nonNull(requestDTO.getFilter())) {
            url = addFilterInURL(url, requestDTO);
        }
        return url;
    }

    /**
     * <p>
     * Constructs a URL for nutrition lifestyle based on the given URL.
     * </p>
     *
     * @param url the URL to be modified
     * @return the modified URL as a String
     */
    private String constructUrlForNutrition(String url) {
        url = StringUtil.concatString(url, Constants.AND,
                Constants.PARAM_HAS, Constants.COLON,
                ResourceType.Observation.toString(), Constants.COLON,
                Observation.SP_PATIENT, Constants.COLON,
                Constants.SP_NUTRITION_LIFESTYLE_STATUS, Constants.EQUAL_SYMBOL,
                Observation.ObservationStatus.REGISTERED.toCode());
        return url;
    }

    /**
     * <p>
     * Constructs a URL for dispense based on the given URL.
     * </p>
     *
     * @param url the URL to be modified
     * @return the modified URL as a String
     */
    private String constructUrlForDispense(String url) {
        url = StringUtil.concatString(url, Constants.AND,
                Constants.PARAM_HAS, Constants.COLON,
                ResourceType.MedicationRequest.toString(), Constants.COLON,
                MedicationRequest.SP_PATIENT, Constants.COLON,
                MedicationRequest.SP_STATUS, Constants.EQUAL_SYMBOL,
                MedicationRequest.MedicationRequestStatus.ACTIVE.toCode());
        return url;
    }

    /**
     * <p>
     * Constructs a URL for investigation based on the given URL.
     * </p>
     *
     * @param url the URL to be modified
     * @return the modified URL as a String
     */
    private String constructUrlForInvestigation(String url) {
        url = url.replace(Constants.SP_RELATED_PERSON, DiagnosticReport.SP_PATIENT);
        url = url.replace(ResourceType.RelatedPerson.toString(), ResourceType.Patient.toString());
        url = StringUtil.concatString(url, Constants.AND,
                Constants.PARAM_HAS, Constants.COLON,
                ResourceType.DiagnosticReport.toString(), Constants.COLON,
                DiagnosticReport.SP_PATIENT, Constants.COLON,
                DiagnosticReport.SP_STATUS, Constants.EQUAL_SYMBOL,
                DiagnosticReport.DiagnosticReportStatus.REGISTERED.toCode());
        return url;
    }

    /**
     * Adds filters to the given URL based on the provided PatientRequestDTO.
     *
     * @param url               the URL to which filters will be added
     * @param patientRequestDTO the PatientRequestDTO containing filter parameters
     * @return the updated URL with filters applied
     */
    private String addFilterInURL(String url, PatientRequestDTO patientRequestDTO) {
        Map<String, String> datesForFilter = getDatesFilter(patientRequestDTO.getFilter(), new HashMap<>());
        if (Constants.ENROLLED.equalsIgnoreCase(patientRequestDTO.getFilter().getEnrollmentStatus())) {
            url = url.replace(StringUtil.concatString(Constants.SP_LIST_PATIENT_REGISTRATION),
                    StringUtil.concatString(Constants.SP_PATIENT_STATUS, Constants.EQUAL_SYMBOL,
                            Constants.ENROLLED, Constants.AND, Patient.SP_ORGANIZATION));
        } else if (Constants.NOT_ENROLLED.equalsIgnoreCase(patientRequestDTO.getFilter().getEnrollmentStatus())) {
            url = url.concat(StringUtil.concatString(Constants.AND, Constants.SP_PATIENT_STATUS, Constants.EQUAL_SYMBOL,
                    FhirConstants.SCREENED, Constants.COMMA,
                    FhirConstants.ASSESSED, Constants.COMMA,
                    FhirConstants.MEDICAL_REVIEWED));
        }
        if (Boolean.TRUE.equals(patientRequestDTO.getFilter().getIsRedRiskPatient())) {
            url = url.concat(Constants.FILTER_RED_RISK);
        }
        if (Objects.nonNull(patientRequestDTO.getFilter().getCvdRiskLevel())) {
            url = url.concat(Constants.FILTER_CVD_RISK_LEVEL)
                    .concat(patientRequestDTO.getFilter().getCvdRiskLevel());
        }
        if (Objects.nonNull(patientRequestDTO.getFilter().getAssessmentDate())) {
            url = url.concat(Constants.FILTER_ASSESSMENT_DATE).concat(SearchComparator.GE.toCode())
                    .concat(datesForFilter.get(Constants.ASSESSMENT_START_DATE));
            url = url.concat(Constants.FILTER_ASSESSMENT_DATE).concat(SearchComparator.LE.toCode())
                    .concat(datesForFilter.get(Constants.ASSESSMENT_END_DATE));

        }
        if (Objects.nonNull(patientRequestDTO.getFilter().getMedicalReviewDate())) {
            url = url.concat(Constants.FILTER_MEDICAL_REVIEW_DATE).concat(SearchComparator.GE.toCode())
                    .concat(datesForFilter.get(Constants.MEDICAL_REVIEW_START_DATE));
            url = url.concat(Constants.FILTER_MEDICAL_REVIEW_DATE).concat(SearchComparator.LE.toCode())
                    .concat(datesForFilter.get(Constants.MEDICAL_REVIEW_END_DATE));
        }
        return url;
    }

    /**
     * Adds sorting parameters to the given URL based on the provided PatientRequestDTO.
     *
     * @param url               the URL to which sorting parameters will be added
     * @param patientRequestDTO the PatientRequestDTO containing sorting parameters
     * @return the updated URL with sorting parameters applied
     */
    private String addSortInURL(String url, PatientRequestDTO patientRequestDTO) {
        if (Boolean.TRUE.equals(patientRequestDTO.getSort().getIsHighLowBg())) {
            url = url.replace(ResourceType.Patient.toString(), ResourceType.Observation.toString());
            url = url.replace(ResourceType.RelatedPerson.toString(), ResourceType.Observation.toString());
            url =
                    url.replace(Constants.AND, StringUtil.concatString(Constants.AND, RelatedPerson.SP_PATIENT,
                            Constants.PERIOD)).replace(Constants.QUESTION_MARK,
                            StringUtil.concatString(Constants.QUESTION_MARK,
                                    RelatedPerson.SP_PATIENT,
                                    Constants.PERIOD));
            url = url.concat(Constants.INCLUDE_OBSERVATION_PATIENT);
            url = StringUtil.concatString(url, Constants.AND, Observation.SP_IDENTIFIER, Constants.EQUAL_SYMBOL,
                    FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL, Constants.VERTICAL_BAR,
                    Constants.OBSERVATION_VITAL_SIGNS);
            url = url.concat(Constants.SORT_HIGH_LOW_BG);
        } else if (Boolean.TRUE.equals(patientRequestDTO.getSort().getIsHighLowBp())) {
            url = url.replace(ResourceType.Patient.toString(), ResourceType.Observation.toString());
            url = url.replace(ResourceType.RelatedPerson.toString(), ResourceType.Observation.toString());
            url = url.replace(Constants.AND, StringUtil.concatString(Constants.AND, RelatedPerson.SP_PATIENT,
                    Constants.PERIOD)).replace(Constants.QUESTION_MARK, StringUtil.concatString(Constants.QUESTION_MARK,
                    RelatedPerson.SP_PATIENT,
                    Constants.PERIOD));
            url = url.concat(Constants.INCLUDE_OBSERVATION_PATIENT);
            url = StringUtil.concatString(url, Constants.AND, Observation.SP_IDENTIFIER, Constants.EQUAL_SYMBOL,
                    FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL, Constants.VERTICAL_BAR,
                    Constants.OBSERVATION_VITAL_SIGNS);
            url = url.concat(Constants.SORT_HIGH_LOW_BP);
        } else if (Boolean.TRUE.equals(patientRequestDTO.getSort().getIsAssessmentDueDate())) {
            url = url.replace(ResourceType.Patient.toString(), ResourceType.Appointment.toString());
            url = url.replace(ResourceType.RelatedPerson.toString(), ResourceType.Appointment.toString());
            url = url.replace(Constants.AND, StringUtil.concatString(Constants.AND, RelatedPerson.SP_PATIENT,
                    Constants.PERIOD)).replace(Constants.QUESTION_MARK, StringUtil.concatString(Constants.QUESTION_MARK,
                    RelatedPerson.SP_PATIENT,
                    Constants.PERIOD));
            url =
                    url.concat(StringUtil.concatString(Constants.AND, RelatedPerson.SP_IDENTIFIER,
                            Constants.EQUAL_SYMBOL, FhirIdentifierConstants.APPOINTMENT_TYPE_SYSTEM_URL,
                            Constants.VERTICAL_BAR, Constants.HYPERTENSION_DIAGNOSIS_VALUE));
            url = url.concat(Constants.INCLUDE_APPOINTMENT_PATIENT);
            url = url.concat(Constants.SORT_ASSESSMENT_DATE);
        } else if (Boolean.TRUE.equals(patientRequestDTO.getSort().getIsMedicalReviewDueDate())) {
            url = url.replace(ResourceType.Patient.toString(), ResourceType.Appointment.toString());
            url = url.replace(ResourceType.RelatedPerson.toString(), ResourceType.Appointment.toString());
            url = url.replace(Constants.AND, StringUtil.concatString(Constants.AND, RelatedPerson.SP_PATIENT,
                    Constants.PERIOD)).replace(Constants.QUESTION_MARK, StringUtil.concatString(Constants.QUESTION_MARK,
                    RelatedPerson.SP_PATIENT,
                    Constants.PERIOD));
            url = url.concat(StringUtil.concatString(Constants.AND, RelatedPerson.SP_IDENTIFIER,
                    Constants.EQUAL_SYMBOL, FhirIdentifierConstants.APPOINTMENT_TYPE_SYSTEM_URL,
                    Constants.VERTICAL_BAR, Constants.MEDICAL_REVIEW.toLowerCase()));
            url = url.concat(Constants.INCLUDE_APPOINTMENT_PATIENT);
            url = url.concat(Constants.SORT_MEDICAL_REVIEW_DATE);
        } else if (Boolean.TRUE.equals(patientRequestDTO.getSort().getIsRedRisk())) {
            url = url.concat(Constants.SORT_RED_RISK);
        }
        return url;
    }

    /**
     * Get count by URL
     *
     * @param url - URL
     * @return - Total count return in bundle
     */
    private int getCount(String url) {
        if (url.startsWith(ResourceType.RelatedPerson.toString())
                || url.startsWith(ResourceType.Patient.toString())) {
            url = StringUtil.concatString(url, Constants.AND, Constants.PARAM_COUNT, Constants.EQUAL_SYMBOL, Constants.ZERO_STRING);
        } else {
            url = StringUtil.concatString(url,
                    Constants.AND, Constants.PARAM_SEARCH_TOTAL_MODE, Constants.EQUAL_SYMBOL, Constants.ACCURATE,
                    Constants.AND, Constants.PARAM_COUNT, Constants.EQUAL_SYMBOL, String.valueOf(Constants.MAX_LIMIT));
        }
        Bundle bundle = restApiUtil.getBatchRequest(url);
        return bundle.getEntry().size() != bundle.getTotal() ? Math.abs(bundle.getEntry().size() - bundle.getTotal()) :
                bundle.getTotal();
    }


    /**
     * Process search patient details bundle and
     * convert into person details DTO
     *
     * @param entryComponent - Search patient FHIR bundle Entity.
     * @return converted PersonDetailsDTO entity.
     */
    private SearchPersonDetailsDTO processSearchPatients(Bundle.BundleEntryComponent entryComponent) {
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();

        if (entryComponent.getResource() instanceof RelatedPerson relatedPersonResource) {
            searchPersonDetailsDTO.setRelatedPerson(relatedPersonResource);
        } else if (entryComponent.getResource() instanceof Patient patient) {
            searchPersonDetailsDTO.setPatient(patient);
        }
        return searchPersonDetailsDTO;
    }

    /**
     * {@inheritDoc}
     */
    public PatientDetailsDTO searchPatientDetails(RequestDTO requestDTO) {
        String searchRelatedPersonQuery = String.format(
                Constants.SEARCH_RELATED_PERSON,
                requestDTO.getId());
        Bundle bundle = restApiUtil.getBatchRequest(searchRelatedPersonQuery);
        String searchVitalSigns = String.format(Constants.SEARCH_VITAL_SIGNS_BY_RELATED_PERSON, FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                Constants.OBSERVATION_VITAL_SIGNS,
                requestDTO.getId());
        Bundle vitalResponsBundle = restApiUtil.getBatchRequest(searchVitalSigns);
        spiceConverter.addBundleEntries(vitalResponsBundle, bundle);
        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();
        SearchPersonDetailsDTO searchPersonDetailsDTO;
        if (Objects.nonNull(bundle) && !bundle.getEntry().isEmpty()) {
            searchPersonDetailsDTO = processSearchPatientDetails(bundle);
            patientDetailsDTO = searchPersonDetailsConverter
                    .constructSearchPersonDetails(searchPersonDetailsDTO, requestDTO.getType());
            if (!Objects.isNull(searchPersonDetailsDTO.getPatient())) {
                requestDTO.setPatientReference(searchPersonDetailsDTO.getPatient().getIdPart());
                ConfirmDiagnosisDTO confirmDiagnosisDTO = getPatientDiagnosisDetails(requestDTO, Boolean.TRUE);
                ConfirmDiagnosisDTO diagnosisDTOS = addMentalHealthDetails(patientDetailsDTO, confirmDiagnosisDTO);
                patientDetailsDTO.setConfirmDiagnosis(diagnosisDTOS);
            } else {
                addMentalHealthDetails(patientDetailsDTO, null);
            }
            patientDetailsDTO.setInitialReviewed(patientVisitService.getPatientMedicalReviewStatus(requestDTO.getId()));
            patientDetailsDTO.setProvisionalDiagnosis(getProvisionalDiagnosisDetails(searchPersonDetailsDTO.getRelatedPerson().getIdPart()));
        } else {
            Logger.logInfo("No mapping related person found for search Id" + requestDTO.getId());
        }
        return patientDetailsDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PatientDetailsDTO> getPatientDetailsByVillageIds(RequestDTO requestDTO) {
        List<PatientDetailsDTO> patientDetails = new ArrayList<>();
        List<String> villageIdentifiers = requestDTO.getVillageIds().stream()
                .map(village -> StringUtil.concatString(FhirIdentifierConstants.VILLAGE_SYSTEM_URL,
                        Constants.VERTICAL_BAR, String.valueOf(village))).toList();
        String url = String.format(Constants.RELATED_PERSON_QUERY_BASED_ON_VILLAGE, String.join(Constants.COMMA, villageIdentifiers));
        Calendar calendar = Calendar.getInstance();
        if (Objects.nonNull(requestDTO.getLastSyncTime())) {
            calendar.setTime(requestDTO.getLastSyncTime());
            url = StringUtil.concatString(url,
                    String.format(Constants.LAST_UPDATED_GE_PARAM, DateUtil.getISOString(calendar)));
        }
        calendar.setTime(requestDTO.getCurrentSyncTime());
        url = StringUtil.concatString(url,
                String.format(Constants.LAST_UPDATED_LT_PARAM, DateUtil.getISOString(calendar)));
        url = StringUtil.concatString(url, Constants.AND, String.format(Constants.PAGINATE_PARAMS,
                requestDTO.getLimit(), requestDTO.getSkip()));
        Bundle bundle = restApiUtil.getBatchRequest(url);

        if (Objects.nonNull(bundle) && !bundle.getEntry().isEmpty()) {
            Map<String, Patient> patientMap = new HashMap<>();
            Map<String, RelatedPerson> relatedPersonMap = new HashMap<>();
            Map<String, SearchPersonDetailsDTO> searchPersonDetailsMap = new HashMap<>();
            for (Bundle.BundleEntryComponent component : bundle.getEntry()) {
                if (component.getResource() instanceof RelatedPerson relatedPerson) {
                    relatedPersonMap.put(relatedPerson.getIdPart(), relatedPerson);
                } else if (component.getResource() instanceof Patient patient) {
                    patientMap.put(patient.getIdPart(), patient);
                }
            }
            Set<String> relatedPersonIds = relatedPersonMap.keySet();
            Bundle vitalResponsBundle = null;
            if (!relatedPersonIds.isEmpty()) {
                String searchVitalSignsUrl = String.format(Constants.SEARCH_VITAL_SIGNS_BY_RELATED_PERSONS,
                        FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                        FhirConstants.VITAL_SIGNS_IDENTIFIER,
                        String.join(Constants.COMMA, relatedPersonIds));
                searchVitalSignsUrl = StringUtil.concatString(searchVitalSignsUrl, Constants.AND,
                        String.format(Constants.COUNT_PARAM, relatedPersonIds.size()));
                vitalResponsBundle = restApiUtil.getBatchRequest(searchVitalSignsUrl);
            }
            if (Objects.nonNull(vitalResponsBundle) && !vitalResponsBundle.getEntry().isEmpty()) {
                for (Bundle.BundleEntryComponent component : vitalResponsBundle.getEntry()) {
                    if (component.getResource() instanceof Observation observation) {
                        String relatedPersonId = observation.getPerformer().stream()
                                .filter(reference -> reference != null &&
                                        reference.getReference() != null &&
                                        !reference.getReference().isEmpty() &&
                                        reference.getReference().contains(ResourceType.RelatedPerson.name()))
                                .findFirst()
                                .map(reference -> reference.getReference().split(Constants.FORWARD_SLASH)[1])
                                .orElse(null);
                        if (Objects.nonNull(relatedPersonId)) {
                            SearchPersonDetailsDTO searchPersonDetailsDTO = searchPersonDetailsMap.getOrDefault(
                                    relatedPersonId, new SearchPersonDetailsDTO());
                            spiceConverter.setObservationDetails(observation, searchPersonDetailsDTO);
                            searchPersonDetailsMap.put(relatedPersonId, searchPersonDetailsDTO);
                        }
                    }
                }
            }
            for (Map.Entry<String, RelatedPerson> entry : relatedPersonMap.entrySet()) {
                String relatedPersonId = entry.getKey();
                RelatedPerson relatedPerson = entry.getValue();
                SearchPersonDetailsDTO searchPersonDetailsDTO = searchPersonDetailsMap.getOrDefault(relatedPersonId,
                        new SearchPersonDetailsDTO());
                searchPersonDetailsDTO.setRelatedPerson(relatedPerson);
                searchPersonDetailsDTO.setPatient(patientMap.get(
                        fhirUtils.getIdFromReference(relatedPerson.getPatient().getReference())));
                PatientDetailsDTO patientDetailDTO = searchPersonDetailsConverter
                        .constructSearchPersonDetails(searchPersonDetailsDTO, Constants.OFFLINE_ASSESSMENT);
                if (!Objects.isNull(searchPersonDetailsDTO.getPatient())) {
                    requestDTO.setPatientReference(searchPersonDetailsDTO.getPatient().getIdPart());
                    ConfirmDiagnosisDTO confirmDiagnosisDTO = getPatientDiagnosisDetails(requestDTO, Boolean.TRUE);
                    ConfirmDiagnosisDTO diagnosisDTOS = addMentalHealthDetails(patientDetailDTO, confirmDiagnosisDTO);
                    patientDetailDTO.setConfirmDiagnosis(diagnosisDTOS);
                } else {
                    addMentalHealthDetails(patientDetailDTO, null);
                }
                patientDetailDTO.setIsActive(Objects.nonNull(searchPersonDetailsDTO.getPatient())
                        ? searchPersonDetailsDTO.getPatient().getActive()
                        : searchPersonDetailsDTO.getRelatedPerson().getActive());
                patientDetails.add(patientDetailDTO);
            }
        }
        return patientDetails;
    }

    /**
     * <p>
     * Retrieves the provisional diagnosis details for a given patient.
     * </P>
     *
     * @param memberId the ID of the patient whose provisional diagnosis details are to be retrieved
     * @return a List of Strings containing the text of each provisional diagnosis
     */
    private List<String> getProvisionalDiagnosisDetails(String memberId) {
        List<String> provisionalDiagnosis = new ArrayList<>();
        String url = String.format(Constants.PROVISIONAL_DIAGNOSIS_QUERY, memberId);
        Bundle bundle = restApiUtil.getBatchRequest(url);
        if (!Objects.isNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
            provisionalDiagnosis = fhirAssessmentMapper.getProvisionalDiagnosisDetails(bundle);
        }
        return provisionalDiagnosis;
    }

    private ConfirmDiagnosisDTO addMentalHealthDetails(PatientDetailsDTO patientDetailsDTO, ConfirmDiagnosisDTO confirmDiagnosisDTO) {
        List<String> mentalLevels = new ArrayList<>();
        if (Objects.isNull(confirmDiagnosisDTO) || Objects.isNull(confirmDiagnosisDTO.getMentalHealthLevels()) || confirmDiagnosisDTO.getMentalHealthLevels().isEmpty()) {
            if ((!Objects.isNull(patientDetailsDTO.getPhq4FirstScore())
                    && Constants.TWO <= (int) Double.parseDouble(patientDetailsDTO.getPhq4FirstScore()))) {
                mentalLevels.add(Constants.GAD7);
            }
            if ((!Objects.isNull(patientDetailsDTO.getPhq4SecondScore())
                    && Constants.TWO <= (int) Double.parseDouble(patientDetailsDTO.getPhq4SecondScore()))) {
                mentalLevels.add(Constants.PHQ9);
            }
        }
        if (Objects.nonNull(confirmDiagnosisDTO) && Objects.nonNull(confirmDiagnosisDTO.getMentalHealthLevels()) &&
                !confirmDiagnosisDTO.getMentalHealthLevels().isEmpty()) {
            mentalLevels.addAll(confirmDiagnosisDTO.getMentalHealthLevels());
        }
        if (mentalLevels.isEmpty()) {
            mentalLevels.add(Constants.PHQ4);
        }
        patientDetailsDTO.setMentalHealthLevels(mentalLevels);
        return confirmDiagnosisDTO;
    }

    /**
     * Process search patient details bundle and
     * convert into person details DTO
     *
     * @param bundle - Search patient FHIR bundle Entity.
     * @return converted PersonDetailsDTO entity.
     */
    private SearchPersonDetailsDTO processSearchPatientDetails(Bundle bundle) {
        SearchPersonDetailsDTO searchPersonDetailsDTO = new SearchPersonDetailsDTO();

        bundle.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .forEach(resource -> {
                    if (resource instanceof RelatedPerson relatedPersonResource) {
                        searchPersonDetailsDTO.setRelatedPerson(relatedPersonResource);
                    } else if (resource instanceof Observation observationResource) {
                        spiceConverter.setObservationDetails(observationResource, searchPersonDetailsDTO);
                    } else if (resource instanceof Patient patientResource) {
                        searchPersonDetailsDTO.setPatient(patientResource);
                    }
                });
        return searchPersonDetailsDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PregnancyDetailsDTO createPregnancyDetails(PregnancyDetailsDTO pregnancyDetailsDTO) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        String url = String.format(Constants.GET_MEMBER_ID, pregnancyDetailsDTO.getMemberReference());
        Bundle relatedPersonBundle = restApiUtil.getBatchRequest(url);
        RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();
        Patient patient = null;
        if (Objects.nonNull(pregnancyDetailsDTO.getPatientReference())) {
            patient = restApiUtil.getPatientById(StringUtil.concatString(fhirServerUrl, FhirConstants.PATIENT,
                    Constants.FORWARD_SLASH, pregnancyDetailsDTO.getPatientReference()));
        } else {
            patient = patientConverter.createPatientFromRelatedPerson(relatedPerson);
        }
        if (Objects.nonNull(pregnancyDetailsDTO.getNcdPatientStatus())) {
            createPatientStatus(pregnancyDetailsDTO, bundle);
        }
        if (Objects.nonNull(pregnancyDetailsDTO.getActualDeliveryDate())) {
            Bundle conditionBundle = restApiUtil.getBatchRequest(String.format(Constants.CONDITION_QUERY_VERIFICATION_STATUS, pregnancyDetailsDTO.getPatientReference()));
            for (Bundle.BundleEntryComponent resource : conditionBundle.getEntry()) {
                Condition condition = (Condition) resource.getResource();
                condition.getIdentifier().forEach(identifier -> {
                    if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_DIAGNOSIS_NCD_IDENTIFIER_SYSTEM_URL)) {
                        Condition ncdCondition = removeNCDDiagnosis(condition);
                        commonConverter.setConditionInBundle(bundle, ncdCondition,
                                FhirConstants.PATIENT_STATUS_DIABETES_IDENTIFIER_URL, Boolean.TRUE,
                                pregnancyDetailsDTO.getProvenance());
                    } else if (identifier.getSystem().equals(FhirIdentifierConstants.PATIENT_DIAGNOSIS_PREGNANCY_IDENTIFIER_URL)) {
                        Condition maternalHealthCondition = removeMaternalHealthDiagnosis(condition);
                        commonConverter.setConditionInBundle(bundle, maternalHealthCondition,
                                FhirConstants.PATIENT_STATUS_DIABETES_IDENTIFIER_URL, Boolean.TRUE,
                                pregnancyDetailsDTO.getProvenance());
                    }
                });
            }
            pregnancyDetailsDTO.setIsPregnant(Boolean.FALSE);
            pregnancyDetailsDTO.setIsPregnancyRisk(Boolean.FALSE);
        }

        Encounter encounter = encounterConverter.createEncounter(patient, relatedPerson, null, null,
                Objects.isNull(pregnancyDetailsDTO.getProvenance().getModifiedDate())
                        ? new Date() : pregnancyDetailsDTO.getProvenance().getModifiedDate());
        if (Objects.nonNull(pregnancyDetailsDTO.getPatientVisitId())) {
            encounter.setPartOf(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                    Constants.FORWARD_SLASH, pregnancyDetailsDTO.getPatientVisitId())));
        }
        encounter.setStatus(Encounter.EncounterStatus.INPROGRESS);
        encounter.setServiceProvider(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Organization),
                Constants.FORWARD_SLASH, pregnancyDetailsDTO.getProvenance().getOrganizationId())));
        encounter.addIdentifier().setSystem(FhirIdentifierConstants.ENCOUNTER_TYPE_SYSTEM_URL)
                .setValue(pregnancyDetailsDTO.getType());

        Observation pregnancyObservation = createPregnancyObservation(pregnancyDetailsDTO, encounter);
        Observation temperatureObservation = createTemperatureObservation(pregnancyDetailsDTO, encounter);
        Observation weightObservation = createWeightObservation(pregnancyDetailsDTO, encounter);
        Condition diagnosisCondition = createDiagnosisCondition(pregnancyDetailsDTO, encounter, patient, relatedPerson);

        commonConverter.setPatientDetailsInBundle(bundle, patient, FhirConstants.PATIENT_IDENTIFIER_URL,
                pregnancyDetailsDTO.getProvenance());
        commonConverter.setResourceInBundle(bundle, encounter, FhirConstants.ENCOUNTER_IDENTIFIER_URL,
                FhirConstants.ENCOUNTER_ID, String.valueOf(ResourceType.Encounter), pregnancyDetailsDTO.getProvenance());
        commonConverter.setResourceInBundle(bundle, diagnosisCondition, FhirConstants.CONDITION_IDENTIFIER_URL,
                FhirConstants.CONDITION_ID, String.valueOf(ResourceType.Condition), pregnancyDetailsDTO.getProvenance());

        setObservationDetails(bundle, pregnancyObservation, FhirConstants.PREGNANCY_IDENTIFIER_URL, patient,
                relatedPerson, pregnancyDetailsDTO.getProvenance());
        setObservationDetails(bundle, temperatureObservation, FhirConstants.TEMPERATURE_IDENTIFIER_URL,
                patient, relatedPerson, pregnancyDetailsDTO.getProvenance());
        setObservationDetails(bundle, weightObservation, FhirConstants.WEIGHT_IDENTIFIER_URL,
                patient, relatedPerson, pregnancyDetailsDTO.getProvenance());

        Observation vitalSignsObservation = createVitalSignsObservation(bundle, relatedPerson,
                pregnancyDetailsDTO.getProvenance());
        setObservationDetails(bundle, vitalSignsObservation, FhirConstants.VITAL_SIGNS_IDENTIFIER_URL,
                patient, null, pregnancyDetailsDTO.getProvenance());

        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        return pregnancyDetailsDTO;
    }

    /**
     * <p>
     * Removes maternal health diagnoses from the given pregnancy details.
     * </p>
     *
     * @param maternalHealthCondition The condition,
     *                            including any maternal health diagnoses to be removed.
     */
    private Condition removeMaternalHealthDiagnosis(Condition maternalHealthCondition) {
        maternalHealthCondition.setVerificationStatus(fhirUtils.setCodes(Constants.UNCONFIRMED));
        return maternalHealthCondition;
    }

    /**
     * <p>
     * Removes NCD diagnoses from the given pregnancy details.
     * </p>
     *
     * @param ncdCondition The condition,
     *                            including any NCD diagnoses to be removed.
     */
    private Condition removeNCDDiagnosis(Condition ncdCondition) {
        List<CodeableConcept> categories = ncdCondition.getCategory();
        List<CodeableConcept> newCategories = new ArrayList<>();
        categories.forEach(category -> {
            if (!category.getText().equals(Constants.GESTATIONAL_DIABETES_MELLITUS)) {
                newCategories.add(category);
            }
        });
        ncdCondition.setCategory(newCategories);
        if (newCategories.isEmpty()) {
            ncdCondition.setVerificationStatus(fhirUtils.setCodes(Constants.UNCONFIRMED));
        }
        return ncdCondition;
    }

    /**
     * Process and create FHIR weight observation based on given
     * pregnancy details
     *
     * @param pregnancyDetailsDTO The pregnancy details
     * @param encounter           The FHIR Encounter entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createWeightObservation(PregnancyDetailsDTO pregnancyDetailsDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(pregnancyDetailsDTO.getWeight())) {
            observation = commonConverter.createBasicObservation(pregnancyDetailsDTO.getWeight(),
                    pregnancyDetailsDTO.getProvenance().getModifiedDate(), MetaCodeConstants.WEIGHT_KEY,
                    FhirConstants.KG_CODE, FhirConstants.WEIGHT, Constants.OBSERVATION_WEIGHT);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.WEIGHT);
        }
        return observation;
    }

    /**
     * Create a new pregnancy observation based on the pregnancy details.
     *
     * @param pregnancyDetailsDTO The assessment details for the given patient.
     * @param encounter           The FHIR Encounter entity.
     * @return {@link Observation} Converted FHIR Observation entity.
     */
    private Observation createPregnancyObservation(PregnancyDetailsDTO pregnancyDetailsDTO, Encounter encounter) {
        String url = String.format(Constants.FETCH_LATEST_OBSERVATION_QUERY, FhirConstants.PREGNANCY.toLowerCase(),
                Observation.ObservationStatus.PRELIMINARY.name().toLowerCase(),
                pregnancyDetailsDTO.getMemberReference());
        Bundle pregnancyBundle = restApiUtil.getBatchRequest(url);
        Observation observation = null;
        if (pregnancyBundle.getEntry().isEmpty()) {
            observation = pregnancyConverter.createPregnancyObservation(pregnancyDetailsDTO);
        } else {
            Observation pregnancyObservation = (Observation) pregnancyBundle.getEntry().getFirst().getResource();
            observation = pregnancyConverter.updatePregnancyObservation(pregnancyObservation, pregnancyDetailsDTO);
        }
        commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
        return observation;
    }

    /**
     * Process and create FHIR Temperature observation based on given
     * assessment details
     *
     * @param pregnancyDetailsDTO The patient pregnancy details
     * @param encounter           The FHIR Encounter entity
     * @return Converted FHIR Observation entity.
     */
    private Observation createTemperatureObservation(PregnancyDetailsDTO pregnancyDetailsDTO, Encounter encounter) {
        Observation observation = null;
        if (Objects.nonNull(pregnancyDetailsDTO.getTemperature())) {
            observation = commonConverter.createBasicObservation(pregnancyDetailsDTO.getTemperature(),
                    pregnancyDetailsDTO.getProvenance().getModifiedDate(), MetaCodeConstants.TEMPERATURE_KEY,
                    FhirConstants.CELSIUS_CODE, FhirConstants.TEMPERATURE, Constants.OBSERVATION_TEMPERATURE);
            commonConverter.setObservationEncounterAndOrganization(observation, null, encounter);
            commonConverter.setObservationText(observation, FhirConstants.TEMPERATURE);
        }
        return observation;
    }

    /**
     * Set observation details in FHIR bundle resource
     *
     * @param bundle        The FHIR bundle resource
     * @param observation   The FHIR Observation entity
     * @param identifierUrl The Observation identifier url
     */
    private void setObservationDetails(Bundle bundle, Observation observation, String identifierUrl,
                                       Patient patient, RelatedPerson relatedPerson, ProvenanceDTO provenanceDTO) {
        if (Objects.nonNull(observation)) {
            commonConverter.setObservationReference(observation, patient, relatedPerson);
            commonConverter.setObservationDetailsInBundle(bundle, observation, identifierUrl, provenanceDTO);
            observation.addPerformer(new Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    provenanceDTO.getOrganizationId())));
        }
    }

    /**
     * Create the condition for the pregnancy diagnosis
     *
     * @param pregnancyDetailsDTO pregnanciesDetailsDTO which contains the patient pregnancy details
     * @param encounter           the encounter resource
     * @param patient             the patient resource
     * @param relatedPerson       the related person resource
     * @return condition resource
     */
    private Condition createDiagnosisCondition(PregnancyDetailsDTO pregnancyDetailsDTO, Encounter encounter,
                                               Patient patient, RelatedPerson relatedPerson) {
        Condition condition = null;
        if (Objects.nonNull(pregnancyDetailsDTO.getDiagnosis()) && !pregnancyDetailsDTO.getDiagnosis().isEmpty()) {
            condition = getPregnancyCondition(pregnancyDetailsDTO.getMemberReference());
            conditionConverter.convertPregnancyDiagnosisToCondition(condition, pregnancyDetailsDTO);
            commonConverter.setConditionMetaDetails(condition, encounter, patient, relatedPerson);
            condition.setText(commonConverter.getNarrative(FhirConstants.PREGNANCY));
        }
        return condition;
    }

    /**
     * Fetch the condition associated with the patient pregnancy observation
     *
     * @param relatedPersonFhirId related person fhir id to fetch the condition
     * @return the condition associated with the patient pregnancy observation
     */
    private Condition getPregnancyCondition(String relatedPersonFhirId) {
        Bundle bundle = restApiUtil.getBatchRequest(String.format(
                Constants.PREGNANCY_CONDITION_QUERY, FhirConstants.PREGNANCY, relatedPersonFhirId));
        if (bundle.getEntry().isEmpty()) {
            return new Condition();
        }
        return (Condition) bundle.getEntry().get(0).getResource();
    }

    /**
     * Process and create FHIR vital signs observation based on given
     * bundle details
     *
     * @param bundle        The FHIR bundle entity it contains patient vital observations.
     * @param relatedPerson The FHIR RelatedPerson entity.
     * @param provenanceDTO The ProvenanceDTO entity to store performers information
     * @return Converted FHIR Observation entity.
     */
    private Observation createVitalSignsObservation(Bundle bundle, RelatedPerson relatedPerson,
                                                    ProvenanceDTO provenanceDTO) {
        VitalSignsDTO vitalSignsDTO = new VitalSignsDTO();
        vitalSignsDTO.setRelatedPersonId(relatedPerson.getIdPart());
        vitalSignsDTO.setProvenanceDTO(provenanceDTO);
        vitalSignsDTO.setPregnancyObservation(commonConverter.getObservationFromBundleByIdentifier(
                bundle, FhirConstants.PREGNANCY_IDENTIFIER_URL));
        vitalSignsDTO.setTemperatureObservation(commonConverter.getObservationFromBundleByIdentifier(
                bundle, FhirConstants.TEMPERATURE_IDENTIFIER_URL));
        vitalSignsDTO.setWeightObservation(commonConverter.getObservationFromBundleByIdentifier(
                bundle, FhirConstants.WEIGHT_IDENTIFIER_URL));
        Observation observation = vitalSignsConverter.createOrUpdateVitalSigns(vitalSignsDTO, bundle);
        commonConverter.setObservationText(observation, FhirConstants.VITAL_SIGNS);
        return observation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PregnancyDetailsDTO getPregnancyDetails(RequestDTO requestData) {
        fhirUtils.initiateCodesMap();
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setMemberReference(requestData.getId());
        setDiagnosisInPregnancyDetailsDTO(pregnancyDetailsDTO, requestData);
        setPregnancyDetails(pregnancyDetailsDTO, requestData);
        return pregnancyDetailsDTO;
    }

    /**
     * Set the pregnancy information in pregnancy details dto.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} to set the diagnosis information
     * @param requestData         {@link RequestDTO} to fetch the pregnancy information.
     */
    private void setPregnancyDetails(PregnancyDetailsDTO pregnancyDetailsDTO, RequestDTO requestData) {
        String searchVitalSigns = String.format(Constants.SEARCH_VITAL_SIGNS_BY_RELATED_PERSON,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL, Constants.OBSERVATION_VITAL_SIGNS,
                requestData.getId());
        Bundle vitalResponsBundle = restApiUtil.getBatchRequest(searchVitalSigns);
        if (Objects.nonNull(vitalResponsBundle) && !vitalResponsBundle.getEntry().isEmpty()) {
            SearchPersonDetailsDTO searchPersonDetailsDTO = processSearchPatientDetails(vitalResponsBundle);
            if (Objects.nonNull(searchPersonDetailsDTO.getPregnancyObservation())) {
                pregnancyConverter.convertObservationToPregnancyDetails(pregnancyDetailsDTO,
                        searchPersonDetailsDTO.getPregnancyObservation());
            }
            if (Objects.nonNull(searchPersonDetailsDTO.getTemperatureObservation())
                    && Objects.nonNull(searchPersonDetailsDTO.getTemperatureObservation().getValueQuantity())) {
                pregnancyDetailsDTO.setTemperature(
                        searchPersonDetailsDTO.getTemperatureObservation().getValueQuantity().getValue().doubleValue());
            }
            if (Objects.nonNull(searchPersonDetailsDTO.getWeightObservation())
                    && Objects.nonNull(searchPersonDetailsDTO.getWeightObservation().getValueQuantity())) {
                pregnancyDetailsDTO.setWeight(
                        searchPersonDetailsDTO.getWeightObservation().getValueQuantity().getValue().doubleValue());
            }
        } else {
            Logger.logInfo("No mapping related person found for search Id : " + requestData.getId());
        }
    }

    /**
     * Set the diagnosis information in pregnancy details dto.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} to set the diagnosis information
     * @param requestData         {@link RequestDTO} to fetch the pregnancy information.
     */
    private void setDiagnosisInPregnancyDetailsDTO(PregnancyDetailsDTO pregnancyDetailsDTO, RequestDTO requestData) {
        List<Map<String, String>> diagnosis = new ArrayList<>();
        Bundle bundle = restApiUtil.getBatchRequest(String.format(Constants.PREGNANCY_CONDITION_QUERY,
                FhirConstants.PREGNANCY, requestData.getId()));
        if (!bundle.getEntry().isEmpty()) {
            Condition condition = (Condition) bundle.getEntry().getFirst().getResource();
            for (CodeableConcept codeableConcept : condition.getCategory()) {
                MetaCodeDetails metaCodeDetails = fhirUtils.getCodeDetails().get(codeableConcept.getText());
                Map<String, String> diagnosisMap = new HashMap<>();
                diagnosisMap.put(Constants.NAME, Objects.isNull(metaCodeDetails) ? null : metaCodeDetails.getText());
                diagnosisMap.put(Constants.VALUE, codeableConcept.getText());
                diagnosis.add(diagnosisMap);
            }
            pregnancyDetailsDTO.setDiagnosisTime(condition.getOnsetDateTimeType().getValue());
            pregnancyDetailsDTO.setDiagnosis(diagnosis);
        }
    }

    /**
     * Populates the provided map with date filters based on the medical review date and assessment date
     * from the given PatientFilterDTO.
     *
     * @param patientFilterDTO the PatientFilterDTO containing the date filters
     * @param map              the map to be populated with date filters
     * @return the map populated with date filters
     */
    private Map<String, String> getDatesFilter(PatientFilterDTO patientFilterDTO, Map<String, String> map) {
        Map<String, String> dateMap = CommonUtils.getDatesUsingUserTimezone();
        if (Objects.nonNull(patientFilterDTO.getMedicalReviewDate())) {
            if (patientFilterDTO.getMedicalReviewDate().equalsIgnoreCase(Constants.TODAY)) {
                map.put(Constants.MEDICAL_REVIEW_START_DATE, dateMap.get(Constants.TODAY_START_DATE));
                map.put(Constants.MEDICAL_REVIEW_END_DATE, dateMap.get(Constants.TODAY_END_DATE));
            } else if (patientFilterDTO.getMedicalReviewDate().equalsIgnoreCase(Constants.TOMORROW)) {
                map.put(Constants.MEDICAL_REVIEW_START_DATE, dateMap.get(Constants.TOMORROW_START_DATE));
                map.put(Constants.MEDICAL_REVIEW_END_DATE, dateMap.get(Constants.TOMORROW_END_DATE));
            }
        }

        if (Objects.nonNull(patientFilterDTO.getAssessmentDate())) {
            if (patientFilterDTO.getAssessmentDate().equalsIgnoreCase(Constants.TODAY)) {
                map.put(Constants.ASSESSMENT_START_DATE, dateMap.get(Constants.TODAY_START_DATE));
                map.put(Constants.ASSESSMENT_END_DATE, dateMap.get(Constants.TODAY_END_DATE));
            } else if (patientFilterDTO.getAssessmentDate().equalsIgnoreCase(Constants.TOMORROW)) {
                map.put(Constants.ASSESSMENT_START_DATE, dateMap.get(Constants.TOMORROW_START_DATE));
                map.put(Constants.ASSESSMENT_END_DATE, dateMap.get(Constants.TOMORROW_END_DATE));
            }
        }

        if (Objects.nonNull(patientFilterDTO.getLabTestReferredOn())) {
            if (patientFilterDTO.getLabTestReferredOn().equalsIgnoreCase(Constants.TODAY)) {
                map.put(Constants.LAB_TEST_REFERRED_START_DATE, dateMap.get(Constants.TODAY_START_DATE));
                map.put(Constants.LAB_TEST_REFERRED_END_DATE, dateMap.get(Constants.TODAY_END_DATE));
            } else if (patientFilterDTO.getLabTestReferredOn().equalsIgnoreCase(Constants.YESTERDAY)) {
                map.put(Constants.LAB_TEST_REFERRED_START_DATE, dateMap.get(Constants.YESTERDAY_START_DATE));
                map.put(Constants.LAB_TEST_REFERRED_END_DATE, dateMap.get(Constants.YESTERDAY_END_DATE));
            } else if (Constants.TOMORROW.equalsIgnoreCase(patientFilterDTO.getLabTestReferredOn())) {
                map.put(Constants.LAB_TEST_REFERRED_START_DATE, dateMap.get(Constants.TOMORROW_START_DATE));
                map.put(Constants.LAB_TEST_REFERRED_END_DATE, dateMap.get(Constants.TOMORROW_END_DATE));
            }
        }

        if (Objects.nonNull(patientFilterDTO.getPrescriptionReferredOn())) {
            if (patientFilterDTO.getPrescriptionReferredOn().equalsIgnoreCase(Constants.TODAY)) {
                map.put(Constants.PRESCRIPTION_REFERRED_START_DATE, dateMap.get(Constants.TODAY_START_DATE));
                map.put(Constants.PRESCRIPTION_REFERRED_END_DATE, dateMap.get(Constants.TODAY_END_DATE));
            } else if (patientFilterDTO.getPrescriptionReferredOn().equalsIgnoreCase(Constants.YESTERDAY)) {
                map.put(Constants.PRESCRIPTION_REFERRED_START_DATE, dateMap.get(Constants.YESTERDAY_START_DATE));
                map.put(Constants.PRESCRIPTION_REFERRED_END_DATE, dateMap.get(Constants.YESTERDAY_END_DATE));
            } else if (Constants.TOMORROW.equalsIgnoreCase(patientFilterDTO.getPrescriptionReferredOn())) {
                map.put(Constants.PRESCRIPTION_REFERRED_START_DATE, dateMap.get(Constants.TOMORROW_START_DATE));
                map.put(Constants.PRESCRIPTION_REFERRED_END_DATE, dateMap.get(Constants.TOMORROW_END_DATE));
            }
        }
        return map;
    }

    /**
     * {@inheritDoc}
     */
    public ConfirmDiagnosisDTO getPatientDiagnosisDetails(RequestDTO request, boolean isFromDetails) {
        Logger.logInfo("Patient reference to get diagnosis details is: " + request.getPatientReference());
        ConfirmDiagnosisDTO confirmDiagnosisDTO = new ConfirmDiagnosisDTO();
        if (Objects.isNull(request.getPatientReference())) {
            return new ConfirmDiagnosisDTO();
        }
        String url = null;
        fhirUtils.initiateCodesMap();
        if (!isFromDetails) {
            String commaSeparatedString = String.join(Constants.COMMA, request.getDiagnosisType());
            commaSeparatedString = commaSeparatedString.concat(Constants.COMMA_OTHER);
            url = String.format(Constants.CONDITION_QUERY_VERIFICATION_STATUS_WITH_IDENTIFIER, request.getPatientReference(), commaSeparatedString);
        } else {
            url = String.format(Constants.CONDITION_QUERY_VERIFICATION_STATUS, request.getPatientReference());
        }
        Bundle bundle = restApiUtil.getBatchRequest(url);
        if (!Objects.isNull(bundle.getEntry()) && !bundle.getEntry().isEmpty()) {
            confirmDiagnosisDTO = fhirAssessmentMapper.mapToDiagnosis(bundle);
        }
        return confirmDiagnosisDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateReferredSite(ScreeningLogRequestDTO requestDTO) {
        String memberFhirId = requestDTO.getMemberReference();
        ProvenanceDTO provenanceDTO = new ProvenanceDTO();
        provenanceDTO.setUserId(requestDTO.getUserId().toString());
        provenanceDTO.setOrganizationId(requestDTO.getSiteId().toString());
        provenanceDTO.setModifiedDate(requestDTO.getScreeningDateTime());
        Bundle relatedPersonbundle = restApiUtil.getBatchRequest(String.format(Constants.GET_MEMBER_ID, memberFhirId));
        List<Bundle.BundleEntryComponent> entries = relatedPersonbundle.getEntry();
        RelatedPerson relatedPerson = null;
        if (entries.isEmpty()) {
            throw new DataNotAcceptableException(2004);
        } else {
            relatedPerson = (RelatedPerson) relatedPersonbundle.getEntry().getFirst().getResource();
        }
        String patientFhirId = relatedPerson.getPatient().getReferenceElement().getIdPart();
        relatedPerson.getIdentifier().forEach(identifier -> {
            if (FhirIdentifierConstants.ORGANIZATION_ID_SYSTEM_URL.equals(identifier.getSystem())) {
                identifier.setValue(String.valueOf(requestDTO.getSiteId()));
            }
        });
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        fhirUtils.setBundle(StringUtil.concatString(FhirConstants.RELATED_PERSON,
                        Constants.FORWARD_SLASH, memberFhirId),
                StringUtil.concatString(Constants.FHIR_BASE_URL, memberFhirId),
                Bundle.HTTPVerb.PUT, relatedPerson, bundle, provenanceDTO);

        if (!Objects.isNull(patientFhirId)) {
            Bundle patientBundle = getPatientDetailsByPatientReference(patientFhirId);
            List<Bundle.BundleEntryComponent> patientBundleEntries = patientBundle.getEntry();
            Patient patient = null;
            if (!patientBundleEntries.isEmpty()) {
                patient = (Patient) patientBundle.getEntry().getFirst().getResource();
            }
            Organization organization = new Organization();
            organization.setId(String.valueOf(requestDTO.getSiteId()));
            commonConverter.setPatientOrganization(patient, organization);
            fhirUtils.setBundle(StringUtil.concatString(FhirConstants.PATIENT, Constants.FORWARD_SLASH, patientFhirId),
                    StringUtil.concatString(Constants.FHIR_BASE_URL, patientFhirId), Bundle.HTTPVerb.PUT, patient, bundle, provenanceDTO);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean updatePregnancyANCRisk(PregnancyDetailsDTO pregnancyDetailsDTO) {
        Boolean isUpdated = Boolean.FALSE;
        if (Objects.nonNull(pregnancyDetailsDTO.getIsPregnancyRisk())) {
            fhirUtils.initiateCodesMap();
            Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
            String relatedPersonUrl = String.format(Constants.GET_MEMBER_ID,
                    pregnancyDetailsDTO.getMemberReference());
            Bundle relatedPersonBundle = restApiUtil.getBatchRequest(relatedPersonUrl);
            RelatedPerson relatedPerson = (RelatedPerson) relatedPersonBundle.getEntry().getFirst().getResource();
            Patient patient = null;
            if (Objects.nonNull(pregnancyDetailsDTO.getPatientReference())) {
                patient = restApiUtil.getPatientById(StringUtil.concatString(fhirServerUrl, FhirConstants.PATIENT,
                        Constants.FORWARD_SLASH, pregnancyDetailsDTO.getPatientReference()));
            }
            Observation observation = null;
            String url = String.format(Constants.FETCH_LATEST_OBSERVATION_QUERY, FhirConstants.PREGNANCY.toLowerCase(),
                    Observation.ObservationStatus.PRELIMINARY.name().toLowerCase(),
                    pregnancyDetailsDTO.getMemberReference());
            Bundle pregnancyBundle = restApiUtil.getBatchRequest(url);
            if (!pregnancyBundle.getEntry().isEmpty()) {
                Observation pregnancyObservation = (Observation) pregnancyBundle.getEntry().getFirst().getResource();
                observation = pregnancyConverter.updatePregnancyObservation(pregnancyObservation, pregnancyDetailsDTO);
                setObservationDetails(bundle, observation, FhirConstants.PREGNANCY_IDENTIFIER_URL, patient,
                        relatedPerson, pregnancyDetailsDTO.getProvenance());
                Observation vitalSignsObservation = createVitalSignsObservation(bundle, relatedPerson,
                        pregnancyDetailsDTO.getProvenance());
                setObservationDetails(bundle, vitalSignsObservation, FhirConstants.VITAL_SIGNS_IDENTIFIER_URL,
                        patient, relatedPerson, pregnancyDetailsDTO.getProvenance());
                restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
                isUpdated = Boolean.TRUE;
            }
        }
        return isUpdated;
    }

    /**
     * {@inheritDoc}
     */
    public Patient createPatientByMemberReference(String memberReference, ProvenanceDTO provenanceDTO,
                                                  Bundle transactionBundle) {
        Patient patient = null;
        String uuid = fhirUtils.getUniqueId();
        Bundle bundle =
                restApiUtil.getBatchRequest(StringUtil.concatString(ResourceType.RelatedPerson.name(),
                        Constants.QUESTION_MARK, RelatedPerson.SP_RES_ID, Constants.EQUAL_SYMBOL,
                        memberReference));
        RelatedPerson relatedPerson = null;
        if (Objects.nonNull(bundle) && Objects.nonNull(bundle.getEntry()) && !bundle.getEntry().isEmpty() &&
                bundle.getEntry().getFirst().getResource() instanceof RelatedPerson relatedPersonResource) {
            relatedPerson = relatedPersonResource;
        }
        if (Objects.nonNull(relatedPerson)) {
            patient = patientConverter.createPatientFromRelatedPerson(relatedPerson);
            String fullUrl = Constants.FHIR_BASE_URL.concat(uuid);
            String url =
                    String.valueOf(ResourceType.Patient).concat(Constants.FORWARD_SLASH)
                            .concat(Constants.FHIR_BASE_URL)
                            .concat(uuid);
            fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, patient, transactionBundle, provenanceDTO);
            patient.setId(fullUrl);
        }
        return patient;
    }

    private void createPatientStatus(PregnancyDetailsDTO pregnancyDetailsDTO, Bundle bundle) {
        fhirUtils.initiateCodesMap();
        pregnancyDetailsDTO.getProvenance().setModifiedDate(new Date());
        String url = String.format(Constants.PATIENT_STATUS_CONDITION_QUERY, pregnancyDetailsDTO.getPatientReference(),
                Constants.PATIENT_STATUS_FIELD);
        Bundle resultBundle = restApiUtil.getBatchRequest(url);

        AtomicReference<Condition> diabetesCondition = new AtomicReference<>();
        AtomicReference<Condition> hypertensionCondition = new AtomicReference<>();
        resultBundle.getEntry().stream().map(Bundle.BundleEntryComponent::getResource)
                .filter(Condition.class::isInstance)
                .map(Condition.class::cast)
                .forEach(condition -> {
                    if (condition.getIdentifier().getFirst().getValue().equals(Constants.DIABETES)) {
                        diabetesCondition.set(condition);
                    }
                    if (condition.getIdentifier().getFirst().getValue().equals(Constants.HYPERTENSION)) {
                        hypertensionCondition.set(condition);
                    }
                });
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();
        patientStatusDTO.setNcdPatientStatus(pregnancyDetailsDTO.getNcdPatientStatus());
        patientStatusDTO.setProvenance(pregnancyDetailsDTO.getProvenance());
        patientStatusDTO.setMemberReference(pregnancyDetailsDTO.getMemberReference());
        patientStatusDTO.setPatientReference(pregnancyDetailsDTO.getPatientReference());
        patientStatusDTO.setPatientVisitId(pregnancyDetailsDTO.getPatientVisitId());
        commonConverter.createOrUpdateDiabetesAndHypertensionPatientStatus(patientStatusDTO, bundle,
                diabetesCondition, hypertensionCondition, Objects.nonNull(diabetesCondition.get()));
        if (Constants.KNOWN_PATIENT.equals(patientStatusDTO.getNcdPatientStatus().getDiabetesStatus())
                || Constants.KNOWN_PATIENT.equals(patientStatusDTO.getNcdPatientStatus().getHypertensionStatus())) {
            Condition confirmedNCDCondition = commonConverter.getConfirmDiagnosis(patientStatusDTO.getPatientReference(), Constants.NCD);
            commonConverter.updateNCDKnownStatus(patientStatusDTO, bundle, confirmedNCDCondition, Objects.nonNull(confirmedNCDCondition));
        }
    }

    /**
     * Create coverage for the insurance data of the patient
     *
     * @param request          request containing the enrollment data
     * @param patientReference the fhir ID of the patient resource
     * @param bundle           the bundle to be saved in database
     */
    private void createCoverage(EnrollmentRequestDTO request, String patientReference, Bundle bundle) {
        String uuid = fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Coverage), Constants.FORWARD_SLASH, fullUrl);
        Coverage coverage = new Coverage();
        coverage.setBeneficiary(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, patientReference)));
        if (Boolean.TRUE.equals(request.getBioData().getInsuranceStatus())) {
            coverage.setStatus(Coverage.CoverageStatus.ACTIVE);
            coverage.setSubscriberId(request.getBioData().getInsuranceId());
            coverage.setType(request.getBioData().getInsuranceType().equalsIgnoreCase(MetaCodeConstants.OTHER) ?
                    fhirUtils.createCodeableConcept(MetaCodeConstants.OTHER + Constants.HIGHFIN + request.getBioData().getOtherInsurance()) :
                    fhirUtils.createCodeableConcept(request.getBioData().getInsuranceType()));
        } else {
            coverage.setStatus(Coverage.CoverageStatus.CANCELLED);
        }
        fhirUtils.setBundle(url, fullUrl, Bundle.HTTPVerb.POST, coverage, bundle, request.getProvenance());
    }
}