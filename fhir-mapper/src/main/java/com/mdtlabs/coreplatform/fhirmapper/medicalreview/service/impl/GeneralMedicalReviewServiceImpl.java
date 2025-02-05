package com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Money;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.PaymentNotice;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.RelatedPerson;
import org.hl7.fhir.r4.model.ResourceType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdPatientStatus;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.CommonConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.PatientStatusConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirAssessmentMapper;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.GeneralMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

/**
 * <p>
 * This class is a service class to perform operation on GeneralMedicalReview
 * operations.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Mar 20, 2024
 */
@Service
public class GeneralMedicalReviewServiceImpl implements GeneralMedicalReviewService {

    private final FhirAssessmentMapper fhirAssessmentMapper;

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final PatientService patientService;

    private final HouseholdService householdService;

    private final PrescriptionRequestService prescriptionService;

    private final InvestigationService investigationService;

    private final PatientStatusConverter patientStatusConverter;

    private final CommonConverter commonConverter;

    private final PatientConverter patientConverter;

    private final SpiceConverter spiceConverter;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    public GeneralMedicalReviewServiceImpl(FhirAssessmentMapper fhirAssessmentMapper, FhirUtils fhirUtils,
                                           PatientService patientService, HouseholdService householdService,
                                           PrescriptionRequestService prescriptionService, PatientStatusConverter patientStatusConverter,
                                           CommonConverter commonConverter, PatientConverter patientConverter, RestApiUtil restApiUtil,
                                           InvestigationService investigationService, SpiceConverter spiceConverter) {
        this.fhirAssessmentMapper = fhirAssessmentMapper;
        this.fhirUtils = fhirUtils;
        this.patientService = patientService;
        this.householdService = householdService;
        this.prescriptionService = prescriptionService;
        this.patientStatusConverter = patientStatusConverter;
        this.commonConverter = commonConverter;
        this.patientConverter = patientConverter;
        this.restApiUtil = restApiUtil;
        this.investigationService = investigationService;
        this.spiceConverter = spiceConverter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createGeneralMedicalReview(GeneralMedicalReviewDTO generalMedicalReviewDTO) {
        fhirUtils.initiateCodesMap();
        generalMedicalReviewDTO.getEncounter().setType(Constants.MEDICAL_REVIEW);
        generalMedicalReviewDTO.getEncounter().setDiagnosisType(Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW);
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        //Create Patient
        patientService.setPatientReferenceInEncounterDetails(generalMedicalReviewDTO.getEncounter(), bundle);
        String encounterId = patientService.createOrUpdateMedicalReviewEncounter(generalMedicalReviewDTO.getId(),
                generalMedicalReviewDTO.getEncounter(), Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW, null, bundle);
        generalMedicalReviewDTO.setId(encounterId);
        String urlSign = StringUtil.concatString(String.valueOf(ResourceType.Observation), Constants.FORWARD_SLASH,
                Constants.FHIR_BASE_URL);
        Observation observation = fhirAssessmentMapper.createNotes(Constants.CLINICAL_NOTES,
                generalMedicalReviewDTO.getClinicalNotes(), generalMedicalReviewDTO.getEncounter());
        Observation presentingComplains = fhirAssessmentMapper.createSignsObservation(generalMedicalReviewDTO.getPresentingComplaints(), generalMedicalReviewDTO.getEncounter(),
                Constants.PRESENTING_COMPLAINTS, generalMedicalReviewDTO.getPresentingComplaintsNotes());
        if (!Objects.isNull(presentingComplains)) {
            String presentingComplainsRef = fhirAssessmentMapper.addObservationToBundle(presentingComplains, bundle,
                    generalMedicalReviewDTO.getEncounter().getProvenance());
        }
        Observation systemicExamination = fhirAssessmentMapper.createSignsObservation(generalMedicalReviewDTO.getSystemicExaminations(),
                generalMedicalReviewDTO.getEncounter(),
                Constants.SYSTEMIC_EXAMINATIONS, generalMedicalReviewDTO.getSystemicExaminationsNotes());
        if (!Objects.isNull(systemicExamination)) {
            String systemicExaminationRef = fhirAssessmentMapper.addObservationToBundle(systemicExamination, bundle,
                    generalMedicalReviewDTO.getEncounter().getProvenance());
        }
        checkAndCloseRmnchDetails(generalMedicalReviewDTO, bundle);
        fhirUtils.setBundle(urlSign, StringUtil.concatString(Constants.FHIR_BASE_URL),
                Bundle.HTTPVerb.POST,
                observation,
                bundle,
                generalMedicalReviewDTO.getEncounter().getProvenance());
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));
        if (Objects.isNull(responseEntity.getBody())) {
            throw new Validation(1006);
        }
        Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
        Map<String, String> responseMap = new HashMap<>();
        responseMap.put(Constants.KEY_ENCOUNTER_ID, !Objects.isNull(response.get(String.valueOf(ResourceType.Encounter)))
                && !response.get(String.valueOf(ResourceType.Encounter)).isEmpty()
                ? response.get(String.valueOf(ResourceType.Encounter)).getFirst()
                : fhirUtils.getIdFromReference(generalMedicalReviewDTO.getId()));
        responseMap.put(Constants.KEY_PATIENT_REFERENCE,
                !Objects.isNull(response.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                        !response.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                        response.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() :
                        fhirUtils.getIdFromReference(generalMedicalReviewDTO.getEncounter().getPatientReference()));
        return responseMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GeneralMedicalReviewSummaryDetailsDTO getGeneralMedicalReviewDetails(String id, String patientReference) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = fhirAssessmentMapper.getEncounterDetails(id, Boolean.TRUE);
        List<DiagnosisDTO.DiseaseDTO> diseases = new ArrayList<>();
        GeneralMedicalReviewSummaryDetailsDTO generalMedicalReviewDTO = new GeneralMedicalReviewSummaryDetailsDTO();
        generalMedicalReviewDTO.setId(fhirUtils.getIdFromHistoryUrl(id));
        bundle.getEntry().forEach(entry -> {
            ResourceType resourceType = entry.getResource().getResourceType();
            if (ResourceType.Encounter.equals(resourceType)) {
                Encounter encounter = (Encounter) entry.getResource();
                encounter.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem()) && !Objects.isNull(identifier.getValue())) {
                        generalMedicalReviewDTO.setPatientStatus(Constants.PATIENT_STATUS_DISPLAY_NAMES.get(identifier.getValue().toLowerCase()));
                    }
                });
            } else if (ResourceType.Observation.equals(resourceType)) {
                Observation observation = (Observation) entry.getResource();
                observation.getIdentifier().forEach(identifier -> {
                    if (FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL.equals(identifier.getSystem())) {
                        if (Constants.PRESENTING_COMPLAINTS.equals(identifier.getValue())) {
                            generalMedicalReviewDTO.setPresentingComplaints(getSignsFromObservation(observation));
                            generalMedicalReviewDTO.setPresentingComplaintsNotes(
                                    !(observation.getNote().isEmpty()) ? observation.getNote().getFirst().getText() :
                                            null);
                        }
                        if (Constants.SYSTEMIC_EXAMINATIONS.equals(identifier.getValue())) {
                            generalMedicalReviewDTO.setSystemicExaminations(getSignsFromObservation(observation));
                            generalMedicalReviewDTO.setSystemicExaminationsNotes(
                                    !(observation.getNote().isEmpty()) ? observation.getNote().getFirst().getText() :
                                            null);
                        }
                        if (Constants.CLINICAL_NOTES.equals(identifier.getValue())) {
                            generalMedicalReviewDTO.setClinicalNotes(observation.getValueCodeableConcept().getText());
                        }
                    }
                });
            } else if (ResourceType.Condition.equals(resourceType)) {
                diseases.add(fhirAssessmentMapper.mapToDiagnosis((Condition) entry.getResource(), Boolean.TRUE,
                        Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW));
            } else if (ResourceType.Patient.equals(resourceType)) {
                Patient patient = (Patient) entry.getResource();
                generalMedicalReviewDTO.setSummaryStatus(fhirAssessmentMapper.getSummaryStatus(patient));
            }
        });
        generalMedicalReviewDTO.setDiagnosis(diseases);
        generalMedicalReviewDTO.setPrescriptions(prescriptionService.getPrescriptionsByEncounter(id, patientReference));
        generalMedicalReviewDTO.setInvestigations(investigationService.getInvestigationsByEncounter(id, patientReference));
        return generalMedicalReviewDTO;
    }

    /**
     * Get Signs from Observation
     *
     * @param observation Observation Object
     * @return List of signs
     */
    private List<String> getSignsFromObservation(Observation observation) {
        return observation.getComponent()
                .stream()
                .map(component -> fhirUtils.getText(component.getCode().getText()))
                .toList();
    }

    /**
     * Save summary Details
     *
     * @param summaryDTO summary Details
     * @return Summary Object
     */
    public GeneralMedicalReviewSummaryDTO saveSummaryDetails(GeneralMedicalReviewSummaryDTO summaryDTO) {
        EncounterDetailsDTO encounterDetailsDTO = summaryDTO.getEncounter();
        fhirUtils.initiateCodesMap();
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        encounterDetailsDTO.setPatientReference(
                StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH,
                        summaryDTO.getPatientReference()));
        encounterDetailsDTO.setId(
                StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH,
                        summaryDTO.getId()));
        //setMedical Supplies
        if (!Objects.isNull(summaryDTO.getMedicalSupplies()) && !summaryDTO.getMedicalSupplies().isEmpty()) {
            Observation observation = fhirAssessmentMapper.createSignsObservation(summaryDTO.getMedicalSupplies(),
                    encounterDetailsDTO, Constants.MEDICAL_SUPPLIES, null);
            fhirAssessmentMapper.addObservationToBundle(observation, bundle, summaryDTO.getProvenance());
        }
        //set Payment Notice
        createPaymentNotice(summaryDTO, summaryDTO.getId(), bundle);
        processReferralTickets(summaryDTO, bundle);
        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
        return summaryDTO;
    }

    /**
     * Process Referral Tickets for medicalReview
     *
     * @param summaryDTO Summary Details
     * @param bundle     Bundle Object
     */
    private void processReferralTickets(GeneralMedicalReviewSummaryDTO summaryDTO, Bundle bundle) {
        boolean labourDelivery = Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW.equals(
                summaryDTO.getEncounterType());
        String visitIdentifierValue = null;
        if (!Objects.isNull(summaryDTO.getPatientStatus())) {
            Encounter encounter = fhirAssessmentMapper.updateEncounterStatusDetails(summaryDTO.getId(),
                    summaryDTO.getProvenance(),
                    summaryDTO.getPatientStatus(), bundle);
            summaryDTO.setPatientStatus(labourDelivery ? Constants.RECOVERED : summaryDTO.getPatientStatus());
            if (!Objects.isNull(encounter)) {
                visitIdentifierValue = encounter.getIdentifier().stream()
                        .filter(identifier -> FhirIdentifierConstants.VILLAGE_SYSTEM_URL.equals(identifier.getSystem()))
                        .map(Identifier::getValue).findFirst().orElse(null);
            }
            //update Existing Ticket Status
            RequestDTO requestDTO = getRequestDto(summaryDTO,
                    StringUtil.concatString(Constants.ICCM, Constants.COMMA, Constants.RMNCH, Constants.COMMA,
                            Constants.CHILDHOOD_VISIT), Boolean.FALSE);
            patientService.updateReferralTicketByMemberId(requestDTO, bundle);
        }
        if (labourDelivery) {
            ReferralDetailsDTO referralDetailsDTO = getReferralDetailsDTO(summaryDTO, Constants.RMNCH_VISIT,
                    Constants.PNC_VISIT, Constants.MEDICAL_REVIEW);
            patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE, Boolean.TRUE, null);
        } else if (summaryDTO.getCategory().equals(Constants.RMNCH)) {
            processRMNCHTicket(summaryDTO, bundle, visitIdentifierValue);
        } else if (!Objects.isNull(summaryDTO.getNextVisitDate())) {
            ReferralDetailsDTO referralDetailsDTO = getReferralDetailsDTO(summaryDTO, summaryDTO.getCategory(),
                    Constants.MEDICAL_REVIEW, Constants.MEDICAL_REVIEW);
            patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE, Boolean.TRUE, null);
        }
    }

    /**
     * Get Request DTO
     *
     * @param summaryDTO   Summary Details
     * @param category     Category
     * @param closeTickets Close Tickets
     * @return RequestDTO Object
     */
    private RequestDTO getRequestDto(GeneralMedicalReviewSummaryDTO summaryDTO, String category, boolean closeTickets) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientStatus(summaryDTO.getPatientStatus());
        requestDTO.setMemberId(summaryDTO.getMemberId());
        requestDTO.setTicketType(Constants.MEDICAL_REVIEW);
        requestDTO.setCategory(category);
        requestDTO.setProvenance(summaryDTO.getProvenance());
        requestDTO.setEncounterId(
                StringUtil.concatString(String.valueOf(ResourceType.Encounter), Constants.FORWARD_SLASH,
                        summaryDTO.getId()));
        requestDTO.setClosedEncounterType(summaryDTO.getEncounterType());
        requestDTO.setCloseReferralTicket(closeTickets);
        requestDTO.setClosedReason(Constants.MEDICAL_REVIEW);
        requestDTO.setReason(null);
        return requestDTO;
    }

    /**
     * Process RMNCH Ticket
     *
     * @param summaryDTO Summary Details
     * @param bundle     Bundle Object
     */
    private void processRMNCHTicket(GeneralMedicalReviewSummaryDTO summaryDTO, Bundle bundle,
                                    String visitIdentifierValue) {
        //update Existing Ticket Status
        RequestDTO requestDTO = getRequestDto(summaryDTO, Constants.RMNCH_VISIT, Boolean.TRUE);
        patientService.updateReferralTicketByMemberId(requestDTO, bundle);
        String visitReason = Objects.isNull(visitIdentifierValue) ? Constants.MEDICAL_REVIEW :
                String.format(Constants.RMNCH_VISIT_REASON, summaryDTO.getEncounterType(), visitIdentifierValue);
        ReferralDetailsDTO referralDetailsDTO = getReferralDetailsDTO(summaryDTO, Constants.RMNCH_VISIT, visitReason, Constants.MEDICAL_REVIEW);
        patientService.createReferralTicket(referralDetailsDTO, bundle, Boolean.FALSE, Boolean.TRUE, null);
        if (summaryDTO.getPatientStatus().toLowerCase().contains(Constants.ON_TREATMENT.toLowerCase())) {
            ReferralDetailsDTO referralDetailsDTOForStatus = getReferralDetailsDTO(summaryDTO, Constants.RMNCH,
                    Constants.MEDICAL_REVIEW, Constants.MEDICAL_REVIEW);
            patientService.createReferralTicket(referralDetailsDTOForStatus, bundle, Boolean.FALSE, Boolean.TRUE, null);
        }
    }

    /**
     * Set ReferralTicket Details
     *
     * @param summaryDTO Assessment Details
     * @return ReferralDetailsDTO
     */
    private ReferralDetailsDTO getReferralDetailsDTO(GeneralMedicalReviewSummaryDTO summaryDTO, String category,
                                                     String reason, String type) {
        ReferralDetailsDTO referralDetailsDTO = new ReferralDetailsDTO();
        referralDetailsDTO.setProvenance(summaryDTO.getProvenance());
        referralDetailsDTO.setPatientReference(summaryDTO.getPatientReference());
        referralDetailsDTO.setReferred(Boolean.FALSE);
        referralDetailsDTO.setNextVisitDate(summaryDTO.getNextVisitDate());
        referralDetailsDTO.setType(type);
        referralDetailsDTO.setReferredReason(reason);
        referralDetailsDTO.setEncounterId(summaryDTO.getId());
        referralDetailsDTO.setEncounterType(summaryDTO.getEncounterType());
        referralDetailsDTO.setPatientStatus(
                Constants.RMNCH.equalsIgnoreCase(summaryDTO.getCategory()) ? Constants.ON_TREATMENT :
                        summaryDTO.getPatientStatus());
        referralDetailsDTO.setCategory(category);
        referralDetailsDTO.setMemberId(summaryDTO.getMemberId());
        return referralDetailsDTO;
    }

    /**
     * Create Payment Notice For the cost
     *
     * @param summaryDTO     Summary Details
     * @param observationRef reference
     * @param bundle         Bundle Object
     */
    private void createPaymentNotice(GeneralMedicalReviewSummaryDTO summaryDTO, String observationRef, Bundle bundle) {
        if (!Objects.isNull(summaryDTO.getCost())) {
            //set Payment Notice
            String uuid = fhirUtils.getUniqueId();
            String id = StringUtil.concatString(String.valueOf(ResourceType.PaymentNotice),
                    Constants.FORWARD_SLASH,
                    Constants.FHIR_BASE_URL,
                    uuid);
            PaymentNotice paymentNotice = new PaymentNotice();
            paymentNotice.setCreated(new Date());
            paymentNotice.setAmount(new Money().setValue(Constants.ZERO));
            paymentNotice.setStatus(PaymentNotice.PaymentNoticeStatus.ACTIVE);
            paymentNotice.setPaymentStatus(new CodeableConcept().setText(summaryDTO.getCost()));
            paymentNotice.setRequest(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Encounter),
                    Constants.FORWARD_SLASH,
                    observationRef)));
            fhirUtils.setBundle(id,
                    StringUtil.concatString(Constants.FHIR_BASE_URL, uuid),
                    Bundle.HTTPVerb.POST,
                    paymentNotice,
                    bundle,
                    summaryDTO.getProvenance());
        }
    }

    /**
     * Check and close RMNCH details
     *
     * @param generalMedicalReviewDTO General medical review Details
     * @param bundle                  Bundle object
     */
    public void checkAndCloseRmnchDetails(GeneralMedicalReviewDTO generalMedicalReviewDTO, Bundle bundle) {
        HouseholdMemberDTO householdMemberDTO = householdService.getHouseholdMemberById(
                generalMedicalReviewDTO.getEncounter().getMemberId());
        PregnancyInfo pregnancyInfo = getPatientVitals(generalMedicalReviewDTO.getEncounter().getMemberId());
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setProvenance(generalMedicalReviewDTO.getEncounter().getProvenance());
        requestDTO.setMemberId(generalMedicalReviewDTO.getEncounter().getMemberId());
        requestDTO.setEncounterId(generalMedicalReviewDTO.getEncounter().getId());
        requestDTO.setClosedEncounterType(Constants.ABOVE_5_GENERAL_MEDICAL_REVIEW);
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.CHILD_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_CHILHOOD_REASON);
            patientService.closeChildhoodDetails(bundle, requestDTO);
        }
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_CHILD_REASON);
            patientService.closePncNeonateDetails(bundle, requestDTO);
        }
        Date pncDeliveryDate = Objects.nonNull(pregnancyInfo.getDateOfDelivery()) ? pregnancyInfo.getDateOfDelivery() :
                pregnancyInfo.getPncCreatedDate();
        if (Objects.nonNull(pncDeliveryDate) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(pncDeliveryDate)) {
            requestDTO.setClosedReason(Constants.AUTO_CLOSE_PNC_MOTHER_REASON);
            patientService.closePncDetails(bundle, requestDTO);
        }
    }

    /**
     * Get patient vitals information from member
     *
     * @param memberId - member Id
     * @return Pregnancy Info map
     */
    private PregnancyInfo getPatientVitals(String memberId) {
        RequestDTO request = new RequestDTO();
        request.setMemberId(memberId);
        return patientService.getPatientVitals(request);
    }

    /**
     * {@inheritDoc}
     */
    public PatientStatusDTO createPatientStatus(PatientStatusDTO patientStatusDto) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        fhirUtils.initiateCodesMap();
        patientStatusDto.getProvenance().setModifiedDate(new Date());
        String url = String.format(Constants.PATIENT_STATUS_CONDITION_QUERY, patientStatusDto.getPatientReference(),
                Constants.PATIENT_STATUS_FIELD);
        Bundle resultBundle = restApiUtil.getBatchRequest(url);

        AtomicReference<Condition> diabetesCondition = new AtomicReference<>();
        AtomicReference<Condition> hypertensionCondition = new AtomicReference<>();
        AtomicReference<Condition> mentalHealthCondition = new AtomicReference<>();
        AtomicReference<Condition> substanceCondition = new AtomicReference<>();
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
                    if (condition.getIdentifier().getFirst().getValue().equals(Constants.MENTAL_HEALTH_STATUS)) {
                        mentalHealthCondition.set(condition);
                    }
                    if (condition.getIdentifier().getFirst().getValue().equals(Constants.SUBSTANCE_DISORDER)) {
                        substanceCondition.set(condition);
                    }
                });
        Patient patient = null;
        if (Objects.isNull(patientStatusDto.getPatientReference())) {
            url = String.format(Constants.GET_MEMBER_ID, patientStatusDto.getMemberReference());
            resultBundle = restApiUtil.getBatchRequest(url);
            RelatedPerson relatedPerson = (RelatedPerson) resultBundle.getEntry().getFirst().getResource();
            AssessmentDTO assessmentDTO = new AssessmentDTO();
            spiceConverter.setPatientBioDetails(relatedPerson, assessmentDTO);
            patient = patientConverter.createPatient(patient, assessmentDTO.getBioData(),
                    assessmentDTO.getBioMetrics(), assessmentDTO.getBioMetrics().getDateOfBirth());
            patient.setManagingOrganization(new
                    Reference(String.format(FhirConstants.ORGANIZATION_ID,
                    patientStatusDto.getProvenance().getOrganizationId())));
            commonConverter.setPatientAndRelatedPersonLink(patient, relatedPerson);
            setPatientStatus(relatedPerson, patient);
            commonConverter.setRelatedPersonDetailsInBundle(bundle, relatedPerson, null, patientStatusDto.getProvenance());
        }
        commonConverter.setPatientDetailsInBundle(bundle, patient,
                FhirConstants.PATIENT_IDENTIFIER_URL, patientStatusDto.getProvenance());
        commonConverter.createOrUpdateDiabetesAndHypertensionPatientStatus(patientStatusDto, bundle, diabetesCondition,
                hypertensionCondition, Objects.nonNull(diabetesCondition.get()));
        commonConverter.createOrUpdateMentalHealthPatientStatus(patientStatusDto, bundle, mentalHealthCondition,
                substanceCondition);
        if (Objects.nonNull(patientStatusDto.getNcdPatientStatus()) && (Constants.KNOWN_PATIENT.equals(patientStatusDto.getNcdPatientStatus().getDiabetesStatus())
                || Constants.KNOWN_PATIENT.equals(patientStatusDto.getNcdPatientStatus().getHypertensionStatus()))) {
            Condition confirmedNCDCondition = commonConverter.getConfirmDiagnosis(patientStatusDto.getPatientReference(), Constants.NCD);
            commonConverter.updateNCDKnownStatus(patientStatusDto, bundle, confirmedNCDCondition, Objects.nonNull(confirmedNCDCondition));
        }

        if ((Objects.nonNull(patientStatusDto.getMentalHealthStatus()) && Constants.KNOWN_PATIENT.equals(patientStatusDto.getMentalHealthStatus().getStatus()))
                || (Objects.nonNull(patientStatusDto.getSubstanceUseStatus()) && Constants.KNOWN_PATIENT.equals(patientStatusDto.getSubstanceUseStatus().getStatus()))) {
            Condition confirmedMentalCondition = commonConverter.getConfirmDiagnosis(patientStatusDto.getPatientReference(), Constants.MENTAL_HEALTH);
            commonConverter.updateMentalHealthKnownStatus(patientStatusDto, bundle, confirmedMentalCondition, Objects.nonNull(confirmedMentalCondition));
        }
        ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                restApiUtil.constructRequestEntity(bundle));

        if (Objects.isNull(patientStatusDto.getPatientReference())) {
            if (Objects.isNull(responseEntity.getBody())) {
                throw new Validation(1006);
            }
            Map<String, List<String>> response = fhirUtils.getFhirIdsFromResponse(responseEntity.getBody());
            patientStatusDto.setPatientReference(!Objects.isNull(response.get(Constants.FHIR_RESOURCE_PATIENT)) &&
                    !response.get(Constants.FHIR_RESOURCE_PATIENT).isEmpty() ?
                    response.get(Constants.FHIR_RESOURCE_PATIENT).getFirst() : null);
        }
        return patientStatusDto;
    }

    /**
     * This methis used to set the patient status in patient resource.
     */
    private void setPatientStatus(RelatedPerson relatedPerson, Patient patient) {
        relatedPerson.getIdentifier().forEach(identifier -> {
            if (FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL.equals(identifier.getSystem())) {
                patient.addIdentifier().setSystem(FhirIdentifierConstants.PATIENT_STATUS_SYSTEM_URL).setValue(identifier.getValue());
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    public PatientStatusDTO getPatientStatusDetails(PatientStatusDTO patientStatusDto) {
        String url = String.format(Constants.PATIENT_STATUS_CONDITION_QUERY, patientStatusDto.getPatientReference(),
                Constants.PATIENT_STATUS_FIELD);
        Bundle resultBundle = restApiUtil.getBatchRequest(url);
        PatientStatusDTO response = new PatientStatusDTO();
        AtomicReference<Condition> diabetesCondition = new AtomicReference<>();
        AtomicReference<Condition> hypertensionCondition = new AtomicReference<>();
        AtomicReference<Condition> mentalHealthCondition = new AtomicReference<>();
        AtomicReference<Condition> substanceCondition = new AtomicReference<>();
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
                    if (condition.getIdentifier().getFirst().getValue().equals(Constants.MENTAL_HEALTH_STATUS)) {
                        mentalHealthCondition.set(condition);
                    }
                    if (condition.getIdentifier().getFirst().getValue().equals(Constants.SUBSTANCE_DISORDER)) {
                        substanceCondition.set(condition);
                    }
                });
        if (Objects.nonNull(mentalHealthCondition.get())) {
            response.setMentalHealthStatus(setMentalHealthPatientStatus(mentalHealthCondition.get()));
        }

        if (Objects.nonNull(substanceCondition.get())) {
            response.setSubstanceUseStatus(setMentalHealthPatientStatus(substanceCondition.get()));
        }

        if (Objects.nonNull(diabetesCondition.get()) && Objects.nonNull(hypertensionCondition.get())) {
            return setDiabetesAndHypertensionPatientStatus(diabetesCondition.get(),
                    hypertensionCondition.get(), response);
        }
        return null;
    }

    /**
     * <p>
     * Used to convert the condition resource to patient status
     * <p/>
     *
     * @param diabetesCondition     - Patient's latest diabetes condition
     * @param hypertensionCondition - Patient's latest hypertension condition
     * @param patientStatusDto      - Response object for patient status details
     * @return {@link PatientStatusDTO} - Patient status details
     */
    private PatientStatusDTO setDiabetesAndHypertensionPatientStatus(Condition diabetesCondition,
                                                                     Condition hypertensionCondition,
                                                                     PatientStatusDTO patientStatusDto) {
        NcdPatientStatus ncdPatientStatus = new NcdPatientStatus();
        ncdPatientStatus.setDiabetesStatus(diabetesCondition.getVerificationStatus().getText());
        ncdPatientStatus.setDiabetesControlledType(diabetesCondition.getCode().getText());
        ncdPatientStatus.setDiabetesYearOfDiagnosis(diabetesCondition.getOnsetStringType().getValue());
        ncdPatientStatus.setHypertensionStatus(hypertensionCondition.getVerificationStatus().getText());
        ncdPatientStatus.setHypertensionYearOfDiagnosis(hypertensionCondition.getOnsetStringType().getValue());
        patientStatusDto.setNcdPatientStatus(ncdPatientStatus);
        patientStatusDto.setPatientReference(patientStatusDto.getPatientReference());
        return patientStatusDto;
    }

    /**
     * {@inheritDoc}
     */
    public void updateConfirmDiagnosis(ConfirmDiagnosisDTO confirmDiagnosisDTO) {
        Bundle bundle = new Bundle().setType(Bundle.BundleType.TRANSACTION);
        fhirUtils.initiateCodesMap();
        AtomicReference<String> uuid = new AtomicReference<>();
        Map<String, List<String>> diagnosisCategories = new HashMap<>();
        Map<String, List<String>> diagnosisMap = new HashMap<>();
        diagnosisMap.put(Constants.OTHER, null);
        List<String> requestDiagnosis = new ArrayList<>();
        for (DiagnosisDTO diagnosis : confirmDiagnosisDTO.getConfirmDiagnosis()) {
            requestDiagnosis.add(diagnosis.getType());
            diagnosisCategories = getDiagnosisCategoryByValue(diagnosis, diagnosisMap);
            if (diagnosis.getType().equals(Constants.OTHER)) {
                diagnosisCategories.put(Constants.OTHER, List.of(Constants.OTHER_LOWER_CASE));
            }
        }

        if (diagnosisCategories.isEmpty() || (diagnosisCategories.size() == Constants.ONE
                && diagnosisCategories.containsKey(Constants.OTHER))) {
            diagnosisCategories.put(confirmDiagnosisDTO.getType(), new ArrayList<>());
        }

        diagnosisCategories.forEach(
                (type, diagnosis) ->
                {
                    uuid.set(fhirUtils.getUniqueId());
                    Condition condition = commonConverter.getConfirmDiagnosis(confirmDiagnosisDTO.getPatientReference(), type);
                    Condition diagnosisCondition = patientStatusConverter.updateConfirmedDiagnosis(confirmDiagnosisDTO,
                            condition, type, diagnosis);
                    patientStatusConverter.setReference(diagnosisCondition, confirmDiagnosisDTO.getPatientReference(),
                            null, confirmDiagnosisDTO.getMemberReference());
                    commonConverter.setConditionInBundle(bundle, diagnosisCondition,
                            FhirConstants.PATIENT_DIAGNOSIS_IDENTIFIER_URL.concat(uuid.get()), Objects.nonNull(condition),
                            confirmDiagnosisDTO.getProvenanceDTO());
                }
        );
        if(!requestDiagnosis.contains(Constants.HIV.toUpperCase())) {
            Condition condition = commonConverter.getConfirmDiagnosis(confirmDiagnosisDTO.getPatientReference(), Constants.HIV.toUpperCase());
            if(Objects.nonNull(condition)) {
                condition.setVerificationStatus(fhirUtils.setCodes(Constants.UNCONFIRMED));
                commonConverter.setConditionInBundle(bundle, condition,
                        FhirConstants.PATIENT_DIAGNOSIS_IDENTIFIER_URL.concat(uuid.get()), Objects.nonNull(condition),
                        confirmDiagnosisDTO.getProvenanceDTO());
            }
        }
        restApiUtil.postBatchRequest(fhirServerUrl, restApiUtil.constructRequestEntity(bundle));
    }

    /**
     * <p>
     * This function categorizes a diagnosis based on its type and value into a map of diagnosis categories.
     * </p>
     *
     * @param diagnosisDTO {@link DiagnosisDTO} It contains information about a diagnosis such as type and value.
     * @param diagnosisMap {@link Map} The Diagnosis type and value is saved in this
     * @return {@link Map} containing diagnosis categories mapped to a list of diagnosis values.
     */
    private Map<String, List<String>> getDiagnosisCategoryByValue(DiagnosisDTO diagnosisDTO, Map<String, List<String>> diagnosisMap) {
        List<String> list = new ArrayList<>();
        for (Map.Entry<String, List<String>> category : FhirConstants.CONFIRM_DIAGNOSIS_CATEGORY.entrySet()) {
            if (category.getValue().contains(diagnosisDTO.getType())) {
                if (diagnosisMap.containsKey(category.getKey())) {
                    diagnosisMap.get(category.getKey()).add(diagnosisDTO.getValue());
                } else {
                    list.add(diagnosisDTO.getValue());
                    diagnosisMap.put(category.getKey(), list);
                }

            }
        }
        return diagnosisMap;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Integer> getMedicalReviewCount(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference())) {
            throw new DataNotFoundException(2003);
        }
        int nonReviewedTestCount = investigationService.getLabtestCount(request);
        int psychologyReviewedCount = getObservationCount(request, Constants.PRELIMINARY, Constants.OBSERVATION_PSYCHOLOGY_ASSESSMENT);
        int nutritionLifestyleReviewedCount = getObservationCount(request, Constants.AMENDED,
                Constants.OBSERVATION_NUTRITION_LIFESTYLE);
        int prescriptionDaysCompletedCount = prescriptionService.getPrescriptionCount(request);
        return Map.of(Constants.COUNT_PRESCRIPTION_DAYS_COMPLETED, prescriptionDaysCompletedCount,
                Constants.COUNT_NON_REVIEWED_TEST, nonReviewedTestCount, Constants.COUNT_NUTRITION_LIFESTYLE_REVIED,
                nutritionLifestyleReviewedCount, Constants.COUNT_PSYCHOLOGY_REVIEWED, psychologyReviewedCount);
    }

    /**
     * <p>
     * To get count of Observations of a patient.
     * </p>
     *
     * @param request    {@link RequestDTO} - Request data with patient reference ID.
     * @param status     {@link String} - Observation status
     * @param identifier {@link String} - identifier value
     * @return {@link int} - Count of Observation count
     */
    private int getObservationCount(RequestDTO request, String status, String identifier) {
        String psychologyCountUrl = String.format(
                Constants.GET_OBSERVATION_BY_IDENTIFIER + Constants.AND + Constants.TOTAL_COUNT_PARAM, status,
                FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL + Constants.VERTICAL_BAR + identifier,
                Constants.PATIENT.concat(Constants.FORWARD_SLASH).concat(request.getPatientReference()));
        Bundle bundle = restApiUtil.getBatchRequest(psychologyCountUrl);
        return bundle.getTotal();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean updateViewCount(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference()) || Objects.isNull(request.getMenuName())) {
            throw new DataNotAcceptableException(1036);
        }
        boolean isUpdated = false;
        String url = constructObservationCountUrl(request);
        if (Objects.nonNull(url)) {
            Bundle observationBundle = restApiUtil.getBatchRequest(url);
            Bundle postBundle = new Bundle();
            if (Objects.nonNull(observationBundle) && !observationBundle.getEntry().isEmpty()) {
                for (Bundle.BundleEntryComponent entry : observationBundle.getEntry()) {
                    if (entry.getResource() instanceof Observation observation) {
                        observation.setStatus(Observation.ObservationStatus.FINAL);
                        if (!observation.getPerformer().isEmpty()) {
                            observation.getPerformer().removeIf(performer -> performer.getReference().toLowerCase()
                                    .contains(ResourceType.Organization.toString().toLowerCase()));
                            observation.getPerformer().add(new Reference(
                                    StringUtil.concatString(ResourceType.Organization.toString(),
                                            Constants.FORWARD_SLASH, request.getProvenance().getOrganizationId())));
                            fhirUtils.setBundle(String.valueOf(ResourceType.Observation).concat(Constants.FORWARD_SLASH)
                                            .concat(observation.getIdPart()), Constants.EMPTY_SPACE, Bundle.HTTPVerb.PUT,
                                    observation, postBundle, request.getProvenance());
                        }
                    }
                }
                ResponseEntity<FhirResponseDTO> responseEntity = restApiUtil.postBatchRequest(fhirServerUrl,
                        restApiUtil.constructRequestEntity(postBundle));
                isUpdated = responseEntity.getStatusCode().equals(HttpStatus.OK);
            }
        }
        return isUpdated;
    }

    /**
     * <p>
     * This method constructs the URL to get Observation for Patient Nutrition lifestyle and Patient psychology
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains Patient reference and Menu name.
     * @return a {@link String} of URL
     */
    private String constructObservationCountUrl(RequestDTO request) {
        String url = null;
        if (Constants.LIFESTYLE_REVIEW_STATUS.equals(request.getMenuName())) {
            url = String.format(Constants.GET_OBSERVATION_BY_IDENTIFIER, Constants.AMENDED,
                    FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL + Constants.VERTICAL_BAR
                            + Constants.OBSERVATION_NUTRITION_LIFESTYLE, request.getPatientReference());

        } else if (Constants.PSYCHOLOGICAL_REVIEW_STATUS.equals(request.getMenuName())) {
            url = String.format(Constants.GET_OBSERVATION_BY_IDENTIFIER, Constants.PRELIMINARY,
                    FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL + Constants.VERTICAL_BAR + Constants.OBSERVATION_PSYCHOLOGY_ASSESSMENT,
                    Constants.PATIENT.concat(Constants.FORWARD_SLASH).concat(request.getPatientReference()));
        }
        return url;
    }

    /**
     * <p>
     * Used to convert the condition resource to patient status
     * <p/>
     *
     * @param mentalHealthCondition     - Patient's latest mental condition
     * @return {@link MentalHealthStatus} - Mental health status details is mapped
     */
    private MentalHealthStatus setMentalHealthPatientStatus(Condition mentalHealthCondition) {
        MentalHealthStatus mentalHealthStatus = new MentalHealthStatus();
        mentalHealthStatus.setMentalHealthDisorder(new ArrayList<>());
        mentalHealthStatus.setStatus(mentalHealthCondition.getVerificationStatus().getText());
        mentalHealthStatus.setYearOfDiagnosis(mentalHealthCondition.getOnsetStringType().getValue());
        mentalHealthStatus.setComments((Objects.nonNull(mentalHealthCondition.getNote()) &&
                !mentalHealthCondition.getNote().isEmpty())
                ? mentalHealthCondition.getNote().getFirst().getText() : null);
        if (mentalHealthCondition.hasCategory()) {
            mentalHealthCondition.getCategory().forEach(category ->
                    mentalHealthStatus.getMentalHealthDisorder().add(category.getText()));
        }
        return mentalHealthStatus;
    }
}