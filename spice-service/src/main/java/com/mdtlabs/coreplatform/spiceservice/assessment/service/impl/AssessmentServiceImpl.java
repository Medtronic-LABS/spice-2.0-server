package com.mdtlabs.coreplatform.spiceservice.assessment.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.RedRiskNotification;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.NotificationApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.RiskAlgorithm;
import com.mdtlabs.coreplatform.spiceservice.common.RiskLevelAlgorithm;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RiskAlgorithmDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.pregnancy.PregnancySymptomDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.common.model.Message;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.spiceservice.common.dto.SymptomDTO;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import org.modelmapper.ModelMapper;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.Repository.RedRiskNotificationRepository;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the AssessmentService interface.
 * <p>
 * This class provides the business logic for assessment operations, including creating assessments
 * and managing follow-up actions based on the assessment outcomes.
 * </p>
 */
@Service
public class AssessmentServiceImpl implements AssessmentService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final FollowUpService followUpService;

    private final CallRegisterRepository callRegisterRepository;

    @Value("${app.successful-call-attempts}")
    private int successfulCallAttempts;
    private final RedRiskNotificationRepository redRiskNotificationRepository;

    @Value("${app.enable-red-risk-notification}")
    private boolean enableRedRiskNotification;

    private final UserServiceApiInterface userApiInterface;

    private final NotificationApiInterface notificationApiInterface;

    private com.mdtlabs.coreplatform.spiceservice.common.CommonUtil spiceUtil;

    private final RiskAlgorithm riskAlgorithm;

    private final StaticDataService staticDataService;

    private final CustomizedModulesService customizedModulesService;

    private final RiskLevelAlgorithm riskLevelAlgorithm;


    public AssessmentServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, FollowUpService followUpService, CallRegisterRepository callRegisterRepository, RedRiskNotificationRepository redRiskNotificationRepository, UserServiceApiInterface userApiInterface, NotificationApiInterface notificationApiInterface, RiskAlgorithm riskAlgorithm, StaticDataService staticDataService, CustomizedModulesService customizedModulesService,
                                 RiskLevelAlgorithm riskLevelAlgorithm) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.followUpService = followUpService;
        this.callRegisterRepository = callRegisterRepository;
        this.redRiskNotificationRepository = redRiskNotificationRepository;
        this.userApiInterface = userApiInterface;
        this.notificationApiInterface = notificationApiInterface;
        this.riskAlgorithm = riskAlgorithm;
        this.staticDataService = staticDataService;
        this.customizedModulesService = customizedModulesService;
        this.riskLevelAlgorithm = riskLevelAlgorithm;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AssessmentDTO createAssessment(AssessmentDTO assessmentDTO) {
        if (Constants.NON_COMMUNITY.equals(assessmentDTO.getAssessmentType())) {
            assessmentDTO = createNcdAssessment(assessmentDTO);
        } else {
            setReferralTicketType(assessmentDTO);
            preAssessmentFollowUp(assessmentDTO);
            AssessmentDTO patientAssessmentDTO = fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(), assessmentDTO);
            assessmentDTO.setId(patientAssessmentDTO.getId());
            assessmentDTO.setReferralTicketType(patientAssessmentDTO.getReferralTicketType());
            postAssessmentFollowUp(assessmentDTO);
        }
        return assessmentDTO;
    }

    /**
     * Sets the referral ticket type for the assessment based on the assessment type.
     *
     * @param assessmentDTO The assessment DTO to update with the referral ticket type.
     */
    private void setReferralTicketType(AssessmentDTO assessmentDTO) {
        String assessmentType = assessmentDTO.getAssessmentType();
        if (Constants.ICCM.equals(assessmentType) || Constants.OTHER_SYMPTOMS.equals(assessmentType)) {
            assessmentDTO.setReferralTicketType(Constants.ICCM);
        } else if (Constants.ANC.equals(assessmentType) || Constants.PNC_MOTHER.equals(assessmentType) ||
                Constants.PNC_NEONATE.equals(assessmentType)) {
            assessmentDTO.setReferralTicketType(Constants.RMNCH);
        } else if (Constants.CHILDHOOD_VISIT.equals(assessmentType)) {
            assessmentDTO.setReferralTicketType(Constants.CHILDHOOD_VISIT);
        }
    }

    /**
     * Performs pre-assessment follow-up actions, such as closing follow-ups based on certain conditions.
     *
     * @param assessmentDTO The assessment DTO to use for pre-assessment follow-up actions.
     */
    private void preAssessmentFollowUp(AssessmentDTO assessmentDTO) {
        boolean hasNextVisitDate = Objects.nonNull(assessmentDTO.getSummary()) && Objects.nonNull(assessmentDTO.getSummary().getNextVisitDate());
        String assessmentType = assessmentDTO.getAssessmentType();
        if (hasNextVisitDate) {
            createAutoReferralTicket(assessmentDTO);
        }
        if (Objects.nonNull(assessmentDTO.getFollowUpId())) {
            ModelMapper mapper = new ModelMapper();
            CallRegister callRegister = callRegisterRepository.findByIdAndIsDeletedFalse(assessmentDTO.getFollowUpId());
            assessmentDTO.setFollowUp(mapper.map(callRegister, FollowUpDTO.class));
        }
        if (Constants.ANC.equals(assessmentType) || Constants.PNC_MOTHER.equals(assessmentType)) {
            checkVisitAndClosePregnancy(assessmentDTO);
            boolean isMiscarriage = Constants.ANC.equals(assessmentType) &&
                    Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getAnc().getMiscarriage());
            if (isMiscarriage) {
                closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()),
                        List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW),
                        null, null);
            }
        }
        if (Constants.ICCM.equals(assessmentDTO.getReferralTicketType())) {
            checkAndCloseRmnchDetails(assessmentDTO);
        }
        if (Constants.CHILDHOOD_VISIT.equals(assessmentDTO.getReferralTicketType())) {
            HouseholdMemberDTO householdMemberDTO = getHouseholdMemberById(assessmentDTO.getEncounter().getMemberId());
            if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                    Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
                closeFollowUps(assessmentDTO, List.of(Constants.RMNCH), List.of(AppointmentType.HH_VISIT,
                        AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), Constants.PNC_NEONATE, null);
            }
        }
    }

    /**
     * Executes post-assessment follow-up actions based on the referral ticket type.
     * <p>
     * This method determines the type of follow-up required after an assessment has been completed,
     * based on the referral ticket type specified in the assessment DTO. It supports follow-up actions
     * for both ICCM and RMNCH referral ticket types.
     * </p>
     *
     * @param assessmentDTO The assessment DTO containing information about the completed assessment,
     *                      including the referral ticket type.
     */
    private void postAssessmentFollowUp(AssessmentDTO assessmentDTO) {
        String referralTicketType = assessmentDTO.getReferralTicketType();
        if (Constants.ICCM.equals(referralTicketType)) {
            createICCMFollowUp(assessmentDTO);
        } else if (Constants.RMNCH.equals(referralTicketType) || Constants.CHILDHOOD_VISIT.equals(referralTicketType)) {
            createRMNCHFollowUp(assessmentDTO);
        }
        if (Objects.nonNull(assessmentDTO.getFollowUp())) {
            CallRegister callRegister = callRegisterRepository.findByIdAndIsDeletedFalse(
                    assessmentDTO.getFollowUp().getId());
            callRegister.setVisits(callRegister.getVisits() + Constants.ONE);
            callRegisterRepository.save(callRegister);
        }
    }

    /**
     * Creates follow-up actions specific to ICCM assessments.
     * <p>
     * This method handles the creation of follow-up actions for assessments categorized under ICCM.
     * It checks if the patient is referred, has a next visit date, or if there's an existing follow-up
     * that matches the referral ticket type. Based on these conditions, it either closes existing follow-ups
     * or creates new follow-up actions.
     * </p>
     *
     * @param assessmentDTO The assessment DTO containing details about the patient, encounter, and assessment
     *                      outcomes that dictate the follow-up actions.
     */
    private void createICCMFollowUp(AssessmentDTO assessmentDTO) {
        boolean isReferred = assessmentDTO.getEncounter().isReferred();
        boolean isRecovered = Constants.RECOVERED.equals(assessmentDTO.getPatientStatus());
        String referralTicketType = assessmentDTO.getReferralTicketType();
        boolean hasNextVisitDate = Objects.nonNull(assessmentDTO.getSummary()) && Objects.nonNull(assessmentDTO.getSummary().getNextVisitDate());
        if (isReferred) {
            closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.HH_VISIT), null, assessmentDTO.getReferredReasons());
            createFollowUp(assessmentDTO,
                    AppointmentType.REFERRED, assessmentDTO.getReferredReasons());
        } else if (hasNextVisitDate) {
            createFollowUp(assessmentDTO,
                    AppointmentType.HH_VISIT, assessmentDTO.getReferredReasons());
        } else if (isRecovered) {
            closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.REFERRED), null, Constants.MUAC_UPPER_CASE);
            if (Objects.nonNull(assessmentDTO.getFollowUp()) && assessmentDTO.getFollowUp().getEncounterType().equals(referralTicketType)) {
                closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.HH_VISIT,
                        AppointmentType.REFERRED), null, assessmentDTO.getFollowUp().getReason());
            }
        }
    }

    /**
     * Creates follow-up actions specific to RMNCH assessments.
     * <p>
     * This method orchestrates the creation of follow-up actions for assessments categorized under RMNCH (Reproductive, Maternal, Neonatal, and Child Health).
     * It evaluates various conditions such as whether the patient is referred, has a next visit date, experienced a miscarriage, or is at their first visit for ANC/PNC.
     * Based on these conditions, it appropriately closes existing follow-ups and creates new follow-up actions as needed.
     * </p>
     *
     * @param assessmentDTO The assessment DTO containing details about the patient, encounter, and assessment outcomes that dictate the follow-up actions.
     */
    private void createRMNCHFollowUp(AssessmentDTO assessmentDTO) {
        String assessmentType = assessmentDTO.getAssessmentType();
        boolean isReferred = assessmentDTO.getEncounter().isReferred();
        boolean isRecovered = Constants.RECOVERED.equals(assessmentDTO.getPatientStatus());
        boolean hasNextVisitDate = Objects.nonNull(assessmentDTO.getSummary()) &&
                Objects.nonNull(assessmentDTO.getSummary().getNextVisitDate());
        boolean isMotherDead = Constants.ANC.equals(assessmentType) &&
                Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getAnc().getDeathOfMother());
        boolean isPncChildDead = Constants.CHILDHOOD_VISIT.equals(assessmentType) &&
                Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getPncChild().getDeathOfBaby());
        boolean isPncNeonatalChildDead = Constants.PNC_NEONATE.equals(assessmentType) &&
                Boolean.TRUE.equals(assessmentDTO.getAssessmentDetails().getPncNeonatal().getDeathOfNewborn());
        if(isMotherDead || isPncChildDead || isPncNeonatalChildDead) {
            closeFollowUps(assessmentDTO, List.of(Constants.ICCM, Constants.RMNCH, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW),
                    null, null);
            return;
        }
        closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.HH_VISIT), null,
                null);
        if (isRecovered) {
            closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.REFERRED), null,
                    null);
        }
        if (isReferred) {
            createFollowUp(assessmentDTO, AppointmentType.REFERRED, assessmentDTO.getReferredReasons());
        }
        if (hasNextVisitDate) {
            createFollowUp(assessmentDTO, AppointmentType.HH_VISIT,
                    String.format(Constants.RMNCH_VISIT_REASON, Constants.RMNCH_VISIT_MAPPING.get(assessmentType),
                            assessmentDTO.getEncounter().getVisitNumber()));
        }
    }

    /**
     * Closes existing follow-ups based on the referral ticket type and specific appointment types.
     * <p>
     * This method identifies and closes existing follow-ups for a given patient based on the referral ticket type and a list of appointment types.
     * It filters existing follow-ups that match the given criteria and marks them as completed. This is a preparatory step before creating new follow-up actions.
     * </p>
     *
     * @param assessmentDTO       The assessment DTO containing patient and encounter information.
     * @param referralTicketTypes A list of referral ticket type used to filter follow-ups.
     * @param appointmentTypes    A list of appointment types used to filter follow-ups.
     * @param encounterName       Encounter name
     * @param reason              An optional reason for closing the follow-ups, used for further filtering.
     */
    private void closeFollowUps(AssessmentDTO assessmentDTO, List<String> referralTicketTypes, List<AppointmentType> appointmentTypes,
                                String encounterName, String reason) {
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                assessmentDTO.getEncounter().getMemberId(), referralTicketTypes, appointmentTypes, Boolean.FALSE);
        if (Objects.nonNull(reason)) {
            List<String> reasons = Arrays.stream(reason.split(Constants.COMMA))
                    .map(String::trim).sorted().toList();
            callRegisters = callRegisters.stream().filter(callRegister -> Objects.nonNull(callRegister.getReason()))
                    .filter(callRegister -> reasons.equals(Arrays.stream(callRegister.getReason().split(Constants.COMMA))
                            .map(String::trim).sorted().toList())).toList();
        }
        if (Objects.nonNull(encounterName)) {
            callRegisters = callRegisters.stream().filter(callRegister -> encounterName.equals(callRegister.getEncounterName())).toList();
        }
        callRegisters.forEach(callRegister -> {
            callRegister.setIsCompleted(Boolean.TRUE);
            callRegister.setLastInteractionMode(InteractionMode.ASSESSMENT);
            callRegister.setUpdatedBy(assessmentDTO.getEncounter().getProvenance().getSpiceUserId());
            callRegisterRepository.save(callRegister);
        });
    }

    /**
     * Creates a new follow-up action based on the assessment outcomes.
     * <p>
     * This method constructs and creates a new follow-up action for a patient based on the outcomes of their assessment.
     * It determines the type of follow-up required (e.g., home visit, medical referral) and the reasons for the follow-up.
     * The new follow-up action is then persisted through the follow-up service.
     * </p>
     *
     * @param assessmentDTO   The assessment DTO containing patient and encounter information, and the basis for the follow-up action.
     * @param appointmentType The type of follow-up appointment to create.
     * @param reason          The reason for the follow-up, which may include referral reasons or visit reasons.
     */
    private void createFollowUp(AssessmentDTO assessmentDTO,
                                AppointmentType appointmentType, String reason) {
        FollowUpDTO followUpDTO = constructAssessmentFollowUpDetails(assessmentDTO, appointmentType, reason);
        followUpService.createFollowUp(followUpDTO);
    }

    /**
     * Constructs the details for a follow-up based on the assessment outcomes.
     * <p>
     * This method prepares a {@link FollowUpDTO} object with details derived from the assessment,
     * the type of follow-up appointment needed, and the reason for the follow-up. It sets various
     * properties on the {@link FollowUpDTO} such as patient ID, encounter type, and reason for the follow-up.
     * Additionally, it determines the patient status based on whether the appointment type is a referral or not,
     * and sets other relevant information like household ID, next visit date, and referred site ID if available.
     * </p>
     *
     * @param assessmentDTO   The {@link AssessmentDTO} containing information from the completed assessment.
     * @param appointmentType The type of follow-up appointment required, as an {@link AppointmentType} enum.
     * @param reason          The reason for the follow-up, which could include referral reasons or visit reasons.
     * @return A fully constructed {@link FollowUpDTO} object ready for use in creating a follow-up action.
     */
    private FollowUpDTO constructAssessmentFollowUpDetails(AssessmentDTO assessmentDTO,
                                                           AppointmentType appointmentType, String reason) {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setPatientId(assessmentDTO.getEncounter().getPatientId());
        followUpDTO.setEncounterType(assessmentDTO.getReferralTicketType());
        followUpDTO.setReason(reason);
        followUpDTO.setPatientStatus(AppointmentType.REFERRED.equals(appointmentType) ? Constants.REFERRED :
                Constants.ON_TREATMENT);
        followUpDTO.setHouseholdId(assessmentDTO.getEncounter().getHouseholdId());
        if (Objects.nonNull(assessmentDTO.getSummary())) {
            followUpDTO.setNextVisitDate(assessmentDTO.getSummary().getNextVisitDate());
            followUpDTO.setReferredSiteId(assessmentDTO.getSummary().getReferredSiteId());
        }
        followUpDTO.setEncounterName(assessmentDTO.getAssessmentType());
        followUpDTO.setEncounterDate(assessmentDTO.getEncounter().getProvenance().getModifiedDate());
        followUpDTO.setMemberId(assessmentDTO.getEncounter().getMemberId());
        followUpDTO.setType(appointmentType);
        followUpDTO.setEncounterId(assessmentDTO.getId());
        followUpDTO.setVillageId(assessmentDTO.getVillageId());
        followUpDTO.setFirstInteractionMode(InteractionMode.ASSESSMENT);
        followUpDTO.setProvenance(assessmentDTO.getEncounter().getProvenance());
        return followUpDTO;
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
        return fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request);
    }

    /**
     * Create Auto Referral Ticket for continuous on treatment
     *
     * @param assessmentDTO - assessment object
     */
    private void createAutoReferralTicket(AssessmentDTO assessmentDTO) {
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                assessmentDTO.getEncounter().getMemberId(), List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.HH_VISIT), Boolean.FALSE);
        String reason = assessmentDTO.getReferredReasons();
        if (Objects.nonNull(reason)) {
            List<String> reasons = Arrays.stream(reason.split(Constants.COMMA))
                    .map(String::trim).sorted().toList();
            callRegisters = callRegisters.stream().filter(callRegister -> Objects.nonNull(callRegister.getReason()))
                    .filter(callRegister -> reasons.equals(Arrays.stream(callRegister.getReason().split(Constants.COMMA))
                            .map(String::trim).sorted().toList())).toList();
        }
        if (callRegisters.size() >= successfulCallAttempts) {
            closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()), List.of(AppointmentType.HH_VISIT), null, reason);
            assessmentDTO.setPatientStatus(Constants.REFERRED);
            assessmentDTO.getEncounter().setReferred(Boolean.TRUE);
            assessmentDTO.getSummary().setReferredSiteId(assessmentDTO.getEncounter().getProvenance().getOrganizationId());
            assessmentDTO.getSummary().setNextVisitDate(null);
        }
    }

    /**
     * Check ANC/PNC visit for followup close
     *
     * @param assessmentDTO - assessment object
     */
    private void checkVisitAndClosePregnancy(AssessmentDTO assessmentDTO) {
        String assessmentType = assessmentDTO.getAssessmentType();
        PregnancyInfo pregnancyInfo = getPatientVitals(assessmentDTO.getEncounter().getMemberId());
        boolean isFirstVisit = assessmentDTO.getEncounter().getVisitNumber() == Constants.ONE;
        boolean isAncType = Constants.ANC.equals(assessmentType);
        boolean isPncMotherType = Constants.PNC_MOTHER.equals(assessmentType);
        if (isFirstVisit && ((isAncType && (Objects.nonNull(pregnancyInfo.getPncVisitNo()) || Objects.nonNull(pregnancyInfo.getPncMotherMedicalReviewVisitNo())))
                || (isPncMotherType && (Objects.nonNull(pregnancyInfo.getAncVisitNo()) || Objects.nonNull(pregnancyInfo.getAncMedicalReviewVisitNo()))))) {
            closeFollowUps(assessmentDTO, List.of(assessmentDTO.getReferralTicketType()),
                    List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null, null);
        }
    }
    /**
     * To add red risk notification.
     *
     * @param patientId                - patient id
     * @param encounterId              - encounter id
     * @param memberId                 - member id
     */
    public void addRedRiskNotification(String patientId, String encounterId, String memberId) {
        Logger.logInfo("In AssessmentServiceImpl, adding red risk notification information");
        RedRiskNotification notification = createRedRiskNotification(patientId, encounterId, memberId);
        if (enableRedRiskNotification) {
            sendRedRiskSms(patientId, notification);
        }
    }

    /**
     * Check and close RMNCH details
     *
     * @param assessmentDTO Assessment Details
     */
    public void checkAndCloseRmnchDetails(AssessmentDTO assessmentDTO) {
        HouseholdMemberDTO householdMemberDTO = getHouseholdMemberById(assessmentDTO.getEncounter().getMemberId());
        PregnancyInfo pregnancyInfo = getPatientVitals(assessmentDTO.getEncounter().getMemberId());
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.CHILD_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            closeFollowUps(assessmentDTO, List.of(Constants.CHILDHOOD_VISIT), List.of(AppointmentType.HH_VISIT,
                    AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null, null);
        }
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            closeFollowUps(assessmentDTO, List.of(Constants.RMNCH), List.of(AppointmentType.HH_VISIT,
                    AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), Constants.PNC_NEONATE, null);
        }
        Date pncDeliveryDate = Objects.nonNull(pregnancyInfo.getDateOfDelivery()) ? pregnancyInfo.getDateOfDelivery() :
                pregnancyInfo.getPncCreatedDate();
        if (Objects.nonNull(pncDeliveryDate) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(pncDeliveryDate)) {
            closeFollowUps(assessmentDTO, List.of(Constants.RMNCH), List.of(AppointmentType.HH_VISIT,
                    AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null, null);
        }
    }

    /**
     * Get Household Member by Id
     *
     * @param memberId - member Id
     * @return Household Member DTO
     */
    private HouseholdMemberDTO getHouseholdMemberById(String memberId) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(memberId);
        return fhirServiceApiInterface.getHouseholdMemberById(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     * Creates a red risk notification for a patient.
     *
     * @param patientId                - patient id
     * @param encounterId              - encounter id
     * @param memberId                 - member id
     * @return RedRiskNotification - entity
     */
    private RedRiskNotification createRedRiskNotification(String patientId, String encounterId, String memberId) {
        Logger.logInfo("In AssessmentServiceImpl, creating red risk notification information");
        RedRiskNotification redRiskNotification = new RedRiskNotification();
        redRiskNotification.setMemberId(memberId);
        redRiskNotification.setEncounterId(encounterId);
        redRiskNotification.setPatientId(patientId);
        redRiskNotification.setStatus(Constants.NEW);
        return redRiskNotificationRepository.save(redRiskNotification);
    }

    /**
     * To update red risk Notification table
     *
     * @param patientReference  - Patient base reference
     * @param status - Risk status to be update
     */
    private void updateRedRiskNotification(String patientReference, String status) {
        List<RedRiskNotification> redRiskNotification = redRiskNotificationRepository
                .findByPatientIdAndStatus(patientReference, Constants.NEW);
        if (!Objects.isNull(redRiskNotification)) {
            redRiskNotification.stream().forEach(redRisk -> redRisk.setStatus(status));
            redRiskNotificationRepository.saveAll(redRiskNotification);
        }
    }

    /**
     * To send red risk notification.
     *
     * @param patientId                - patient id
     * @param notification Notification that having the id need to saved in appropriate outbound sms
     */
    private void sendRedRiskSms(String patientId, RedRiskNotification notification) {
        List<UserResponseDTO> users = userApiInterface.getUsersByRoleName(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), Constants.ROLE_RED_RISK_USER).getBody();
        List<SmsDTO> smsDtos = new ArrayList<>();
        SMSTemplate smsTemplate = notificationApiInterface
                .getSmsTemplateValues(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), Constants.TEMPLATE_TYPE_RED_RISK)
                .getBody();
        if (Objects.nonNull(users) && !users.isEmpty()) {
            users.stream().forEach(user -> {
                if (Objects.equals(user.getTenantId(), UserSelectedTenantContextHolder.get())) {
                    smsDtos.add(new SmsDTO(smsTemplate.getBody(), user.getCountryCode() + user.getPhoneNumber(), user.getUsername(),
                            notification.getTenantId(), patientId, notification.getId()));
                }
            });
        }
        notificationApiInterface.saveOutBoundSms(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                smsDtos);
    }

    /**
     * {@inheritDoc}
     */
    public AssessmentDTO createNcdAssessment(AssessmentDTO assessmentDTO) {
        Long cultureId = UserContextHolder.getUserDto().getCultureId();
        ScreeningLogRequestDTO screeningLogRequestDTODTO = new ScreeningLogRequestDTO();
        screeningLogRequestDTODTO.setFhirId(assessmentDTO.getMemberReference());
        ScreeningLog screeningLog = fhirServiceApiInterface.getScreeningLog(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), screeningLogRequestDTODTO);
        if (null == cultureId) {
            cultureId = staticDataService.findCulture(Constants.DEFAULT_CULTURE_VALUE).getId();
        }
        boolean existingRiskLevel = Constants.HIGH.equals(screeningLog.getRiskLevel());
        String riskLevel = calculateRiskLevel(assessmentDTO, screeningLog);
        String riskMessage = setRiskMessage(riskLevel);
        boolean isRedRisk = Constants.HIGH.equals(riskLevel);
        Boolean pregnancyRedRisk = setPregnancySymptoms(assessmentDTO.getPregnancyAnc());
        calculateMentalhealthRiskLevel(assessmentDTO);
        if (Objects.nonNull(pregnancyRedRisk) && Boolean.TRUE.equals(pregnancyRedRisk)) {
            riskLevel = Constants.HIGH;
            assessmentDTO.setRiskLevel(riskLevel);
            assessmentDTO.getPregnancyAnc().setIsPregnancyRisk(Boolean.TRUE);
            riskMessage = setRiskMessage(riskLevel);
        } else if (Boolean.FALSE.equals(pregnancyRedRisk) && (assessmentDTO.getBioMetrics().getAge() < 18 ||
                assessmentDTO.getBioMetrics().getAge() > 35)) {
            assessmentDTO.getPregnancyAnc().setIsPregnancyRisk(Constants.BOOLEAN_TRUE);
        }
        
        if (Objects.nonNull(assessmentDTO.getPregnancyAnc()) && Objects.isNull(assessmentDTO.getPregnancyAnc().getIsPregnancyRisk())) {
            assessmentDTO.getPregnancyAnc().setIsPregnancyRisk(Constants.BOOLEAN_FALSE);
        }

        assessmentDTO.setRiskLevel(riskLevel);
        assessmentDTO.setRiskMessage(riskMessage);
        AssessmentDTO patientAssessmentDTO = fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), assessmentDTO);

        String patientId = Objects.isNull(patientAssessmentDTO.getPatientReference()) ?
                patientAssessmentDTO.getPatientId() : patientAssessmentDTO.getPatientReference();
        if (isRedRisk) {
            updateRedRiskNotification(patientId, Constants.ASSESSMENT_COMPLETED);
            addRedRiskNotification(patientId, patientAssessmentDTO.getEncounter().getId(),
                    patientAssessmentDTO.getMemberReference());
        } else if (existingRiskLevel) {
            updateRedRiskNotification(patientId, Constants.ASSESSMENT_COMPLETED);
        }

        if (!Objects.isNull(assessmentDTO.getCustomizedWorkflows())
                && !assessmentDTO.getCustomizedWorkflows().isEmpty()) {
            customizedModulesService.createCustomizedModules(assessmentDTO.getCustomizedWorkflows(),
                    Constants.WORKFLOW_ASSESSMENT, patientAssessmentDTO.getMemberReference(), patientId);
        }

        if (!Objects.nonNull(assessmentDTO.getPatientReference())) {
            customizedModulesService.updateCustomizedModules(assessmentDTO.getMemberReference(), patientAssessmentDTO.getPatientId());
        }

        if (Objects.nonNull(patientAssessmentDTO.getNextBgAssessmentDate())
                || Objects.nonNull(patientAssessmentDTO.getNextBpAssessmentDate())) {
            createCallRegister(patientAssessmentDTO, AppointmentType.ASSESSMENT);
        }

        if (Objects.nonNull(patientAssessmentDTO.getNextMedicalReviewDate())) {
            createCallRegister(patientAssessmentDTO, AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW);
        }
        return patientAssessmentDTO;
    }

    /**
     * <p>
     * Calculates and updates the mental health risk levels for the provided assessment.
     * </p>
     *
     * This method processes the PHQ-4, PHQ-9, and GAD-7 scores from the assessmentDTO,
     * and calculates their respective risk levels using the riskLevelAlgorithm.
     *
     * @param assessmentDTO The data transfer object containing assessment details, including PHQ-4, PHQ-9, and GAD-7 scores.
     */
    private void calculateMentalhealthRiskLevel(AssessmentDTO assessmentDTO) {
        if (Objects.nonNull(assessmentDTO.getPhq4())) {
            MentalHealthDTO phq4 = assessmentDTO.getPhq4();
            phq4.setRiskLevel(riskLevelAlgorithm.calculatePhq4RiskLevel(assessmentDTO.getPhq4().getScore()));
            assessmentDTO.setPhq4(phq4);
        }
        if (Objects.nonNull(assessmentDTO.getPhq9())) {
            MentalHealthDTO phq9 = assessmentDTO.getPhq9();
            phq9.setRiskLevel(riskLevelAlgorithm.calculatePhq9RiskLevel(assessmentDTO.getPhq9().getScore()));
            assessmentDTO.setPhq9(phq9);
        }
        if (Objects.nonNull(assessmentDTO.getGad7())) {
            MentalHealthDTO gad7 = assessmentDTO.getGad7();
            gad7.setRiskLevel(riskLevelAlgorithm.calculateGad7RiskLevel(assessmentDTO.getGad7().getScore()));
            assessmentDTO.setGad7(gad7);
        }
    }

    /**
     * This method is used to set the pregnancy risk.
     *
     * @param pregnancyAncDTO  - set of pregnancyAncDTO
     */
    private Boolean setPregnancySymptoms(PregnancyDetailsDTO pregnancyAncDTO) {
        Boolean redRisk = null;
        if (!Objects.isNull(pregnancyAncDTO) && !Objects.isNull(pregnancyAncDTO.getPregnancySymptoms())
                && !pregnancyAncDTO.getPregnancySymptoms().isEmpty()) {
            redRisk = Boolean.FALSE;
            for (PregnancySymptomDTO symptom : pregnancyAncDTO.getPregnancySymptoms()) {
                redRisk = !symptom.getName().equals(Constants.NO_SYMPTOMS);
            }
        }
        return redRisk;
    }

    /**
     * <p>
     * Sets risk message bases on the risk level.
     * </p>
     *
     * @param riskLevel {@link String} risk level is given
     * @return String {@link String} risk message is returned
     */
    private String setRiskMessage(String riskLevel) {
        Logger.logInfo("In AssessmentServiceImpl, set risk message");
        List<MetaDataDTO> messageMetaData = staticDataService.getMessageMetaData();
        MetaDataDTO redRiskMessage = messageMetaData.stream().filter(message -> message.getCategory().equals(riskLevel))
                .findFirst().orElse(messageMetaData.stream().filter(message -> message.getCategory()
                                .equals(Constants.MODERATE)).findFirst()
                        .orElseThrow(() -> new DataNotFoundException(20007)));
        return redRiskMessage.getDisplayValue();
    }

    /**
     * <p>
     * This function calculates the risk level of a patient based on their medical data and symptoms.
     * </p>
     *
     * @param assessment {@link AssessmentDTO} Object with patient assessment data is given
     * @param screeningLog {@link ScreeningLog} Object with patient screening log data is given
     * @return {@link String} The method returns a String value which represents the calculated risk level based on
     * the given parameters is returned
     */
    public String calculateRiskLevel(AssessmentDTO assessment, ScreeningLog screeningLog) {
        Logger.logInfo("In AssessmentServiceImpl, calculate risk level");
        RiskAlgorithmDTO riskAlgorithmDto = new RiskAlgorithmDTO();
        if (isGlucoseLogGiven(assessment.getGlucoseLog())) {
            riskAlgorithmDto.setGlucoseType(assessment.getGlucoseLog().getGlucoseType());
            riskAlgorithmDto.setGlucoseValue(assessment.getGlucoseLog().getGlucoseValue());
        }
        if (!Objects.isNull(assessment.getBpLog())) {
            riskAlgorithmDto.setAvgDiastolic(assessment.getBpLog().getAvgDiastolic());
            riskAlgorithmDto.setAvgSystolic(assessment.getBpLog().getAvgSystolic());
        }
        riskAlgorithmDto.setIsPregnant(screeningLog.getIsPregnant());
        riskAlgorithmDto.setRiskLevel(screeningLog.getRiskLevel());
        if (!Objects.isNull(assessment.getNcdSymptoms())) {
            riskAlgorithmDto.setNcdSymptoms(
                    assessment.getNcdSymptoms().stream().map(SymptomDTO::getId).collect(Collectors.toSet()));
        }
        return riskAlgorithm.getRiskLevelInAssessmentDbm(riskAlgorithmDto);
    }

    /**
     * <p>
     * The function checks if a glucose log object is not null and has either a glucose value or an
     * HbA1c value.
     * </p>
     *
     * @param glucoseLog It is an object of the class GlucoseLog, which contains information about a
     *                   glucose log entry, such as the glucose value and HbA1c level.
     * @return The method `isGlucoseLogGiven` is returning a boolean value
     */
    public static boolean isGlucoseLogGiven(GlucoseLogDTO glucoseLog) {
        return !Objects.isNull(glucoseLog)
                && (!Objects.isNull(glucoseLog.getGlucoseValue())
                || !Objects.isNull(glucoseLog.getHba1c()));
    }

    /**
     * {@inheritDoc}
     */
    public void createBpAssessment(AssessmentDTO assessment) {
        Logger.logInfo("In AssessmentServiceImpl, creating assessment bp log information");
        Long cultureId = UserContextHolder.getUserDto().getCultureId();
        if (null == cultureId) {
            cultureId = staticDataService.findCulture(Constants.DEFAULT_CULTURE_VALUE).getId();
        }
        boolean isRedRisk = false;
        ScreeningLogRequestDTO screeningLogRequestDTODTO = new ScreeningLogRequestDTO();
        screeningLogRequestDTODTO.setFhirId(assessment.getMemberReference());
        ScreeningLog screeningLog = fhirServiceApiInterface.getScreeningLog(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), screeningLogRequestDTODTO);
        boolean existingRiskLevel = Constants.HIGH.equals(screeningLog.getRiskLevel());

        BpLogDTO existingBpLog = fhirServiceApiInterface.getExistingBpLog(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), assessment);
        if (!Objects.isNull(existingBpLog) && !Objects.isNull(existingBpLog.getBpTakenOn())
                && !Objects.isNull(assessment.getAssessmentTakenOn())
                && (Constants.ZERO > assessment.getAssessmentTakenOn().compareTo(existingBpLog.getBpTakenOn()))) {
            assessment.setOldRecord(true);
        } else {
            assessment.setOldRecord(false);
            String riskLevel = calculateRiskLevel(assessment, screeningLog);
            isRedRisk = Constants.HIGH.equals(riskLevel);
            String riskMessage = setRiskMessage(riskLevel);
            assessment.setRiskLevel(riskLevel);
            assessment.setRiskMessage(riskMessage);
        }
        assessment.setAssessmentType(Constants.NON_COMMUNITY);
        AssessmentDTO patientAssessmentDTO = fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), assessment);
        String patientId = Objects.isNull(patientAssessmentDTO.getPatientReference()) ?
                patientAssessmentDTO.getPatientId() : patientAssessmentDTO.getPatientReference();

        if (isRedRisk) {
            updateRedRiskNotification(patientId, Constants.ASSESSMENT_COMPLETED);
            addRedRiskNotification(patientId, patientAssessmentDTO.getEncounter().getId(),
                    patientAssessmentDTO.getMemberReference());
        } else if (existingRiskLevel) {
            updateRedRiskNotification(patientId, Constants.ASSESSMENT_COMPLETED);
        }

        if (Objects.nonNull(patientAssessmentDTO.getNextBpAssessmentDate())) {
            createCallRegister(patientAssessmentDTO, AppointmentType.ASSESSMENT);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void createGlucoseLog(AssessmentDTO assessment) {
        Logger.logInfo("In AssessmentServiceImpl, creating assessment bg log information");
        Long cultureId = UserContextHolder.getUserDto().getCultureId();
        if (null == cultureId) {
            cultureId = staticDataService.findCulture(Constants.DEFAULT_CULTURE_VALUE).getId();
        }
        boolean isRedRisk = false;
        ScreeningLogRequestDTO screeningLogRequestDTODTO = new ScreeningLogRequestDTO();
        screeningLogRequestDTODTO.setFhirId(assessment.getMemberReference());
        ScreeningLog screeningLog = fhirServiceApiInterface.getScreeningLog(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), screeningLogRequestDTODTO);
        boolean existingRiskLevel = Constants.HIGH.equals(screeningLog.getRiskLevel());

        GlucoseLogDTO existingGlucoseLog = fhirServiceApiInterface.getExistingGlucoseLog(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), assessment);
        if (!Objects.isNull(existingGlucoseLog) && !Objects.isNull(existingGlucoseLog.getBgTakenOn())
                && !Objects.isNull(assessment.getAssessmentTakenOn())
                && (Constants.ZERO > assessment.getAssessmentTakenOn().compareTo(existingGlucoseLog.getBgTakenOn()))) {
            assessment.setOldRecord(true);
        } else {
            assessment.setOldRecord(false);
            String riskLevel = calculateRiskLevel(assessment, screeningLog);
            isRedRisk = Constants.HIGH.equals(riskLevel);
            String riskMessage = setRiskMessage(riskLevel);
            assessment.setRiskLevel(riskLevel);
            assessment.setRiskMessage(riskMessage);
        }
        assessment.setAssessmentType(Constants.NON_COMMUNITY);
        AssessmentDTO patientAssessmentDTO = fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), assessment);
        String patientId = Objects.isNull(patientAssessmentDTO.getPatientReference()) ?
                patientAssessmentDTO.getPatientId() : patientAssessmentDTO.getPatientReference();
        if (isRedRisk) {
            updateRedRiskNotification(patientId, Constants.ASSESSMENT_COMPLETED);
            addRedRiskNotification(patientId, patientAssessmentDTO.getEncounter().getId(),
                    patientAssessmentDTO.getMemberReference());
        } else if (existingRiskLevel) {
            updateRedRiskNotification(patientId, Constants.ASSESSMENT_COMPLETED);
        }

        if (Objects.nonNull(patientAssessmentDTO.getNextBgAssessmentDate())) {
            createCallRegister(patientAssessmentDTO, AppointmentType.ASSESSMENT);
        }
    }

    /**
     * <p>
     * This function Creates and updates patients registers for the patient
     * </p>
     *
     * @param assessmentDTO {@link AssessmentDTO} request that contains the patient related details
     */
    private void createCallRegister(AssessmentDTO assessmentDTO, AppointmentType type) {
        CallRegister callRegister = new CallRegister();
        callRegister.setMemberId(assessmentDTO.getMemberReference());
        callRegister.setPatientId(Objects.nonNull(assessmentDTO.getPatientReference())
                ? assessmentDTO.getPatientReference() : assessmentDTO.getPatientId());
        callRegister.setIsInitiated(Boolean.FALSE);
        callRegister.setType(type);
        callRegister.setIsWrongNumber(Boolean.FALSE);
        callRegister.setVillageId(assessmentDTO.getVillageId());
        if (AppointmentType.ASSESSMENT.equals(type)) {
            if (Objects.nonNull(assessmentDTO.getNextBpAssessmentDate())) {
                callRegister.setNextBPAssessmentDate(assessmentDTO.getNextBpAssessmentDate());
            }
            if (Objects.nonNull(assessmentDTO.getNextBgAssessmentDate())) {
                callRegister.setNextBGAssessmentTime(assessmentDTO.getNextBgAssessmentDate());
            }
        } else {
            callRegister.setNextMedicalReviewDate(assessmentDTO.getNextMedicalReviewDate());
        }
        callRegister.setReferredSiteId(assessmentDTO.getAssessmentOrganizationId());
        followUpService.addCallRegister(callRegister, Boolean.TRUE);
    }
}