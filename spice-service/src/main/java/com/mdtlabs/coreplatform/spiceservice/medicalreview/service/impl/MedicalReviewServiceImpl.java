package com.mdtlabs.coreplatform.spiceservice.medicalreview.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Date;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.UnderFiveIccmDTO;

import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PncMedicalReviewDTO;

import com.mdtlabs.coreplatform.spiceservice.medicalreview.service.MedicalReviewService;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;

/**
 * <p>
 * This class is a service class to perform operation on MedicalReviewService
 * operations.
 * </p>
 *
 * @author Nandhakumar created on Mar 28, 2024
 */
@Service
public class MedicalReviewServiceImpl implements MedicalReviewService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final FollowUpService followUpService;

    private final CallRegisterRepository callRegisterRepository;

    private final CustomizedModulesService customizedModulesService;

    private final StaticDataService staticDataService;

    public MedicalReviewServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, FollowUpService followUpService, CallRegisterRepository callRegisterRepository, CustomizedModulesService customizedModulesService, StaticDataService staticDataService) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.followUpService = followUpService;
        this.callRegisterRepository = callRegisterRepository;
        this.customizedModulesService = customizedModulesService;
        this.staticDataService = staticDataService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createGeneralMedicalReview(GeneralMedicalReviewDTO generalMedicalReviewDTO) {
        checkAndCloseRmnchDetails(generalMedicalReviewDTO.getEncounter().getMemberId());
        return fhirServiceApiInterface.createIccmGeneralMedialReview(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                generalMedicalReviewDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GeneralMedicalReviewSummaryDetailsDTO getGeneralMedicalReviewDetails(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getIccmGeneralMedialReview(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GeneralMedicalReviewSummaryDTO saveSummaryDetails(GeneralMedicalReviewSummaryDTO summaryDTO) {
        if (Constants.RECOVERED.equals(summaryDTO.getPatientStatus()) && Constants.ICCM.equals(summaryDTO.getCategory())) {
            closeFollowUps(summaryDTO.getMemberId(),
                    List.of(Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.HH_VISIT), null);
        }
        GeneralMedicalReviewSummaryDTO medicalReviewSummary = fhirServiceApiInterface.createMedicalReviewSummary(
                CommonUtil.getAuthToken(),
                CommonUtil.getClient(),
                summaryDTO);
        createMedicalReviewFollowUp(summaryDTO);
        return medicalReviewSummary;
    }


    /**
     * {@inheritDoc}
     */
    public Map<String, String> createMedicalReview(MedicalReviewPregnancyDTO medicalReviewDTO) {
        if (medicalReviewDTO.getEncounter().getVisitNumber() == Constants.ONE) {
            PregnancyInfo pregnancyInfo = getPatientVitals(medicalReviewDTO.getEncounter().getMemberId());
            if (Objects.nonNull(pregnancyInfo.getPncVisitNo())
                    || Objects.nonNull(pregnancyInfo.getPncMotherMedicalReviewVisitNo())) {
                closeFollowUps(medicalReviewDTO.getEncounter().getMemberId(), List.of(Constants.RMNCH),
                        List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
            }
        }
        return fhirServiceApiInterface.createMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                medicalReviewDTO);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, String> createLabourMotherAndNeonate(MotherNeonateDTO requestDTO) {
        closeFollowUps(requestDTO.getMotherDTO().getEncounter().getMemberId(), List.of(Constants.RMNCH),
                List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
        return fhirServiceApiInterface.createLabourMotherAndNeonateMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createICCMUnder2months(UnderFiveIccmDTO iccmdto) {
        checkAndCloseRmnchDetails(iccmdto.getEncounter().getMemberId());
        return fhirServiceApiInterface.createICCMUnder2months(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                iccmdto);
    }

    /**
     * {@inheritDoc}
     */
    public ObservationDTO createWeight(ObservationDTO observationDto) {
        return fhirServiceApiInterface.createWeight(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                observationDto);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MedicalReviewHistoryDTO getHistory(MedicalReviewRequestDTO requestDTO) {
        return fhirServiceApiInterface.getMedicalReviewHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MedicalReviewHistoryDTO getPncHistory(MedicalReviewRequestDTO requestDTO) {
        return fhirServiceApiInterface.getPncMedicalReviewHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * Get Birth-History Details
     *
     * @param requestDTO the requestDTO
     * @return BirthHistoryDTO Details
     */
    @Override
    public BirthHistoryDTO getBirthHistory(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getBirthHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    public ObservationDTO createBp(ObservationDTO observationDto) {
        return fhirServiceApiInterface.createBp(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                observationDto);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IccmResponseDTO getIccmUnder2MSummary(MedicalReviewRequestDTO requestDTO) {
        return fhirServiceApiInterface.getIccmUnder2MSummary(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createICCMUnder5Years(UnderFiveIccmDTO iccmdto) {
        checkAndCloseRmnchDetails(iccmdto.getEncounter().getMemberId());
        return fhirServiceApiInterface.createICCMUnder5Years(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                iccmdto);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> savePncMedicalReview(PncMedicalReviewDTO pncMedicalReviewDTO) {
        if (Boolean.FALSE.equals(pncMedicalReviewDTO.getPncMother().getIsMotherAlive())) {
            closeFollowUps(pncMedicalReviewDTO.getPncMother().getEncounter().getMemberId(), List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
        } else if (pncMedicalReviewDTO.getPncMother().getEncounter().getVisitNumber() == Constants.ONE) {
            PregnancyInfo pregnancyInfo = getPatientVitals(pncMedicalReviewDTO.getPncMother().getEncounter().getMemberId());
            if (Objects.nonNull(pregnancyInfo.getAncVisitNo())
                    || Objects.nonNull(pregnancyInfo.getAncMedicalReviewVisitNo())) {
                closeFollowUps(pncMedicalReviewDTO.getPncMother().getEncounter().getMemberId(), List.of(Constants.RMNCH),
                        List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
            }
        }
        if (Objects.nonNull(pncMedicalReviewDTO.getChild())) {
            String householdId = pncMedicalReviewDTO.getPncMother().getEncounter().getHouseholdId();
            HouseholdMemberDTO householdMemberDTO = pncMedicalReviewDTO.getChild();
            if (Objects.nonNull(householdId)) {
                householdMemberDTO.setHouseholdId(householdId);
            }
            householdMemberDTO = fhirServiceApiInterface.createChild(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdMemberDTO);
            if (Objects.nonNull(pncMedicalReviewDTO.getPncChild())) {
                pncMedicalReviewDTO.getPncChild().getEncounter().setPatientId(householdMemberDTO.getPatientId());
                pncMedicalReviewDTO.getPncChild().getEncounter().setMemberId(householdMemberDTO.getId());
            }
            pncMedicalReviewDTO.getPncMother().getLabourDTO().setNeonatePatientId(householdMemberDTO.getPatientId());
        }
        if (Objects.nonNull(pncMedicalReviewDTO.getPncChild()) && Boolean.FALSE.equals(pncMedicalReviewDTO.getPncChild().getIsChildAlive())) {
            HouseholdMemberDTO householdMember = getHouseholdMemberByPatientId(pncMedicalReviewDTO.getPncChild().getEncounter().getPatientId());
            closeFollowUps(householdMember.getId(), List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.HH_VISIT, AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
        }
        return fhirServiceApiInterface.createPncMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), pncMedicalReviewDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IccmResponseDTO getIccmUnder5YSummary(MedicalReviewRequestDTO requestDTO) {
        return fhirServiceApiInterface.getIccmUnder5YSummary(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MedicalReviewPregnancySummaryDetailsDTO getPregnancyMedicalReviewDetails(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getPregnancyMedicalReviewDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PncMedicalReviewDTO getPNCMedicalReviewDetails(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getPncDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Double> getPatientWeight(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getWeight(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Double> getPatientBp(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getBp(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * Create Medical Review followup
     *
     * @param summaryDTO - summary object
     */
    private void createMedicalReviewFollowUp(GeneralMedicalReviewSummaryDTO summaryDTO) {
        boolean hasNextVisitDate = Objects.nonNull(summaryDTO.getNextVisitDate());
        closeFollowUps(summaryDTO.getMemberId(), List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                List.of(AppointmentType.REFERRED), null);
        if (Constants.ICCM.equals(summaryDTO.getCategory())) {
            if (hasNextVisitDate) {
                summaryDTO.setType(AppointmentType.MEDICAL_REVIEW);
                createFollowUp(summaryDTO);
            } else {
                closeFollowUps(summaryDTO.getMemberId(), List.of(Constants.ICCM, Constants.CHILDHOOD_VISIT),
                        List.of(AppointmentType.MEDICAL_REVIEW), null);
            }
        } else if (Constants.RMNCH.equals(summaryDTO.getCategory())) {
            closeFollowUps(summaryDTO.getMemberId(),
                    List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.MEDICAL_REVIEW), null);
            if (hasNextVisitDate) {
                boolean labourDelivery = Constants.PREGNANCY_ANC_MOTHER_MEDICAL_REVIEW.equals(
                        summaryDTO.getEncounterType());
                summaryDTO.setType(AppointmentType.MEDICAL_REVIEW);
                summaryDTO.setReason(
                        labourDelivery ? Constants.PNC_VISIT :
                                null);
                createFollowUp(summaryDTO);
            }
        }
    }

    /**
     * Create followup
     *
     * @param summaryDTO - summary object
     */
    private void createFollowUp(GeneralMedicalReviewSummaryDTO summaryDTO) {
        FollowUpDTO followUpDTO = constructMedicalReviewFollowUpDetails(summaryDTO, summaryDTO.getType(), summaryDTO.getReason());
        followUpService.createFollowUp(followUpDTO);
    }

    /**
     * Close followups
     *
     * @param memberId            - member Id
     * @param referralTicketTypes - referral ticket types
     * @param appointmentTypes    - appointment types
     * @param encounterName       - encounter name
     */
    private void closeFollowUps(String memberId, List<String> referralTicketTypes, List<AppointmentType> appointmentTypes, String encounterName) {
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                memberId, referralTicketTypes, appointmentTypes, Boolean.FALSE);
        if (Objects.nonNull(encounterName)) {
            callRegisters = callRegisters.stream().filter(callRegister -> encounterName.equals(callRegister.getEncounterName())).toList();
        }
        callRegisters.forEach(callRegister -> {
            callRegister.setIsCompleted(Boolean.TRUE);
            callRegister.setLastInteractionMode(InteractionMode.MEDICAL_REVIEW);
            callRegisterRepository.save(callRegister);
        });
    }

    /**
     * Construct Medical review followup details
     *
     * @param summaryDTO - summary object
     * @return FollowUpDTO
     */
    private FollowUpDTO constructMedicalReviewFollowUpDetails(GeneralMedicalReviewSummaryDTO summaryDTO, AppointmentType type, String reason) {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setPatientId(summaryDTO.getPatientId());
        followUpDTO.setEncounterType(summaryDTO.getCategory());
        followUpDTO.setEncounterName(summaryDTO.getEncounterType());
        followUpDTO.setPatientStatus(Constants.ON_TREATMENT);
        followUpDTO.setHouseholdId(summaryDTO.getHouseholdId());
        followUpDTO.setNextVisitDate(summaryDTO.getNextVisitDate());
        followUpDTO.setEncounterDate(summaryDTO.getProvenance().getModifiedDate());
        followUpDTO.setMemberId(summaryDTO.getMemberId());
        followUpDTO.setType(type);
        followUpDTO.setReason(reason);
        followUpDTO.setEncounterId(summaryDTO.getId());
        followUpDTO.setVillageId(summaryDTO.getVillageId());
        followUpDTO.setFirstInteractionMode(InteractionMode.MEDICAL_REVIEW);
        followUpDTO.setProvenance(summaryDTO.getProvenance());
        return followUpDTO;
    }

    /**
     * {@inheritDoc}
     */
    public MotherNeonateDTO getLabourMotherAndNeonateDetails(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getLabourMotherAndNeonateDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    /**
     * Check and close RMNCH details
     *
     * @param memberId - member Id
     */
    public void checkAndCloseRmnchDetails(String memberId) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(memberId);
        HouseholdMemberDTO householdMemberDTO = fhirServiceApiInterface.getHouseholdMemberById(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
        PregnancyInfo pregnancyInfo = getPatientVitals(memberId);
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.CHILD_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            closeFollowUps(memberId, List.of(Constants.CHILDHOOD_VISIT), List.of(AppointmentType.HH_VISIT,
                    AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
        }
        if (Objects.nonNull(householdMemberDTO.getDateOfBirth()) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(householdMemberDTO.getDateOfBirth())) {
            closeFollowUps(memberId, List.of(Constants.RMNCH), List.of(AppointmentType.HH_VISIT,
                    AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), Constants.PNC_NEONATE);
        }
        Date pncDeliveryDate = Objects.nonNull(pregnancyInfo.getDateOfDelivery()) ? pregnancyInfo.getDateOfDelivery() :
                pregnancyInfo.getPncCreatedDate();
        if (Objects.nonNull(pncDeliveryDate) &&
                Constants.PNC_VISIT_CLOSE_DAYS < DateUtil.daysSincePast(pncDeliveryDate)) {
            closeFollowUps(memberId, List.of(Constants.RMNCH), List.of(AppointmentType.HH_VISIT,
                    AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), null);
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
        return fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request);
    }

    /**
     * Get Household Member by patient Id
     *
     * @param patientId - patient Idf
     * @return Household Member DTO
     */
    private HouseholdMemberDTO getHouseholdMemberByPatientId(String patientId) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientId(patientId);
        return fhirServiceApiInterface.getHouseholdMemberByPatientId(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createPncChild(HouseholdMemberDTO householdMemberDTO) {
        fhirServiceApiInterface.createPncChild(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdMemberDTO);
    }

    /**
     * {@inheritDoc}
     */
    public PatientStatusDTO createPatientStatus(PatientStatusDTO patientStatusDto) {
        PatientStatusDTO response = fhirServiceApiInterface.createPatientStatus(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                patientStatusDto);
        if (Objects.nonNull(response.getPatientReference())) {
            customizedModulesService.updateCustomizedModules(patientStatusDto.getMemberReference(), patientStatusDto.getPatientReference());
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public PatientStatusDTO getPatientStatusDetails(PatientStatusDTO patientStatusDto) {
        return fhirServiceApiInterface.getPatientStatusDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                patientStatusDto);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, String> createNcdMedicalReview(NCDMedicalReviewDTO request) {
        return fhirServiceApiInterface.createNcdMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);

    }

    /**
     * {@inheritDoc}
     */
    public void createSummary(NCDMedicalReviewDTO request) {
        fhirServiceApiInterface.createSummary(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
        if (Objects.nonNull(request.getNextMedicalReviewDate()) && Objects.nonNull(request.getPatientReference())) {
            createMedicalReviewCallRegister(request);
        }
    }

    /**
     * {@inheritDoc}
     */
    public NcdMedicalReviewResponse ncdMedicalReviewSummary(MedicalReviewRequestDTO request) {
        return fhirServiceApiInterface.getNcdMedicalReviewDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    public void updateConfirmDiagnosis(ConfirmDiagnosisDTO confirmDiagnosisDTO) {
        fhirServiceApiInterface.updateConfirmDiagnosis(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                confirmDiagnosisDTO);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Integer> getMedicalReviewCount(RequestDTO request) {
        return fhirServiceApiInterface.getMedicalReviewCount(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                request);
    }

    /**
     * {@inheritDoc}
     */
    public NCDMedicalReviewHistoryDTO getNCDMedicalReviewHistory(MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return fhirServiceApiInterface.getNCDMedicalReviewHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                medicalReviewRequestDTO);
    }

    /**
     * {@inheritDoc}
     */
    public NCDMedicalReviewHistoryDTO getNCDMedicalReviewSummaryHistory(MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return fhirServiceApiInterface.getNCDMedicalReviewSummaryHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                medicalReviewRequestDTO);
    }

    /**
     * {@inheritDoc}
     */
    public boolean updateViewCount(RequestDTO request) {
        return fhirServiceApiInterface.updateViewCount(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                request);
    }
    /**
     * {@inheritDoc}
     */
    public List<LifestyleResponseDTO> getPatientLifestyleDetails(RequestDTO request) {
        if (Objects.isNull(request.getPatientReference())) {
            throw new SpiceValidation();
        }
        List<LifestyleResponseDTO> lifestyleResponses = fhirServiceApiInterface.getPatientLifestyleDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);

        if (!lifestyleResponses.isEmpty()) {
            List<MetaDataDTO> lifestyles = staticDataService.getLifestyles();
            Map<String, MetaDataDTO> lifestylesMap = new HashMap<>();
            lifestyles.stream().forEach(lifestyle -> lifestylesMap.put(lifestyle.getValue().toString(), lifestyle));

            lifestyleResponses.stream().forEach(lifestyleResponse -> {
                lifestyleResponse.setLifestyleType(lifestylesMap.get(lifestyleResponse.getValue()).getType());
                lifestyleResponse.setLifestyle((lifestylesMap.get(lifestyleResponse.getValue()).getName()));
            });
        }
        return lifestyleResponses;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getInstruction() {
        return MessageValidator.getInstance().getMessage(Constants.INSTRUCTIONS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateNCDAppointment(NCDMedicalReviewDTO request) {
        fhirServiceApiInterface.updateNCDAppointment(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * <p>
     * This function Creates and updates patients registers for the patient
     * </p>
     *
     * @param requestDTO {@link NCDMedicalReviewDTO} request that contains the patient related details
     */
    private void createMedicalReviewCallRegister(NCDMedicalReviewDTO requestDTO) {
        CallRegister callRegister = new CallRegister();
        callRegister.setMemberId(requestDTO.getMemberReference());
        callRegister.setPatientId(requestDTO.getPatientReference());
        callRegister.setIsInitiated(Boolean.FALSE);
        callRegister.setType(AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW);
        callRegister.setIsWrongNumber(Boolean.FALSE);
        callRegister.setVillageId(requestDTO.getVillageId());
        callRegister.setNextMedicalReviewDate(requestDTO.getNextMedicalReviewDate());
        callRegister.setReferredSiteId(requestDTO.getProvenance().getOrganizationId());
        followUpService.addCallRegister(callRegister, Boolean.TRUE);
    }
}