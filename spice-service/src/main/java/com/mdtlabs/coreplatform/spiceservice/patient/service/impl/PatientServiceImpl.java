package com.mdtlabs.coreplatform.spiceservice.patient.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WgsData;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.WgsDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.patient.repository.WgsDataRepository;
import com.mdtlabs.coreplatform.spiceservice.patient.service.PatientService;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.repository.PatientTransferRepository;

import org.springframework.stereotype.Service;

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

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final AdminServiceApiInterface adminServiceApiInterface;

    private final FollowUpService followUpService;

    private final CallRegisterRepository callRegisterRepository;

    private final PatientTransferRepository patientTransferRepository;

    private final WgsDataRepository wgsDataRepository;

    public PatientServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, AdminServiceApiInterface adminServiceApiInterface, FollowUpService followUpService, CallRegisterRepository callRegisterRepository, WgsDataRepository wgsDataRepository,
                              PatientTransferRepository patientTransferRepository) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.followUpService = followUpService;
        this.callRegisterRepository = callRegisterRepository;
        this.wgsDataRepository = wgsDataRepository;
        this.patientTransferRepository = patientTransferRepository;
    }

    /**
     * search a patientRequestDTO form FHIR DB.
     *
     * @param patientRequestDTO - patientRequestDTO dto
     * @return success response of patientDTO Entity
     */
    @Override
    public Map<String, Object> searchPatient(PatientRequestDTO patientRequestDTO) {
        if (Objects.isNull(patientRequestDTO.getDistrictId())) {
            throw new SpiceValidation(1007);
        }
        List<VillageDTO> villages = adminServiceApiInterface.getVillageByDistrict(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                Map.of(Constants.DISTRICT_ID, patientRequestDTO.getDistrictId()));
        patientRequestDTO.setVillageIds(villages.stream().map(VillageDTO::getId).toList());
        return fhirServiceApiInterface.searchPatient(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO);
    }

    /**
     * Get Patient details
     *
     * @param patientDTO
     * @return PatientRequest Object
     */
    @Override
    public PatientDetailsDTO getPatientDetails(PatientDTO patientDTO) {
        return fhirServiceApiInterface.getPatientDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientDTO);
    }

    /**
     * Get a patientRequestDTO form FHIR DB.
     *
     * @param patientRequestDTO - patientRequestDTO dto
     * @return patientRequestDTO Entity
     */
    @Override
    @ConfigureAppType
    public Map<String, Object> getPatientList(PatientRequestDTO patientRequestDTO) {
        if (Constants.NON_COMMUNITY.equalsIgnoreCase(SelectedAppTypeContextHolder.get())){
            return fhirServiceApiInterface.getPatientNcdList(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO);
        }
        List<Long> villageIds = adminServiceApiInterface.getFacilityVillagesByTenantId(
                CommonUtil.getAuthToken(), CommonUtil.getClient(), UserSelectedTenantContextHolder.get());
        patientRequestDTO.setVillageIds(villageIds);
        return fhirServiceApiInterface.getPatientList(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getPatientStatus(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getPatientStatus(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean updateStatusOfServiceRequest(RequestDTO requestDTO) {
        fhirServiceApiInterface.updatePatientStatus(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
        return Boolean.TRUE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ReferralTicketDTO> getReferralTickets(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getReferralTickets(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ReferralDetailsDTO createReferralTicket(ReferralDetailsDTO referralTicketDTO) {
        ReferralDetailsDTO referralResponse = fhirServiceApiInterface.createReferralTicket(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), referralTicketDTO);
        createReferralFollowUp(referralTicketDTO);
        return referralResponse;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String createPatientByPatientId(RequestDTO request) {
        return fhirServiceApiInterface.createPatientByPatientId(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    public EnrollmentResponseDTO updatePatient(EnrollmentRequestDTO enrollmentRequestDto){
        return fhirServiceApiInterface.updatePatient(CommonUtil.getAuthToken(), CommonUtil.getClient(), enrollmentRequestDto);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createDiagnosis(DiagnosisDTO diagnosis) {
        fhirServiceApiInterface.updatePatientDiagnosis(CommonUtil.getAuthToken(), CommonUtil.getClient(), diagnosis);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DiseaseDTO> getPatientDiagnosis(RequestDTO request) {
        return fhirServiceApiInterface.getPatientDiagnosis(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ConfirmDiagnosisDTO getPatientDiagnosisDetails(RequestDTO request) {
        return fhirServiceApiInterface.getPatientDiagnosisDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }


    /**
     * {@inheritDoc}
     */
    public List<PregnancyInfo> getPregnancyInfoByVillages(RequestDTO request) {
        Set<PregnancyInfo> pregnancyInfoList = new HashSet<>();
        List<PregnancyInfo> pregnancyInfoResponseList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            request.setSkip(skip);
            request.setLimit(limit);
            pregnancyInfoResponseList = fhirServiceApiInterface.getPregnancyInfoByVillages(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
            pregnancyInfoList.addAll(pregnancyInfoResponseList);
            skip += limit;
        } while (!pregnancyInfoResponseList.isEmpty());
        return pregnancyInfoList.stream().toList();
    }

    /**
     * Create Referral followup
     *
     * @param referralDetailsDTO - Referral details object
     */
    private void createReferralFollowUp(ReferralDetailsDTO referralDetailsDTO) {
        if (referralDetailsDTO.isReferred()) {
            closeFollowUps(referralDetailsDTO, List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.REFERRED));
            if (Constants.ICCM.equals(referralDetailsDTO.getCategory())) {
                closeFollowUps(referralDetailsDTO, List.of(Constants.ICCM, Constants.CHILDHOOD_VISIT),
                        List.of(AppointmentType.MEDICAL_REVIEW));
            } else if (Constants.RMNCH.equals(referralDetailsDTO.getCategory())) {
                closeFollowUps(referralDetailsDTO, List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                        List.of(AppointmentType.MEDICAL_REVIEW));
            }
            createFollowUp(referralDetailsDTO);
        }
    }

    /**
     * Close followups
     *
     * @param referralDetailsDTO - referralDetailsDTO object
     * @param referralTicketTypes - referral ticket types
     * @param appointmentTypes   - appointment types
     */
    private void closeFollowUps(ReferralDetailsDTO referralDetailsDTO, List<String> referralTicketTypes,
                                List<AppointmentType> appointmentTypes) {
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                referralDetailsDTO.getMemberId(), referralTicketTypes, appointmentTypes, Boolean.FALSE);
        callRegisters.forEach(callRegister -> {
            callRegister.setIsCompleted(Boolean.TRUE);
            callRegister.setLastInteractionMode(InteractionMode.MEDICAL_REVIEW);
            callRegisterRepository.save(callRegister);
        });
    }

    /**
     * Create followup
     *
     * @param referralDetailsDTO - Referral details object
     */
    private void createFollowUp(ReferralDetailsDTO referralDetailsDTO) {
        FollowUpDTO followUpDTO = constructReferralFollowUpDetails(referralDetailsDTO);
        followUpService.createFollowUp(followUpDTO);
    }

    /**
     * Construct Referral followup details
     *
     * @param referralDetailsDTO - Referral details object
     * @return FollowUpDTO
     */
    private FollowUpDTO constructReferralFollowUpDetails(ReferralDetailsDTO referralDetailsDTO) {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setPatientId(referralDetailsDTO.getPatientId());
        followUpDTO.setEncounterType(referralDetailsDTO.getCategory());
        followUpDTO.setReason(referralDetailsDTO.getReferredReason());
        followUpDTO.setReferredSiteId(referralDetailsDTO.getReferredSiteId());
        followUpDTO.setEncounterName(referralDetailsDTO.getAssessmentName());
        followUpDTO.setPatientStatus(Constants.REFERRED);
        followUpDTO.setHouseholdId(referralDetailsDTO.getHouseholdId());
        followUpDTO.setEncounterDate(referralDetailsDTO.getProvenance().getModifiedDate());
        followUpDTO.setMemberId(referralDetailsDTO.getMemberId());
        followUpDTO.setType(AppointmentType.REFERRED);
        followUpDTO.setEncounterId(referralDetailsDTO.getEncounterId());
        followUpDTO.setVillageId(referralDetailsDTO.getVillageId());
        followUpDTO.setFirstInteractionMode(InteractionMode.MEDICAL_REVIEW);
        followUpDTO.setProvenance(referralDetailsDTO.getProvenance());
        return followUpDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> searchPatients(PatientRequestDTO requestDTO) {
        if (Objects.nonNull(requestDTO.getType())
            && ((Constants.ASSESSMENT.equalsIgnoreCase(requestDTO.getType())
                    || Constants.MY_PATIENTS.equalsIgnoreCase(requestDTO.getType())
                    || Constants.INVESTIGATION.equalsIgnoreCase(requestDTO.getType())
                    || Constants.DISPENSE.equalsIgnoreCase(requestDTO.getType())
                    || Constants.NUTRITION_LIFESTYLE.equalsIgnoreCase(requestDTO.getType()))
                    && Objects.nonNull(requestDTO.getTenantId()))) {
                List<HealthFacilityDTO> healthFacilities = adminServiceApiInterface.getAllHealthFacilitiesByTenantId(
                        CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO.getTenantId());
                requestDTO.setSiteId(healthFacilities.stream().map(HealthFacilityDTO::getFhirId).collect(
                        Collectors.joining(Constants.COMMA)));
        }
        return  fhirServiceApiInterface.searchPatients(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    public PatientDetailsDTO searchPatientDetails(PatientDTO patientDTO) {
        return  fhirServiceApiInterface.searchPatientDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), patientDTO);
    }

    /**
     * {@inheritDoc}
     */
    public PregnancyDetailsDTO createPregnancyDetails(PregnancyDetailsDTO pregnancyDetailsDTO) {
        Logger.logInfo(("To create pregnancy details for patient related person fhir id - ")
                .concat(pregnancyDetailsDTO.getMemberReference()));
        if (Objects.isNull(pregnancyDetailsDTO.getMemberReference())) {
            throw new BadRequestException(3000);
        }
        pregnancyDetailsDTO.setType(Constants.PREGNANCY);
        pregnancyDetailsDTO = fhirServiceApiInterface.createPregnancyDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), pregnancyDetailsDTO);
        return pregnancyDetailsDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PregnancyDetailsDTO getPregnancyDetails(RequestDTO requestData) {
        Logger.logInfo("To create pregnancy details for patient related person fhir id - "
                .concat(requestData.getId()));
        if (Objects.isNull(requestData.getId())) {
            throw new BadRequestException(3000);
        }
        return fhirServiceApiInterface.getPregnancyDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestData);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deletePatientByPatientId(RequestDTO requestData) {
        Logger.logInfo(("To delete patient details for patient fhir id - ")
                .concat(requestData.getPatientId()));
        if (Objects.isNull(requestData.getPatientId())) {
            throw new BadRequestException(3000);
        }
        fhirServiceApiInterface.deletePatientByPatientId(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestData);
        removeFollowUps(requestData);
        removeTransfer(requestData);
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean updatePregnancyANCRisk(PregnancyDetailsDTO pregnancyDetailsDTO) {
        return fhirServiceApiInterface.updatePregnancyANCRisk(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                pregnancyDetailsDTO);
    }

    /**
     * {@inheritDoc}
     */
    public void updateReferredSite(ScreeningLogRequestDTO requestDTO) {
        fhirServiceApiInterface.updateReferredSite(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                requestDTO);
    }

    @Override
    public Map<String, Object> getPatientWgsData(WgsDataDTO wgsDataDTO) {
        Map<String, Object> responseMap = new HashMap<>();

        if (Boolean.TRUE.equals(validateInput(wgsDataDTO))) {
            throw new DataNotAcceptableException(3002);
        }
        List<String> indicators = new ArrayList<>();
        Optional.ofNullable(wgsDataDTO.getHeight()).ifPresent(height -> wgsDataDTO.setHeight(Math.round(height * Constants.TEN) / Constants.DOUBLE_TEN));
        Optional.ofNullable(wgsDataDTO.getAgeInMonths()).ifPresent(age -> indicators.add(Constants.WFA));
        Optional.ofNullable(wgsDataDTO.getHeight()).ifPresent(height -> indicators.add(Constants.WFH));

        indicators.forEach(indicator -> {
            List<WgsData> wgsDataList = wgsDataRepository.findBySexAndGivenAndIndicator(wgsDataDTO.getGender().equals(Constants.MALE)
                    ? Constants.ONE : Constants.TWO, indicator.equals(Constants.WFA) ? wgsDataDTO.getAgeInMonths() : wgsDataDTO.getHeight(), indicator);

            if (wgsDataList.isEmpty()) {
                throw new DataNotFoundException(3003);
            }
            wgsDataList.forEach(wgsData -> calculateZIndex(wgsDataDTO.getWeight(), wgsData, responseMap));
        });
        return responseMap;
    }

    /**
     * Method used to validate the input params for WGS calculation.
     *
     * @param wgsDataDTO - Contains the input params to get WGS data.
     * @return           - Returns true if any of the input fields are invalid.
     */
    private Boolean validateInput(WgsDataDTO wgsDataDTO) {
        return Objects.isNull(wgsDataDTO.getGender()) || Objects.isNull(wgsDataDTO.getWeight())
                || (Objects.isNull(wgsDataDTO.getHeight()) && Objects.isNull(wgsDataDTO.getAgeInMonths()))
                || isInValidAge(wgsDataDTO.getAgeInMonths()) || isInValidHeight(wgsDataDTO.getHeight());
    }

    /**
     * Validates the age in months to ensure it falls within acceptable bounds.
     *
     * @param ageInMonths - Age in months value to check.
     * @return            - Returns true if age is invalid.
     */
    private boolean isInValidAge(Double ageInMonths) {
        return Objects.nonNull(ageInMonths) && (ageInMonths < Constants.ZERO || ageInMonths > Constants.SIXTY);
    }

    /**
     * Validates the height to ensure it falls within acceptable bounds.
     *
     * @param height - height The height value to check.
     * @return       - Returns true if height is invalid.
     */
    private boolean isInValidHeight(Double height) {
        return Objects.nonNull(height) && (height < Constants.FORTY_FIVE || height > Constants.ONE_HUNDRED_TWENTY);
    }

    /**
     * Method calculates the Z-index based on the provided weight and WGS data, adjusting it if necessary.
     *
     * @param weight      - Weight of the patient.
     * @param wgsData     - WGS data containing parameters m, l, and s.
     * @param responseMap - A map to store the calculated Z-index with the corresponding indicator.
     */
    private void calculateZIndex(double weight, WgsData wgsData, Map<String, Object> responseMap) {
        double m = wgsData.getM();
        double l = wgsData.getL();
        double s = wgsData.getS();

        double z = (Math.pow((weight / m), l) - Constants.ONE) / (l * s);

        double sD3pos = m * Math.pow((Constants.ONE + l * s * Constants.THREE), (Constants.ONE / l));
        double SD2pos = m * Math.pow((Constants.ONE + l * s * Constants.TWO), (Constants.ONE / l));
        double SD23pos = sD3pos - SD2pos;

        double sD3neg = m * Math.pow((Constants.ONE + l * s * Constants.NEGATIVE_THREE), (Constants.ONE / l));
        double sD2neg = m * Math.pow((Constants.ONE + l * s * Constants.NEGATIVE_TWO), (Constants.ONE / l));
        double sD23neg = sD2neg - sD3neg;

        double zAdjusted = z;

        // Adjust z-score if it exceeds bounds
        if (z > 3) {
            zAdjusted = Constants.THREE + ((weight - sD3pos) / SD23pos);
        } else if (z < Constants.NEGATIVE_THREE) {
            zAdjusted = Constants.NEGATIVE_THREE + ((weight - sD3neg) / sD23neg);
        }
        responseMap.put(wgsData.getIndicator(), Math.round(zAdjusted * Constants.ONE_HUNDRED) / Constants.ONE_HUNDRED_IN_DOUBLE);
    }

    /**
     * This function removes the call registers associated with the patient.
     *
     * @param requestDTO the request containing the member id.
     */
    private void removeFollowUps(RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getMemberId())) {
            throw new BadRequestException(3000);
        }
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndIsDeletedFalse(
                requestDTO.getMemberId());
        callRegisters.forEach(callRegister -> {
            callRegister.setIsCompleted(Boolean.TRUE);
            callRegister.setDeleted(Boolean.TRUE);
            callRegister.setActive(Boolean.FALSE);
        });
        callRegisterRepository.saveAll(callRegisters);
    }

    /**
     * This function removes the transfer.
     *
     * @param requestDTO the request containing the patient id.
     */
    private void removeTransfer(RequestDTO requestDTO) {
        List<PatientTransfer> patientTransfers = patientTransferRepository.findByPatientFhirId(requestDTO.getPatientId());
        patientTransfers.forEach(callRegister -> {
            callRegister.setDeleted(Boolean.TRUE);
            callRegister.setActive(Boolean.FALSE);
        });
        patientTransferRepository.saveAll(patientTransfers);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PatientDetailsDTO> getPatientDetailsByVillageIds(RequestDTO request) {
        List<PatientDetailsDTO> patientDetails = new ArrayList<>();
        if (Objects.isNull(request.getVillageIds()) || request.getVillageIds().isEmpty()) {
            return patientDetails;
        }
        List<PatientDetailsDTO> tempPatientDetails;
        request.setVillageId(String.join(Constants.COMMA, request.getVillageIds()));
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            request.setSkip(skip);
            request.setLimit(limit);
            tempPatientDetails = fhirServiceApiInterface.listPatientDetails(CommonUtil.getAuthToken(),
                    CommonUtil.getClient(), request);
            patientDetails.addAll(tempPatientDetails);
            skip += limit;
        } while (!tempPatientDetails.isEmpty());
        return patientDetails;
    }

}
