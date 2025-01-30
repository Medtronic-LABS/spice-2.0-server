package com.mdtlabs.coreplatform.spiceservice.patienttransfer.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import jakarta.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferUpdateRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.repository.PatientTransferRepository;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.service.PatientTransferService;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * This service class contain all the business logic and perform all the
 * operation here.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 07, 2024
 */
@Service
public class PatientTransferServiceImpl implements PatientTransferService {


    private final PatientTransferRepository patientTransferRepository;
    private final FhirServiceApiInterface fhirServiceApiInterface;
    private final CustomizedModulesService customizedModulesService;
    private final FollowUpService followUpService;

    @Autowired
    public PatientTransferServiceImpl(PatientTransferRepository patientTransferRepository,
                                      FhirServiceApiInterface fhirServiceApiInterface,
                                      CustomizedModulesService customizedModulesService,
                                      FollowUpService followUpService) {
        this.patientTransferRepository = patientTransferRepository;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.customizedModulesService = customizedModulesService;
        this.followUpService = followUpService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createPatientTransfer(PatientTransferRequestDTO patientTransferDto) {
        validateRequest(patientTransferDto);
        String patientFhirId = patientTransferDto.getPatientReference();
        RequestDTO requestDto = new RequestDTO();
        requestDto.setPatientReference(patientFhirId);
        this.validatePatientTransfer(requestDto);
        PatientTransfer patientTransfer = new PatientTransfer();
        patientTransfer.setTransferSite(new HealthFacility(patientTransferDto.getTransferSite()));
        patientTransfer.setOldSite(new HealthFacility(patientTransferDto.getOldSite()));
        patientTransfer.setTransferTo(new User(patientTransferDto.getTransferTo()));
        patientTransfer.setMemberId(patientTransferDto.getMemberReference());
        patientTransfer.setTransferBy(new User(UserContextHolder.getUserDto().getId()));
        patientTransfer.setTransferReason(patientTransferDto.getTransferReason());
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransfer.setTenantId(UserSelectedTenantContextHolder.get());
        patientTransfer.setPatientFhirId(patientTransferDto.getPatientReference());
        patientTransferRepository.save(patientTransfer);
    }

    /**
     * <p>
     * Used to validate the request.
     * </p>
     *
     * @param requestDTO
     */
    private void validateRequest(PatientTransferRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getPatientReference()) || requestDTO.getPatientReference().isEmpty() ||
                Objects.isNull(requestDTO.getTransferReason()) || requestDTO.getTransferReason().isEmpty() ||
                Objects.isNull(requestDTO.getTransferTo()) || Objects.isNull(requestDTO.getOldSite()) ||
                Objects.isNull(requestDTO.getTransferSite())) {
            throw new DataNotAcceptableException(16009);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> validatePatientTransfer(RequestDTO requestDTO) {
        PatientTransfer existingPatientTransfer = patientTransferRepository
                .findByPatientTrackIdAndTransferStatus(requestDTO.getPatientReference(), PatientTransferStatus.PENDING);
        if (!Objects.isNull(existingPatientTransfer)) {
            throw new DataNotAcceptableException(16007);
        }
        return fhirServiceApiInterface.validatePatientTransfer(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional
    public void updatePatientTransfer(PatientTransferUpdateRequestDTO patientTransferDto) {
        PatientTransfer patientTransfer = patientTransferRepository.findByIdAndIsDeleted(patientTransferDto.getId(), false);
        if (Objects.isNull(patientTransfer)) {
            throw new DataNotFoundException(16014);
        }
        if (patientTransfer.getTransferSite().isDeleted()) {
            throw new DataNotFoundException(16010);
        }
        PatientTransferStatus transferStatus = patientTransfer.getTransferStatus();
        if ((transferStatus == PatientTransferStatus.ACCEPTED || transferStatus == PatientTransferStatus.REJECTED
                || transferStatus == PatientTransferStatus.CANCELED) || !patientTransfer.isShow()) {
            throw new DataNotAcceptableException(16011);
        }
        this.updatePatientTransferStatus(patientTransferDto, patientTransfer);
    }

    /**
     * <p>
     * This method is used to retrieve a list of districts based on a search request.
     * </p>
     *
     * @param patientTransferDto {@link PatientTransferUpdateRequestDTO} The RequestDTO  contains necessary information
     *                         to update the patient transfer
     * @param patientTransfer {@link PatientTransfer} The PatientTransfer request contains necessary information
     *                         of patient transfer object
     * @return {@link PatientTransfer} The response PatientTransfer containing updated
     * patient tranfer object
     */
    private PatientTransfer updatePatientTransferStatus(PatientTransferUpdateRequestDTO patientTransferDto,
                                                        PatientTransfer patientTransfer) {
        PatientTransferStatus transferStatus = patientTransferDto.getTransferStatus();
        switch (transferStatus) {
            case ACCEPTED:
                this.userValidateInStatusUpdate(patientTransfer.getTransferTo().getId());
                patientTransfer.setTransferStatus(transferStatus);
                RequestDTO requestDTO = new RequestDTO();
                requestDTO.setPatientReference(patientTransfer.getPatientFhirId());
                requestDTO.setTransferSite(patientTransfer.getTransferSite().getFhirId());
                requestDTO.setProvenance(patientTransferDto.getProvenance());
                requestDTO.setMemberId(patientTransferDto.getMemberReference());
                patientTransfer.setTenantId(UserSelectedTenantContextHolder.get());
                this.updatePatientRecords(requestDTO, patientTransfer.getTenantId());
                break;
            case REJECTED:
                String rejectReason = patientTransferDto.getRejectReason();
                if (Objects.isNull(rejectReason)) {
                    throw new DataNotAcceptableException(16013);
                }
                this.userValidateInStatusUpdate(patientTransfer.getTransferTo().getId());
                patientTransfer.setRejectReason(rejectReason);
                patientTransfer.setTransferStatus(transferStatus);
                break;
            case REMOVED:
                this.userValidateInStatusUpdate(patientTransfer.getTransferBy().getId());
                patientTransfer.setShow(false);
                break;
            case CANCELED:
                this.userValidateInStatusUpdate(patientTransfer.getTransferBy().getId());
                patientTransfer.setTransferStatus(transferStatus);
                patientTransfer.setShow(false);
                break;
            default:
                throw new DataNotAcceptableException(16012);
        }
        return patientTransferRepository.save(patientTransfer);
    }

    /**
     * <p>
     * This method is used to authenticate the user having a permission ot not
     * </p>
     *
     * @param userId {@link User} The userI  contains necessary information
     *                         to validate the user
     */
    private void userValidateInStatusUpdate(long userId) {
        if (userId != UserContextHolder.getUserDto().getId()) {
            throw new BadCredentialsException(ErrorConstants.ERROR_USER_DOESNT_ROLE);
        }
    }

    /**
     * This method is used to update patient records
     *
     * @param requestDTO
     */
    private void updatePatientRecords(RequestDTO requestDTO, Long tenantId) {
        customizedModulesService.updateCustomizedModulesForTransfer(requestDTO.getMemberId(), tenantId);
        followUpService.transferCallRegisters(requestDTO.getMemberId(), requestDTO.getTransferSite());
        fhirServiceApiInterface.updatePatientRecords(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Long> getPatientTransferCount(RequestDTO requestDTO) {
        Long siteId = requestDTO.getHealthFacilityId();
        if (Objects.isNull(siteId)) {
            throw new DataNotAcceptableException(16008);
        }
        Map<String, Long> notification = new HashMap<>();
        notification.put(Constants.COUNT,
                patientTransferRepository.getPatientTransferCount(siteId, UserContextHolder.getUserDto().getId()));
        return notification;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getPatientTransferList(RequestDTO requestDTO) {
        Long siteId = requestDTO.getHealthFacilityId();
        if (Objects.isNull(siteId)) {
            throw new DataNotAcceptableException(16008);
        }
        Map<String, Object> data = new HashMap<>();
        Long userId = UserContextHolder.getUserDto().getId();
        List<PatientTransfer> incomingList = patientTransferRepository.getIncomingList(siteId, userId,
                PatientTransferStatus.PENDING);
        List<PatientTransfer> outgoingList = patientTransferRepository.getOutgoingList(siteId, userId);
        List<Map<String, Object>> incomingTransferList = this.getPopulatedTransferList(incomingList);
        List<Map<String, Object>> outgoingTransferList = this.getPopulatedTransferList(outgoingList);
        data.put(Constants.INCOMINGPATIENTLIST, incomingTransferList);
        data.put(Constants.OUTGOINGPATIENTLIST, outgoingTransferList);
        return data;
    }

    /**
     * <p>
     * Populates and formats a list of patient transfers into a list of maps with specific key-value pairs
     * for each transfer's details.
     * </p>
     *
     * @param transferList The list of {@link PatientTransfer} objects to be processed.
     * @return {@link List<Map<String, Object>>} A list of maps where each map contains detailed information
     * about a patient transfer, ready for presentation or further processing.
     */
    private List<Map<String, Object>> getPopulatedTransferList(List<PatientTransfer> transferList) {
        List<Map<String, Object>> patientTransferList = new ArrayList<>();
        for (PatientTransfer patientTransfer : transferList) {
            PatientDTO patientDTO = new PatientDTO();
            Map<String, Object> data = new HashMap<>();
            data.put(Constants.ID, patientTransfer.getId());
            data.put(Constants.TENANT_IDS_CLAIM, patientTransfer.getTenantId());
            data.put(Constants.TRANSFERSTATUS, patientTransfer.getTransferStatus());
            data.put(Constants.TRANSFERREASON, patientTransfer.getTransferReason());
            data.put(Constants.TRANSFERBY, this.getPopulatedUser(patientTransfer, true));
            data.put(Constants.TRANSFERTO, this.getPopulatedUser(patientTransfer, false));
            data.put(Constants.OLDSITE, this.getPopulatedSite(patientTransfer, true));
            data.put(Constants.TRANSFERSITE, this.getPopulatedSite(patientTransfer, false));
            patientTransferList.add(data);
            patientDTO.setId(patientTransfer.getMemberId());
            patientDTO.setType(Constants.ASSESSMENT);
            PatientDetailsDTO patientDetailsDTO = fhirServiceApiInterface.searchPatientDetails(CommonUtil.getAuthToken(),
                    CommonUtil.getClient(), patientDTO);
            List<String> diagnosisList = new ArrayList<>();
            if (Objects.nonNull(patientDetailsDTO.getConfirmDiagnosis())
                    && !CollectionUtils.isEmpty(patientDetailsDTO.getConfirmDiagnosis().getDiagnosis())) {
                patientDetailsDTO.getConfirmDiagnosis().getDiagnosis().forEach(diagnosis ->
                    diagnosisList.add(diagnosis.get(Constants.NAME)));
            }
            data.put(Constants.PATIENT, this.getPopulatedPatientData(patientTransfer, patientDetailsDTO, diagnosisList));

        }
        return patientTransferList;
    }

    /**
     * <p>
     * Constructs and populates a map with user-specific details from a patient transfer,
     * based on whether the user is initiating or receiving the transfer.
     * </p>
     *
     * @param patientTransfer The {@link PatientTransfer} object containing transfer details.
     * @param isTransferBy    A boolean indicating whether the user is the initiator (true) or receiver (false) of the transfer.
     * @return {@link Map<String, Object>} A map with key-value pairs representing specific details of the user
     * involved in the patient transfer.
     */
    private Map<String, Object> getPopulatedUser(PatientTransfer patientTransfer, boolean isTransferBy) {
        Map<String, Object> data = new HashMap<>();
        data.put(Constants.PARAM_FIRST_NAME,
                isTransferBy ? patientTransfer.getTransferBy().getFirstName() : patientTransfer.getTransferTo().getFirstName());
        data.put(Constants.PARAM_LAST_NAME,
                isTransferBy ? patientTransfer.getTransferBy().getLastName() : patientTransfer.getTransferTo().getLastName());
        return data;
    }

    /**
     * <p>
     * Creates and populates a map with site-specific details from a patient transfer,
     * depending on whether the site is the origin (old) or destination (new) site.
     * </p>
     *
     * @param patientTransfer The {@link PatientTransfer} object containing the transfer details.
     * @param isOldSite       A boolean indicating whether the site is the origin (true) or destination (false) of the transfer.
     * @return {@link Map<String, Object>} A map with key-value pairs representing specific details of the site
     * involved in the patient transfer.
     */
    private Map<String, Object> getPopulatedSite(PatientTransfer patientTransfer, boolean isOldSite) {
        Map<String, Object> data = new HashMap<>();
        data.put(Constants.ID, isOldSite ? patientTransfer.getOldSite().getId() : patientTransfer.getTransferSite().getId());
        data.put(Constants.NAME, isOldSite ? patientTransfer.getOldSite().getName() : patientTransfer.getTransferSite().getName());
        return data;
    }

    /**
     * <p>
     * Extracts and populates patient-specific data from a patient transfer into a map
     * with detailed information about the patient involved in the transfer.
     * </p>
     *
     * @param patientTransfer The {@link PatientTransfer} object containing details of the transfer and patient.
     * @return {@link Map<String, Object>} A map with key-value pairs representing specific details about the patient,
     * including relevant data for processing or display purposes.
     */
    private Map<String, Object> getPopulatedPatientData(PatientTransfer patientTransfer, PatientDetailsDTO patientDetailsDTO, List<String> diagnosisList) {
        Map<String, Object> data = new HashMap<>();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientId(patientTransfer.getPatientFhirId());
        PatientDTO patientDTO = fhirServiceApiInterface.getPatientById(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
        data.put(Constants.ID, patientDTO.getId());
        data.put(Constants.PARAM_FIRST_NAME, patientDetailsDTO.getFirstName().toUpperCase());
        data.put(Constants.PARAM_LAST_NAME, patientDetailsDTO.getLastName().toUpperCase());
        data.put(Constants.GENDER, patientDTO.getGender());
        data.put(Constants.PARAM_PHONE_NUMBER, patientDTO.getPhoneNumber());
        data.put(Constants.IDENTITY_VALUE, patientDetailsDTO.getNationalId());
        data.put(Constants.CONFIRM_DIAGNOSIS, diagnosisList);
        data.put(Constants.AGE, patientDetailsDTO.getAge());
        data.put(Constants.PROGRAM_ID, patientDetailsDTO.getProgramId());
        data.put(Constants.CVD_RISK_LEVEL, patientDetailsDTO.getCvdRiskLevel());
        data.put(Constants.CVD_RISK_SCORE, patientDetailsDTO.getCvdRiskScore());
        data.put(Constants.CVD_RISK_SCORE_DISPLAY, patientDetailsDTO.getCvdRiskScoreDisplay());
        data.put(Constants.ENROLLMENT_AT, patientDetailsDTO.getEnrollmentAt());
        data.put(Constants.PROVISIONAL_DIAGNOSIS, patientDetailsDTO.getProvisionalDiagnosis());
        data.put(Constants.RED_RISK_PATIENT, patientDetailsDTO.getRedRiskPatient());
        data.put(Constants.BMI, patientDetailsDTO.getBmi());
        data.put(Constants.PREGNANCY_DETAILS, patientDetailsDTO.getPregnancyDetails());

        return data;
    }

}
