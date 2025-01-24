package com.mdtlabs.coreplatform.spiceservice.followup.service.impl;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDto;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpCriteria;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.CallStatus;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.InteractionMode;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDetailDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegisterDetail;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterDetailRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;

/**
 * <p>
 * This class is a service class to perform operation on follow up
 * operations.
 * </p>
 *
 * @author Maria Antony Created on April 29, 2024.
 */
@Service
public class FollowUpServiceImpl implements FollowUpService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final CallRegisterRepository callRegisterRepository;

    private final CallRegisterDetailRepository callRegisterDetailRepository;

    private final ModelMapper modelMapper;

    @Value("${app.successful-call-attempts}")
    private int successfulCallAttempts;

    @Value("${app.unsuccessful-call-attempts}")
    private int unsuccessfulCallAttempts;

    @Value("${app.followup-malaria-in-days}")
    private int malariaCriteria;

    @Value("${app.followup-pneumonia-in-days}")
    private int pneumoniaCriteria;

    @Value("${app.followup-diarrhea-in-days}")
    private int diarrheaCriteria;

    @Value("${app.followup-escalation-limit-in-days}")
    private int escalationCriteria;

    @Value("${app.followup-referral-in-days}")
    private int referralCriteria;

    @Value("${app.followup-muac-in-days}")
    private int muacCriteria;

    @Value("${app.followup-anc-visit-in-days}")
    private int ancVisitCriteria;

    @Value("${app.followup-pnc-visit-in-days}")
    private int pncVisitCriteria;

    @Value("${app.followup-child-visit-in-days}")
    private int childVisitCriteria;

    @Value("${app.followup-call-attempts:5}")
    private int followUpCallAttempts;

    @Value("${app.screening-followup-remaining-days}")
    private int screeningFollowupRemainingDays;

    @Value("${app.assessment-followup-remaining-days}")
    private int assessmentFollowupRemainingDays;

    @Value("${app.medical-review-followup-remaining-days}")
    private int medicalReviewFollowupRemainingDays;

    @Value("${app.lost-to-followup-remaining-days}")
    private int lostToFollowupRemainingDays;

    public FollowUpServiceImpl(FhirServiceApiInterface fhirServiceApiInterface,
                               CallRegisterRepository callRegisterRepository,
                               CallRegisterDetailRepository callRegisterDetailRepository,
                               ModelMapper modelMapper) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.callRegisterRepository = callRegisterRepository;
        this.callRegisterDetailRepository = callRegisterDetailRepository;
        this.modelMapper = modelMapper;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public FollowUpDTO addCallRegister(CallRegister callRegister, boolean updateOldCallRegister) {
        List<CallRegister> oldCallRegisters
                = callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(
                callRegister.getMemberId(), callRegister.getType());
        callRegister.setIsInitiated(updateOldCallRegister ? Constants.BOOLEAN_FALSE : Constants.BOOLEAN_TRUE);
        if (!oldCallRegisters.isEmpty() && updateOldCallRegister) {
            oldCallRegisters.forEach(oldCallRegister -> {
                oldCallRegister.setDeleted(Boolean.TRUE);
                oldCallRegister.setIsCompleted(Boolean.TRUE);
                if (Objects.isNull(callRegister.getNextBPAssessmentDate())) {
                    callRegister.setNextBPAssessmentDate(oldCallRegister.getNextBPAssessmentDate());
                }

                if (Objects.isNull(callRegister.getNextBGAssessmentTime())) {
                    callRegister.setNextBGAssessmentTime(oldCallRegister.getNextBGAssessmentTime());
                }});
            callRegisterRepository.saveAll(oldCallRegisters);
        }

        if (Boolean.TRUE.equals(updateOldCallRegister)) {
            callRegister.setIsCompleted(Constants.BOOLEAN_FALSE);
        } else {
            if (followUpCallAttempts == callRegister.getAttempts()) {
                throw new DataNotAcceptableException(23105);
            }
            if (Constants.BOOLEAN_TRUE.equals(callRegister.getIsInitiated())) {
                throw new DataConflictException(23103);
            }
        }
        if (AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.equals(callRegister.getType())
                || AppointmentType.ASSESSMENT.equals(callRegister.getType())) {
            addLostToFollowup(callRegister);
        }
        return modelMapper.map(callRegisterRepository.save(callRegister), FollowUpDTO.class);
    }

    /**
     * {@inheritDoc}
     */
    public FollowUpDTO createFollowUp(FollowUpDTO followUp) {
        if (Constants.NON_COMMUNITY.equals(followUp.getAppType())) {
            return createNcdFollowUp(followUp, Constants.BOOLEAN_TRUE);
        } else {
            ModelMapper mapper = new ModelMapper();
            CallRegister callRegister = mapper.map(followUp, CallRegister.class);
            callRegister.setIsCompleted(Boolean.FALSE);
            callRegister.setIsWrongNumber(Boolean.FALSE);
            callRegister.setAttempts(Constants.ZERO);
            callRegister.setVisits(Constants.ZERO);
            callRegister.setCreatedBy(followUp.getProvenance().getSpiceUserId());
            callRegister.setUpdatedBy(followUp.getProvenance().getSpiceUserId());
            callRegister = callRegisterRepository.save(callRegister);
            if (Objects.nonNull(followUp.getFollowUpDetails())) {
                CallRegister finalCallRegister = callRegister;
                followUp.getFollowUpDetails().forEach(followUpDetail -> {
                    CallRegisterDetail callRegisterDetail = mapper.map(followUpDetail, CallRegisterDetail.class);
                    callRegisterDetail.setCallRegisterId(finalCallRegister.getId());
                    callRegisterDetail.setCreatedBy(followUp.getProvenance().getSpiceUserId());
                    callRegisterDetail.setUpdatedBy(followUp.getProvenance().getSpiceUserId());
                    callRegisterDetailRepository.save(callRegisterDetail);
                });
            }
            return mapper.map(callRegister, FollowUpDTO.class);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public FollowUpDTO createNcdFollowUp(FollowUpDTO followUpDTO, boolean updateOldCallRegister) {
        ModelMapper modelMapper = new ModelMapper();
        CallRegister callRegister = modelMapper.map(followUpDTO, CallRegister.class);
        List<CallRegister> oldCallRegisters
                = callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(
                callRegister.getMemberId(), callRegister.getType());
        callRegister.setIsInitiated(updateOldCallRegister ? Constants.BOOLEAN_FALSE : Constants.BOOLEAN_TRUE);
        if (!oldCallRegisters.isEmpty() && updateOldCallRegister) {
            oldCallRegisters.forEach(oldCallRegister -> oldCallRegister.setIsCompleted(Constants.BOOLEAN_TRUE));
            callRegisterRepository.saveAll(oldCallRegisters);
        }
        if (Boolean.TRUE.equals(updateOldCallRegister)) {
            callRegister.setIsCompleted(Constants.BOOLEAN_FALSE);
        } else {
            if (followUpCallAttempts == callRegister.getAttempts()) {
                throw new DataNotAcceptableException(23105);
            }
            if (Constants.BOOLEAN_TRUE.equals(callRegister.getIsInitiated())) {
                throw new DataConflictException(23103);
            }
        }
        return modelMapper.map(callRegisterRepository.save(callRegister), FollowUpDTO.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public FollowUpDTO updateFollowUp(FollowUpDTO followUp) {
        if (Constants.NON_COMMUNITY.equals(followUp.getAppType())) {
            return updateNcdOfflineFollowup(followUp);
        } else {
            if (Objects.isNull(followUp.getId())) {
                throw new DataNotAcceptableException(1010);
            }
            CallRegister callRegister = callRegisterRepository.findByIdAndIsDeletedFalse(followUp.getId());
            if (Objects.isNull(callRegister)) {
                throw new DataNotFoundException(1011);
            }
            ModelMapper mapper = new ModelMapper();
            mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
            mapper.map(followUp, callRegister);
            callRegister.setUpdatedBy(followUp.getProvenance().getSpiceUserId());
            processFollowUpUpdate(followUp, callRegister);
            callRegister = callRegisterRepository.save(callRegister);
            if (Objects.nonNull(followUp.getFollowUpDetails())) {
                CallRegister finalCallRegister = callRegister;
                followUp.getFollowUpDetails().forEach(followUpDetail -> {
                    CallRegisterDetail callRegisterDetail = mapper.map(followUpDetail, CallRegisterDetail.class);
                    callRegisterDetail.setCallRegisterId(finalCallRegister.getId());
                    if (Objects.isNull(callRegisterDetail.getId())) {
                        callRegisterDetail.setCreatedBy(followUp.getProvenance().getSpiceUserId());
                    }
                    callRegisterDetail.setUpdatedBy(followUp.getProvenance().getSpiceUserId());
                    callRegisterDetailRepository.save(callRegisterDetail);
                });
            }
            return mapper.map(callRegister, FollowUpDTO.class);
        }
    }

    /**
     * Update ncd followup based on offline
     *
     * @param followUp offline follow up detail
     * @return {@link FollowUpDTO}
     */
    private FollowUpDTO updateNcdOfflineFollowup(FollowUpDTO followUp) {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(followUp.getId())) {
            throw new BadRequestException(23101);
        }
        CallRegister oldCallRegister = null;
        try {
            Optional<CallRegister> callRegister = callRegisterRepository.findById(followUp.getId());
            if (callRegister.isPresent()) {
                oldCallRegister = callRegister.get();
            }
        } catch (IllegalArgumentException exception) {
            return followUp;
        }
        if (Objects.nonNull(followUp.getFollowUpDetails()) && !followUp.getFollowUpDetails().isEmpty()) {
            followUp.getFollowUpDetails().sort(Comparator.comparing(FollowUpDetailDTO::getAttempts));
            for (FollowUpDetailDTO followUpDetail : followUp.getFollowUpDetails()) {
                // call register
                String status = followUpDetail.getStatus().name();
                if (!(status.equals(Constants.SUCCESSFUL) || status.equals(Constants.UN_SUCCESSFUL) ||
                        status.equals(Constants.UN_ANSWERED) || status.equals(Constants.WRONG_NUMBER))) {
                    continue;
                }
                int currentAttempts = (0 == oldCallRegister.getAttempts())
                        ? Constants.ONE : oldCallRegister.getAttempts() + Constants.ONE;
                if (followUpCallAttempts <= currentAttempts) {
                    oldCallRegister.setIsCompleted(Constants.BOOLEAN_TRUE);
                    oldCallRegister.setDeleted(Boolean.TRUE);
                }
                String reason = followUpDetail.getReason();
                if (Constants.WRONG_NUMBER.equals(reason)) {
                    oldCallRegister.setIsCompleted(Constants.BOOLEAN_TRUE);
                    oldCallRegister.setIsWrongNumber(Boolean.TRUE);
                    oldCallRegister.setDeleted(Boolean.TRUE);
                }
                oldCallRegister.setIsInitiated(followUp.getIsInitiated());
                oldCallRegister.setUpdatedBy(UserContextHolder.getUserDto().getId());
                modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
                oldCallRegister.setAttempts(currentAttempts);
                oldCallRegister = callRegisterRepository.save(oldCallRegister);

                // call register detail
                CallRegisterDetail callRegisterDetail = modelMapper.map(followUpDetail, CallRegisterDetail.class);
                callRegisterDetail.setCreatedBy(UserContextHolder.getUserDto().getId());
                callRegisterDetail.setUpdatedBy(UserContextHolder.getUserDto().getId());
                callRegisterDetail.setAttempts(currentAttempts);
                callRegisterDetail.setCallRegisterId(oldCallRegister.getId());
                callRegisterDetailRepository.save(callRegisterDetail);
            }
        }
        return modelMapper.map(oldCallRegister, FollowUpDTO.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public FollowUpDTO updateNcdFollowUp(FollowUpDTO followUp) {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(followUp.getId())) {
            throw new BadRequestException(23101);
        }
        CallRegister oldCallRegister = callRegisterRepository.findByIdAndIsDeletedFalseAndIsCompletedFalse(
                followUp.getId());
        if (Objects.isNull(oldCallRegister)) {
            throw new DataNotFoundException(23102);
        }
        oldCallRegister.setIsInitiated(followUp.getIsInitiated());
        oldCallRegister.setUpdatedAt(new Date());
        oldCallRegister.setUpdatedBy(followUp.getProvenance().getSpiceUserId());
        oldCallRegister.setUserId(followUp.getProvenance().getSpiceUserId());
        oldCallRegister = callRegisterRepository.save(oldCallRegister);
        List<FollowUpDetailDTO> followUpDetailsDTO = new ArrayList<>();
        if (Objects.nonNull(followUp.getFollowUpDetails()) && !followUp.getFollowUpDetails().isEmpty()) {
            for (FollowUpDetailDTO followUpDetail : followUp.getFollowUpDetails()) {
                String status = followUpDetail.getStatus().name();
                if (!(status.equals(Constants.SUCCESSFUL) || status.equals(Constants.UN_SUCCESSFUL) ||
                        status.equals(Constants.UN_ANSWERED) || status.equals(Constants.WRONG_NUMBER))) {
                    throw new BadRequestException(23107);
                }
                if (Boolean.TRUE.equals(oldCallRegister.getIsCompleted())) {
                    throw new DataNotFoundException(23103);
                }
                int currentAttempts = (0 == oldCallRegister.getAttempts())
                        ? Constants.ONE : oldCallRegister.getAttempts() + Constants.ONE;
                String reason = followUpDetail.getReason();
                if (followUpCallAttempts <= currentAttempts) {
                    oldCallRegister.setIsCompleted(Constants.BOOLEAN_TRUE);
                    oldCallRegister.setDeleted(Boolean.TRUE);
                }
                if (Constants.WRONG_NUMBER.equals(reason)) {
                    oldCallRegister.setIsCompleted(Constants.BOOLEAN_TRUE);
                    oldCallRegister.setIsWrongNumber(Boolean.TRUE);
                    oldCallRegister.setDeleted(Boolean.TRUE);
                }
                oldCallRegister.setIsInitiated(followUp.getIsInitiated());
                oldCallRegister.setUpdatedBy(UserContextHolder.getUserDto().getId());
                modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
                CallRegisterDetail callRegisterDetail = modelMapper.map(followUpDetail, CallRegisterDetail.class);
                if (Boolean.FALSE.equals(followUpDetail.isInitiated())) {
                    oldCallRegister.setAttempts(currentAttempts);
                }
                if (Objects.isNull(callRegisterDetail.getId())) {
                    callRegisterDetail.setCreatedBy(followUp.getProvenance().getSpiceUserId());
                }
                callRegisterDetail.setAttempts(currentAttempts);
                callRegisterDetail.setUpdatedBy(followUp.getProvenance().getSpiceUserId());
                oldCallRegister = callRegisterRepository.save(oldCallRegister);
                callRegisterDetail.setCallRegisterId(oldCallRegister.getId());
                if (Boolean.FALSE.equals(followUp.getIsInitiated())) {
                    callRegisterDetail = callRegisterDetailRepository.save(callRegisterDetail);
                }
                FollowUpDetailDTO followUpDetailDTO = modelMapper.map(callRegisterDetail, FollowUpDetailDTO.class);
                followUpDetailsDTO.add(followUpDetailDTO);
            }
        }
        FollowUpDTO followUpDTO = modelMapper.map(oldCallRegister, FollowUpDTO.class);
        followUpDTO.setFollowUpDetails(followUpDetailsDTO);
        followUpDTO.setIsInitiated(followUp.getIsInitiated());
        followUpDTO.setPhoneNumber(followUp.getPhoneNumber());
        return followUpDTO;
    }

    /**
     * {@inheritDoc}
     */
    public List<FollowUpDTO> getFollowUpList(RequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        List<FollowUpDTO> followUpList = new ArrayList<>();
        Date householdVisitDate = DateUtil.addDaysToCurrentDate(Constants.SEVEN);
        Date referralDate = DateUtil.addDaysToCurrentDate(Constants.TWO);
        Date medicalReviewDate = DateUtil.addDaysToCurrentDate(Constants.SEVEN);
        List<CallRegister> callRegisters = callRegisterRepository.findByVillageIds(request.getVillageIds(),
                householdVisitDate, referralDate, medicalReviewDate, request.getLastSyncTime(),
                request.getCurrentSyncTime(), Objects.isNull(request.getLastSyncTime()) ? Boolean.FALSE : null);
        callRegisters.forEach(callRegister -> {
            FollowUpDTO followUp = mapper.map(callRegister, FollowUpDTO.class);
            followUp.setSuccessfulAttempts(callRegister.getCallRegisterDetail().stream()
                    .filter(callRegisterDetail -> callRegisterDetail.getStatus().equals(CallStatus.SUCCESSFUL)).count());
            followUp.setUnsuccessfulAttempts(callRegister.getCallRegisterDetail().stream()
                    .filter(callRegisterDetail -> callRegisterDetail.getStatus().equals(CallStatus.UNSUCCESSFUL)).count());
            followUpList.add(followUp);
        });
        return followUpList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public CallRegisterDto getPendingCallRegister() {
        Long userId = UserContextHolder.getUserDto().getId();
        List<CallRegister> callRegisters =
                callRegisterRepository.findByUserIdAndIsInitiatedTrueAndIsDeletedFalse(userId);
        if (!callRegisters.isEmpty()) {
            PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
            patientRequestDTO.setId(callRegisters.stream().map(CallRegister::getPatientId).collect(
                    Collectors.joining(Constants.COMMA)));
            Map<String, PatientDetailsDTO> patientDetailsDTO = fhirServiceApiInterface.getPatientNcdListBySearchText(
                    CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO);
            PatientDetailsDTO patientDetail = patientDetailsDTO.get(callRegisters.getFirst().getPatientId());
            CallRegisterDto callRegisterDto = modelMapper.map(callRegisters.getFirst(), CallRegisterDto.class);
            if (Objects.nonNull(patientDetail)) {
                callRegisterDto.setName(patientDetail.getName());
                callRegisterDto.setPhoneNumber(patientDetail.getPhoneNumber());
            }
            callRegisterDto.setType(callRegisters.getFirst().getType().name());
            return callRegisterDto;
        }
        return null;
    }

    /**
     * Process data for followup update
     *
     * @param followUp     followup Details
     * @param callRegister callRegister Details
     */
    private void processFollowUpUpdate(FollowUpDTO followUp, CallRegister callRegister) {
        if (Objects.nonNull(followUp.getCalledAt())) {
            updateCalledAtTime(callRegister);
        }
        if (Boolean.TRUE.equals(followUp.getIsWrongNumber())) {
            updateWrongNumber(callRegister);
        }
        if (Objects.nonNull(followUp.getCurrentPatientStatus())) {
            switch (callRegister.getType()) {
                case HH_VISIT:
                    processHouseholdVisitFollowUpData(followUp, callRegister);
                    break;
                case REFERRED:
                    processReferralFollowUpData(followUp, callRegister);
                    break;
                case MEDICAL_REVIEW:
                    processMedicalReviewFollowUpData(followUp, callRegister);
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Process followup data for Household visit
     *
     * @param followUp     followup Details
     * @param callRegister callRegister Details
     */
    private void processHouseholdVisitFollowUpData(FollowUpDTO followUp, CallRegister callRegister) {
        boolean autoRefer = Constants.ON_TREATMENT.equals(followUp.getCurrentPatientStatus()) &&
                followUp.getSuccessfulAttempts() >= successfulCallAttempts;
        String category = getTicketCategoryHHVisit(callRegister.getEncounterType());
        if (Constants.RECOVERED.equals(followUp.getCurrentPatientStatus())) {
            closeFollowUps(callRegister, List.of(callRegister.getEncounterType()), List.of(AppointmentType.HH_VISIT), callRegister.getReason());
            closeReferralTicket(followUp, callRegister, Constants.RECOVERED, callRegister.getReason(), category, Constants.FOLLOW_UP);
        } else if (Constants.REFERRED.equals(followUp.getCurrentPatientStatus()) || autoRefer) {
            closeFollowUps(callRegister, List.of(callRegister.getEncounterType()), List.of(AppointmentType.HH_VISIT), callRegister.getReason());
            closeReferralTicket(followUp, callRegister, Constants.REFERRED, callRegister.getReason(), category, Constants.FOLLOW_UP);
            createFollowUp(constructReferralFollowUpDetails(followUp, callRegister));
            createReferralTicket(followUp, callRegister, Constants.REFERRED, autoRefer);
        }
    }

    /**
     * Process followup data for Household visit
     *
     * @param followUp     followup Details
     * @param callRegister callRegister Details
     */
    private void processReferralFollowUpData(FollowUpDTO followUp, CallRegister callRegister) {
        if (Constants.RECOVERED.equals(followUp.getCurrentPatientStatus())) {
            closeFollowUpsForMedicalReview(callRegister);
            String category = getTicketCategory(callRegister.getEncounterType());
            closeReferralTicket(followUp, callRegister, Constants.RECOVERED, null, category, Constants.FOLLOW_UP);
        }
    }

    /**
     * get ticket category based on ticket Type
     *
     * @param category category
     * @return category
     */
    private String getTicketCategory(String category) {
        if (Constants.RMNCH.equals(category)) {
            category = StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.ICCM, Constants.COMMA,
                    Constants.CHILDHOOD_VISIT, Constants.COMMA, Constants.RMNCH_VISIT, Constants.COMMA,
                    Constants.CHILD_VISIT);
        } else if (Constants.ICCM.equals(category)) {
            category = StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.ICCM, Constants.COMMA,
                    Constants.CHILDHOOD_VISIT);
        } else if (Constants.CHILDHOOD_VISIT.equals(category)) {
            category = StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.ICCM, Constants.COMMA,
                    Constants.CHILDHOOD_VISIT, Constants.COMMA, Constants.CHILD_VISIT);
        }
        return category;
    }

    /**
     * get ticket category based on ticket Type
     *
     * @param category category
     * @return category
     */
    private String getTicketCategoryHHVisit(String category) {
        if (Constants.RMNCH.equals(category)) {
            category = StringUtil.concatString(Constants.RMNCH, Constants.COMMA, Constants.RMNCH_VISIT);
        } else if (Constants.CHILDHOOD_VISIT.equals(category)) {
            category = StringUtil.concatString(Constants.CHILDHOOD_VISIT, Constants.COMMA, Constants.CHILD_VISIT);
        }
        return category;
    }

    /**
     * Close followups based on category
     *
     * @param callRegister callRegister Details
     */
    private void closeFollowUpsForMedicalReview(CallRegister callRegister) {
        closeFollowUps(callRegister, List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                List.of(AppointmentType.REFERRED), null);
        if (Constants.ICCM.equals(callRegister.getEncounterType())) {
            closeFollowUps(callRegister, List.of(Constants.ICCM, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.MEDICAL_REVIEW), null);
        } else if (Constants.RMNCH.equals(callRegister.getEncounterType()) ||
                Constants.CHILDHOOD_VISIT.equals(callRegister.getEncounterType())) {
            closeFollowUps(callRegister, List.of(Constants.RMNCH, Constants.ICCM, Constants.CHILDHOOD_VISIT),
                    List.of(AppointmentType.MEDICAL_REVIEW), null);
        }
    }

    /**
     * Process followup data for Medical review
     *
     * @param followUp     followup Details
     * @param callRegister callRegister Details
     */
    private void processMedicalReviewFollowUpData(FollowUpDTO followUp, CallRegister callRegister) {
        String category = getTicketCategory(callRegister.getEncounterType());
        if (Constants.RECOVERED.equals(followUp.getCurrentPatientStatus())) {
            closeFollowUpsForMedicalReview(callRegister);
            closeReferralTicket(followUp, callRegister, Constants.RECOVERED, null, category, Constants.FOLLOW_UP);
        } else if (Constants.REFERRED.equals(followUp.getCurrentPatientStatus())) {
            closeFollowUpsForMedicalReview(callRegister);
            closeReferralTicket(followUp, callRegister, Constants.REFERRED, null, category, Constants.FOLLOW_UP);
            createFollowUp(constructReferralFollowUpDetails(followUp, callRegister));
            createReferralTicket(followUp, callRegister, Constants.REFERRED, null);
        }
    }

    /**
     * This method is used to close referral ticket
     *
     * @param followUp      followup Details
     * @param callRegister  callRegister Details
     * @param patientStatus patient status
     * @param reason        reason
     */
    private void closeReferralTicket(FollowUpDTO followUp, CallRegister callRegister, String patientStatus, String reason, String category, String closedReason) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setPatientStatus(patientStatus);
        requestDTO.setTicketType(AppointmentType.HH_VISIT.equals(callRegister.getType()) ?
                Constants.ASSESSMENT : Constants.MEDICAL_REVIEW);
        requestDTO.setCategory(category);
        requestDTO.setReason(reason);
        requestDTO.setMemberId(callRegister.getMemberId());
        requestDTO.setCloseReferralTicket(Boolean.TRUE);
        requestDTO.setEncounterId(callRegister.getEncounterId());
        requestDTO.setProvenance(followUp.getProvenance());
        requestDTO.setClosedEncounterType(callRegister.getEncounterName());
        requestDTO.setClosedReason(closedReason);
        fhirServiceApiInterface.updateReferralTicketByMemberId(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     * This method is used to create referral ticket
     *
     * @param followUp             followup Details
     * @param callRegister         callRegister Details
     * @param currentPatientStatus current patient status
     * @param autoRefer            auto refer
     */
    private void createReferralTicket(FollowUpDTO followUp, CallRegister callRegister, String currentPatientStatus, Boolean autoRefer) {
        ReferralDetailsDTO referralDetailsDTO = new ReferralDetailsDTO();
        referralDetailsDTO.setPatientStatus(currentPatientStatus);
        referralDetailsDTO.setAutoReferral(Boolean.TRUE.equals(autoRefer));
        referralDetailsDTO.setReferredReason(callRegister.getReason());
        referralDetailsDTO.setMemberId(callRegister.getMemberId());
        referralDetailsDTO.setCurrentPatientStatus(currentPatientStatus);
        referralDetailsDTO.setProvenance(followUp.getProvenance());
        referralDetailsDTO.setCategory(callRegister.getEncounterType());
        referralDetailsDTO.setPatientId(callRegister.getPatientId());
        referralDetailsDTO.setReferred(Boolean.TRUE);
        referralDetailsDTO.setReferredSiteId(followUp.getProvenance().getOrganizationId());
        referralDetailsDTO.setType(Constants.MEDICAL_REVIEW);
        fhirServiceApiInterface.createReferralTicket(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), referralDetailsDTO);
    }

    /**
     * {@inheritDoc}
     */
    public FollowUpCriteria getFollowUpCriteria(RequestDTO request) {
        FollowUpCriteria followUpCriteria = new FollowUpCriteria();
        if (Constants.NON_COMMUNITY.equalsIgnoreCase(request.getAppType())) {
            followUpCriteria.setFollowupAttempts(followUpCallAttempts);
            followUpCriteria.setScreeningFollowupRemainingDays(screeningFollowupRemainingDays);
            followUpCriteria.setAssessmentFollowupRemainingDays(assessmentFollowupRemainingDays);
            followUpCriteria.setMedicalReviewFollowupRemainingDays(medicalReviewFollowupRemainingDays);
            followUpCriteria.setLostToFollowupRemainingDays(lostToFollowupRemainingDays);
        } else {
            followUpCriteria.setMalaria(malariaCriteria);
            followUpCriteria.setDiarrhea(diarrheaCriteria);
            followUpCriteria.setPneumonia(pneumoniaCriteria);
            followUpCriteria.setMuac(muacCriteria);
            followUpCriteria.setEscalation(escalationCriteria);
            followUpCriteria.setReferral(referralCriteria);
            followUpCriteria.setAncVisit(ancVisitCriteria);
            followUpCriteria.setPncVisit(pncVisitCriteria);
            followUpCriteria.setChildVisit(childVisitCriteria);
            followUpCriteria.setSuccessfulAttempts(successfulCallAttempts);
            followUpCriteria.setUnsuccessfulAttempts(unsuccessfulCallAttempts);
        }
        return followUpCriteria;
    }

    /**
     * Close followups
     *
     * @param callRegisterEntity callRegister Details
     * @param appointmentTypes   - list of appointment types
     * @param encounterTypes - list of encounter types
     * @param reason             - reason
     */
    private void closeFollowUps(CallRegister callRegisterEntity, List<String> encounterTypes,
                                List<AppointmentType> appointmentTypes, String reason) {
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                callRegisterEntity.getMemberId(), encounterTypes, appointmentTypes, Boolean.FALSE);
        if (Objects.nonNull(reason)) {
            List<String> reasons = Arrays.stream(reason.split(Constants.COMMA)).map(String::trim).sorted().toList();
            callRegisters = callRegisters.stream().filter(callRegister -> Objects.nonNull(callRegister.getReason()))
                    .filter(callRegister -> reasons.equals(
                            Arrays.stream(callRegister.getReason().split(Constants.COMMA)).map(String::trim).sorted()
                                    .toList())).toList();
        }
        callRegisters.forEach(callRegister -> {
            callRegister.setIsCompleted(Boolean.TRUE);
            callRegister.setUpdatedBy(callRegisterEntity.getUpdatedBy());
            callRegister.setLastInteractionMode(InteractionMode.FOLLOW_UP);
            callRegisterRepository.save(callRegister);
        });
    }

    /**
     * Update called at time for members
     *
     * @param callRegisterEntity callRegister Details
     */
    private void updateCalledAtTime(CallRegister callRegisterEntity) {
        List<AppointmentType> appointmentTypes = AppointmentType.HH_VISIT.equals(callRegisterEntity.getType()) ?
                List.of(AppointmentType.HH_VISIT) : List.of(AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW);
        String reason = AppointmentType.HH_VISIT.equals(callRegisterEntity.getType()) ? callRegisterEntity.getReason() : null;
        List<CallRegister> callRegisters = callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                callRegisterEntity.getMemberId(), List.of(callRegisterEntity.getEncounterType()), appointmentTypes, Boolean.FALSE);
        if (Objects.nonNull(reason)) {
            List<String> reasons = Arrays.stream(reason.split(Constants.COMMA))
                    .map(String::trim).sorted().toList();
            callRegisters = callRegisters.stream().filter(callRegister -> Objects.nonNull(callRegister.getReason()))
                    .filter(callRegister -> reasons.equals(Arrays.stream(callRegister.getReason().split(Constants.COMMA))
                            .map(String::trim).sorted().toList())).toList();
        }
        callRegisters.forEach(callRegister -> {
            callRegister.setCalledAt(callRegisterEntity.getCalledAt());
            callRegister.setUpdatedBy(callRegisterEntity.getUpdatedBy());
            callRegister.setLastInteractionMode(InteractionMode.FOLLOW_UP);
            callRegisterRepository.save(callRegister);
        });
    }

    /**
     * Update wrong number for members
     *
     * @param callRegisterEntity callRegister Details
     */
    private void updateWrongNumber(CallRegister callRegisterEntity) {
        List<CallRegister> callRegisters = callRegisterRepository
                .findByMemberIdAndIsCompletedAndIsDeletedFalse(callRegisterEntity.getMemberId(), Boolean.FALSE);
        callRegisters.forEach(callRegister -> {
            if (!AppointmentType.HH_VISIT.equals(callRegister.getType())) {
                callRegister.setIsCompleted(Boolean.TRUE);
            }
            callRegister.setIsWrongNumber(Boolean.TRUE);
            callRegister.setUpdatedBy(callRegisterEntity.getUpdatedBy());
            callRegister.setLastInteractionMode(InteractionMode.FOLLOW_UP);
            callRegisterRepository.save(callRegister);
        });
    }

    /**
     * Construct Referral followup details
     *
     * @param followUp     followup Details
     * @param callRegister callRegister Details
     * @return FollowUpDTO
     */
    private FollowUpDTO constructReferralFollowUpDetails(FollowUpDTO followUp, CallRegister callRegister) {
        FollowUpDTO followUpDTO = new FollowUpDTO();
        followUpDTO.setPatientId(callRegister.getPatientId());
        followUpDTO.setEncounterType(callRegister.getEncounterType());
        followUpDTO.setReason(callRegister.getReason());
        followUpDTO.setReferredSiteId(followUp.getProvenance().getOrganizationId());
        followUpDTO.setEncounterName(callRegister.getEncounterName());
        followUpDTO.setPatientStatus(Constants.REFERRED);
        followUpDTO.setHouseholdId(callRegister.getHouseholdId());
        followUpDTO.setEncounterDate(followUp.getProvenance().getModifiedDate());
        followUpDTO.setMemberId(callRegister.getMemberId());
        followUpDTO.setType(AppointmentType.REFERRED);
        followUpDTO.setVillageId(callRegister.getVillageId());
        followUpDTO.setFirstInteractionMode(InteractionMode.FOLLOW_UP);
        followUpDTO.setProvenance(followUp.getProvenance());
        return followUpDTO;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseListDTO<FollowUpDTO> getFollowUpPatients(PatientRequestDTO patientRequestDTO) {
        ResponseListDTO<FollowUpDTO> response = new ResponseListDTO<>();
        if (AppointmentType.SCREENED.name().equalsIgnoreCase(patientRequestDTO.getType())) {
            response = getScreeningFollowUpPatients(patientRequestDTO);
        } else if (AppointmentType.ASSESSMENT.name().equalsIgnoreCase(patientRequestDTO.getType())) {
            response = getAssessmentFollowUpPatients(patientRequestDTO);
        } else if (AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.name().equalsIgnoreCase(patientRequestDTO.getType())) {
            response = getMedicalReviewFollowUpPatients(patientRequestDTO);
        } else if (AppointmentType.LOST_TO_FOLLOW_UP.name().equalsIgnoreCase(patientRequestDTO.getType())) {
            response = getLostToFollowUpPatients(patientRequestDTO);
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<FollowUpDTO> getAllCallRegistersByVillages(PatientRequestDTO patientRequestDTO, String type) {
        Pageable pageable = Pagination.setPagination(patientRequestDTO.getSkip(), patientRequestDTO.getLimit());
        Page<Map<String, Object>> callRegisters = callRegisterRepository.getAllCallRegisters(
                patientRequestDTO.getType(), patientRequestDTO.getLastSyncTime(),
                patientRequestDTO.getCurrentSyncTime(), Objects.isNull(patientRequestDTO.getVillageIds())
                        ? null : patientRequestDTO.getVillageIds().stream().map(
                                String::valueOf).collect(Collectors.toList()), pageable);
        Map<String, PatientDetailsDTO> patientDetails = new HashMap<>();
        if (!callRegisters.getContent().isEmpty()) {
            PatientRequestDTO patientRequest = new PatientRequestDTO();
            patientRequest.setLimit(patientRequestDTO.getLimit());
            patientRequest.setId(callRegisters.getContent().stream().map(
                    callRegister -> callRegister.get(FieldConstants.PATIENT_ID).toString())
                    .collect(Collectors.joining(Constants.COMMA)));
            if (Constants.SCREENED.equals(patientRequestDTO.getType())) {
                patientRequest.setReferredReasonsRequired(true);
            }
            patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequest);
        }
        List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters.getContent(), patientDetails, type);
        return new ArrayList<>(followUpList);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseListDTO<FollowUpDTO> getScreeningFollowUpPatients(PatientRequestDTO patientRequestDTO) {

        // date filter and sort
        Map<String, String> dateFilter = getFollowupDateFilter(patientRequestDTO);
        List<Sort.Order> sorts = new ArrayList<>();
        addSorting(sorts, patientRequestDTO, Constants.SCREENED);
        Pageable pageable = Pagination.setPagination(patientRequestDTO.getSkip(), patientRequestDTO.getLimit(), sorts);

        // remaining attempts
        List<Integer> remainingAttempts = patientRequestDTO.getRemainingAttempts();
        int callRegisterCount = Constants.ONE;
        if (!Objects.isNull(remainingAttempts)) {
            for (int index = 0; index < remainingAttempts.size(); index++) {
                remainingAttempts.set(index, followUpCallAttempts - remainingAttempts.get(index));
                callRegisterCount = remainingAttempts.get(index) == Constants.ZERO ? Constants.ZERO : Constants.ONE;
            }
        }

        Map<String, PatientDetailsDTO> patientDetails = new HashMap<>();
        if (Objects.nonNull(patientRequestDTO.getSearchText())
                && !(patientRequestDTO.getSearchText().isEmpty() || patientRequestDTO.getSearchText().isBlank())) {
            List<Map<String, Object>> callRegisters = new ArrayList<>();
            List<Map<String, Object>> callRegisterPatientIds = callRegisterRepository.getScreeningCallRegisterPatientIds(
                patientRequestDTO.getSiteId(), AppointmentType.SCREENED.name(), false,
                (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts),
                callRegisterCount, dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE));
            if (Objects.nonNull(callRegisterPatientIds) && !callRegisterPatientIds.isEmpty()) {
                patientRequestDTO.setId(callRegisterPatientIds.stream().map(
                        callRegisterPatientId -> callRegisterPatientId.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientRequestDTO.setReferredReasonsRequired(true);
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequestDTO);
            }
            if (!patientDetails.isEmpty()) {
                callRegisters = callRegisterRepository.getCallRegisterByPatientIds(patientRequestDTO.getSiteId(),
                        AppointmentType.SCREENED.name(), false, patientDetails.keySet());
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters, patientDetails, Constants.SCREENED);
            return new ResponseListDTO<>(followUpList, null);
        } else {
            Page<Map<String, Object>> callRegisters = callRegisterRepository.getScreeningCallRegister(
                    patientRequestDTO.getSiteId(), AppointmentType.SCREENED.name(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts),
                    callRegisterCount, dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE),
                    pageable);
            if (!callRegisters.getContent().isEmpty()) {
                PatientRequestDTO patientRequest = new PatientRequestDTO();
                patientRequest.setId(callRegisters.getContent().stream().map(
                                callRegister -> callRegister.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientRequest.setReferredReasonsRequired(true);
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequest);
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters.getContent(), patientDetails,
                    Constants.SCREENED);
            return new ResponseListDTO<>(followUpList, callRegisters.getTotalElements());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseListDTO<FollowUpDTO> getAssessmentFollowUpPatients(PatientRequestDTO patientRequestDTO) {

        // date filter and sort
        Map<String, String> dateFilter = getFollowupDateFilter(patientRequestDTO);
        List<Sort.Order> sorts = new ArrayList<>();
        addSorting(sorts, patientRequestDTO, Constants.ASSESSMENT);
        Pageable pageable = Pagination.setPagination(patientRequestDTO.getSkip(), patientRequestDTO.getLimit(), sorts);

        // remaining attempts
        List<Integer> remainingAttempts = patientRequestDTO.getRemainingAttempts();
        int callRegisterCount = Constants.ONE;
        if (!Objects.isNull(remainingAttempts)) {
            for (int index = 0; index < remainingAttempts.size(); index++) {
                remainingAttempts.set(index, followUpCallAttempts - remainingAttempts.get(index));
                callRegisterCount = remainingAttempts.get(index) == Constants.ZERO ? Constants.ZERO : Constants.ONE;
            }
        }

        Map<String, PatientDetailsDTO> patientDetails = new HashMap<>();
        if (Objects.nonNull(patientRequestDTO.getSearchText())
                && !(patientRequestDTO.getSearchText().isEmpty() || patientRequestDTO.getSearchText().isBlank())) {
            List<Map<String, Object>> callRegisters = new ArrayList<>();
            List<Map<String, Object>> callRegisterPatientIds = callRegisterRepository.getAssessmentCallRegisterPatientIds(
                    patientRequestDTO.getSiteId(), AppointmentType.ASSESSMENT.name(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts), callRegisterCount,
                    dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE));
            if (Objects.nonNull(callRegisterPatientIds) && !callRegisterPatientIds.isEmpty()) {
                patientRequestDTO.setId(callRegisterPatientIds.stream().map(
                                callRegisterPatientId -> callRegisterPatientId.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequestDTO);
            }
            if (!patientDetails.isEmpty()) {
                callRegisters = callRegisterRepository.getCallRegisterByPatientIds(patientRequestDTO.getSiteId(),
                        AppointmentType.ASSESSMENT.name(), false, patientDetails.keySet());
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters, patientDetails, Constants.ASSESSMENT);
            return new ResponseListDTO<>(followUpList, null);
        } else {
            Page<Map<String, Object>> callRegisters = callRegisterRepository.getAssessmentCallRegister(
                    patientRequestDTO.getSiteId(), AppointmentType.ASSESSMENT.name(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts), callRegisterCount,
                    dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE), pageable);
            if (!callRegisters.getContent().isEmpty()) {
                PatientRequestDTO patientRequest = new PatientRequestDTO();
                patientRequest.setId(callRegisters.getContent().stream().map(
                                callRegister -> callRegister.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequest);
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters.getContent(), patientDetails,
                    Constants.ASSESSMENT);
            return new ResponseListDTO<>(followUpList, callRegisters.getTotalElements());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseListDTO<FollowUpDTO> getLostToFollowUpPatients(PatientRequestDTO patientRequestDTO) {

        // date filter and sort
        Map<String, String> dateFilter = getFollowupDateFilter(patientRequestDTO);
        List<Sort.Order> sorts = new ArrayList<>();
        addSorting(sorts, patientRequestDTO, Constants.LOST_TO_FOLLOW_UP);
        Pageable pageable = Pagination.setPagination(patientRequestDTO.getSkip(), patientRequestDTO.getLimit(), sorts);

        // remaining attempts
        List<Integer> remainingAttempts = patientRequestDTO.getRemainingAttempts();
        int callRegisterCount = Constants.ONE;
        if (!Objects.isNull(remainingAttempts)) {
            for (int index = 0; index < remainingAttempts.size(); index++) {
                remainingAttempts.set(index, followUpCallAttempts - remainingAttempts.get(index));
                callRegisterCount = remainingAttempts.get(index) == Constants.ZERO ? Constants.ZERO : Constants.ONE;
            }
        }

        Map<String, PatientDetailsDTO> patientDetails = new HashMap<>();
        if (Objects.nonNull(patientRequestDTO.getSearchText())
                && !(patientRequestDTO.getSearchText().isEmpty() || patientRequestDTO.getSearchText().isBlank())) {
            List<Map<String, Object>> callRegisters = new ArrayList<>();
            List<Map<String, Object>> callRegisterPatientIds = callRegisterRepository.getLostToFollowUpCallRegisterPatientIds(
                    AppointmentType.LOST_TO_FOLLOW_UP.name(), patientRequestDTO.getSiteId(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts),
                    callRegisterCount, dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE));
            if (Objects.nonNull(callRegisterPatientIds) && !callRegisterPatientIds.isEmpty()) {
                patientRequestDTO.setId(callRegisterPatientIds.stream().map(
                                callRegisterPatientId -> callRegisterPatientId.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequestDTO);
            }
            if (!patientDetails.isEmpty()) {
                callRegisters = callRegisterRepository.getCallRegisterByPatientIds(patientRequestDTO.getSiteId(),
                        AppointmentType.LOST_TO_FOLLOW_UP.name(), false, patientDetails.keySet());
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters, patientDetails, Constants.LOST_TO_FOLLOW_UP);
            return new ResponseListDTO<>(followUpList, null);
        } else {
            Page<Map<String, Object>> callRegisters = callRegisterRepository.getLostToFollowUpCallRegister(
                    AppointmentType.LOST_TO_FOLLOW_UP.name(), patientRequestDTO.getSiteId(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts),
                    callRegisterCount, dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE),
                    pageable);
            if (!callRegisters.getContent().isEmpty()) {
                PatientRequestDTO patientRequest = new PatientRequestDTO();
                patientRequest.setId(callRegisters.getContent().stream().map(
                        callRegister -> callRegister.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequest);
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters.getContent(), patientDetails,
                    Constants.LOST_TO_FOLLOW_UP);
            return new ResponseListDTO<>(followUpList, callRegisters.getTotalElements());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ResponseListDTO<FollowUpDTO> getMedicalReviewFollowUpPatients(PatientRequestDTO patientRequestDTO) {

        // date filter and sort
        Map<String, String> dateFilter = getFollowupDateFilter(patientRequestDTO);
        List<Sort.Order> sorts = new ArrayList<>();
        addSorting(sorts, patientRequestDTO, Constants.MEDICAL_REVIEW);
        Pageable pageable = Pagination.setPagination(patientRequestDTO.getSkip(), patientRequestDTO.getLimit(), sorts);

        // remaining attempts
        List<Integer> remainingAttempts = patientRequestDTO.getRemainingAttempts();
        int callRegisterCount = Constants.ONE;
        if (!Objects.isNull(remainingAttempts)) {
            for (int index = 0; index < remainingAttempts.size(); index++) {
                remainingAttempts.set(index, followUpCallAttempts - remainingAttempts.get(index));
                callRegisterCount = remainingAttempts.get(index) == Constants.ZERO ? Constants.ZERO : Constants.ONE;
            }
        }

        Map<String, PatientDetailsDTO> patientDetails = new HashMap<>();
        if (Objects.nonNull(patientRequestDTO.getSearchText())
                && !(patientRequestDTO.getSearchText().isEmpty() || patientRequestDTO.getSearchText().isBlank())) {
            List<Map<String, Object>> callRegisters = new ArrayList<>();
            List<Map<String, Object>> callRegisterPatientIds =
                    callRegisterRepository.getMedicalReviewFollowUpCallRegisterPatientIds(
                    patientRequestDTO.getSiteId(), AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.name(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts), callRegisterCount,
                    dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE));
            if (Objects.nonNull(callRegisterPatientIds) && !callRegisterPatientIds.isEmpty()) {
                patientRequestDTO.setId(callRegisterPatientIds.stream().map(
                                callRegisterPatientId -> callRegisterPatientId.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequestDTO);
            }
            if (!patientDetails.isEmpty()) {
                callRegisters = callRegisterRepository.getCallRegisterByPatientIds(patientRequestDTO.getSiteId(),
                        AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.name(), false, patientDetails.keySet());
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters, patientDetails,
                    Constants.MEDICAL_REVIEW);
            return new ResponseListDTO<>(followUpList, null);
        } else {
            Page<Map<String, Object>> callRegisters = callRegisterRepository.getMedicalReviewFollowUpCallRegister(
                    patientRequestDTO.getSiteId(), AppointmentType.NON_COMMUNITY_MEDICAL_REVIEW.name(), false,
                    (Objects.isNull(remainingAttempts) ? new ArrayList<>() : remainingAttempts), callRegisterCount,
                    dateFilter.get(Constants.START_DATE), dateFilter.get(Constants.END_DATE),
                    pageable);
            if (!callRegisters.getContent().isEmpty()) {
                PatientRequestDTO patientRequest = new PatientRequestDTO();
                patientRequest.setId(callRegisters.getContent().stream().map(
                                callRegister -> callRegister.get(FieldConstants.PATIENT_ID).toString())
                        .collect(Collectors.joining(Constants.COMMA)));
                patientDetails = fhirServiceApiInterface.getPatientNcdListBySearchText(CommonUtil.getAuthToken(),
                        CommonUtil.getClient(), patientRequest);
            }
            List<FollowUpDTO> followUpList = constructFollowUpList(callRegisters.getContent(), patientDetails,
                    Constants.MEDICAL_REVIEW);
            return new ResponseListDTO<>(followUpList, callRegisters.getTotalElements());
        }
    }

    /**
     * <p>
     *  Method to calculate date filter for followup based on date range.
     * </p>
     * @param patientRequestDTO - Contains data about a patient request.
     * @return - Returns map contains start and end date.
     */
    private Map<String, String> getFollowupDateFilter(PatientRequestDTO patientRequestDTO) {
        Map<String, String> dateFilter = new HashMap<>();
        String userTimezone = UserContextHolder.getUserDto().getTimezone().getOffset();
        if (!Objects.isNull(patientRequestDTO.getDateRange())) {
            if (Constants.FOLLOWUP_DAILY.equals(patientRequestDTO.getDateRange())) {
                dateFilter.put(Constants.START_DATE,
                        DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Constants.BOOLEAN_FALSE));
                dateFilter.put(Constants.END_DATE,
                        DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Constants.BOOLEAN_TRUE));
            } else if (Constants.DASHBOARD_YESTERDAY.equals(patientRequestDTO.getDateRange())) {
                dateFilter.put(Constants.START_DATE,
                        DateUtil.getUserTimezoneTime(userTimezone, Constants.ONE, Constants.BOOLEAN_FALSE));
                dateFilter.put(Constants.END_DATE,
                        DateUtil.getUserTimezoneTime(userTimezone, Constants.ONE,  Constants.BOOLEAN_TRUE));
            } else if (Constants.FOLLOWUP_WEEKLY.equals(patientRequestDTO.getDateRange())) {
                String startDate = DateUtil.getStartDayOfWeekByUserTimeZone(userTimezone);
                String endDate = DateUtil.convertDateToString(
                        DateUtil.addDateWithTimezone(DateUtil.formatDate(startDate,
                                Constants.DATE_FORMAT_WITHOUT_MILLISECOND), Constants.SEVEN, userTimezone));
                dateFilter.put(Constants.START_DATE, startDate);
                dateFilter.put(Constants.END_DATE, endDate);
            } else if (Constants.FOLLOWUP_MONTHLY.equals(patientRequestDTO.getDateRange())) {
                dateFilter.put(Constants.START_DATE, DateUtil.getStartDayOfMonthByUserTimeZone(userTimezone));
                dateFilter.put(Constants.END_DATE, DateUtil.getUserTimezoneTime(userTimezone,
                        Constants.ZERO,  Constants.BOOLEAN_TRUE));
            }
        } else if (!Objects.isNull(patientRequestDTO.getCustomDate())
                && !Objects.isNull(patientRequestDTO.getCustomDate().get(Constants.START_DATE))
                && !Objects.isNull(patientRequestDTO.getCustomDate().get(Constants.END_DATE))) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(patientRequestDTO.getCustomDate().get(Constants.START_DATE));
            dateFilter.put(Constants.START_DATE, DateUtil.getISOString(calendar));
            calendar.setTime(patientRequestDTO.getCustomDate().get(Constants.END_DATE));
            dateFilter.put(Constants.END_DATE, DateUtil.getISOString(calendar));
        }
        return dateFilter;
    }

    /**
     * Construct the followup list
     *
     * @param callRegisters the call register details as a page object
     * @param patientDetails patient details of the call register
     * @param type appointment type
     * @return the followup list
     */
    private List<FollowUpDTO> constructFollowUpList(List<Map<String, Object>> callRegisters,
                                                    Map<String, PatientDetailsDTO> patientDetails, String type) {
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        List<FollowUpDTO> followUpList = callRegisters.stream().map(callRegister -> {
            PatientDetailsDTO patientDetail = patientDetails.get(callRegister.get(FieldConstants.PATIENT_ID).toString());
            if (Objects.nonNull(patientDetail)) {
                FollowUpDTO followUpDetail = mapper.map(patientDetail, FollowUpDTO.class);
                followUpDetail.setVillageName(patientDetail.getVillage());
                setFollowupBasicDetails(followUpDetail, callRegister);
                switch (type) {
                    case (Constants.SCREENED) -> {
                        long dateDiff = 0L;
                        if (Objects.nonNull(callRegister.get(FieldConstants.SCREENING_DATE_TIME))) {
                            dateDiff = DateUtil.daysSincePast(Date.from(
                                    Instant.parse(callRegister.get(FieldConstants.SCREENING_DATE_TIME).toString())));
                            if (screeningFollowupRemainingDays < dateDiff) {
                                followUpDetail.setReferredDateSince(dateDiff - screeningFollowupRemainingDays);
                            }
                            followUpDetail.setDueDate(DateUtil.formatDate(
                                    callRegister.get(FieldConstants.SCREENING_DATE_TIME).toString()));
                        }
                    }
                    case (Constants.ASSESSMENT) -> {
                        long dateDiff = 0L;
                        if (Objects.nonNull(callRegister.get(FieldConstants.ASSESSMENT_DATE))) {
                            dateDiff = DateUtil.daysSincePast(Date.from(
                                    Instant.parse(callRegister.get(FieldConstants.ASSESSMENT_DATE).toString())));
                            if (assessmentFollowupRemainingDays < dateDiff) {
                                followUpDetail.setReferredDateSince(dateDiff - assessmentFollowupRemainingDays);
                            }
                            followUpDetail.setDueDate(DateUtil.formatDate(
                                    callRegister.get(FieldConstants.ASSESSMENT_DATE).toString()));
                        }
                    }
                    case (Constants.LOST_TO_FOLLOW_UP) -> {
                        List<String> overDueCategories = new ArrayList<>();
                        Date currentDate = DateUtil.formatDate(DateUtil.getISOStringWithoutMillisecond(
                                Calendar.getInstance()), Constants.DATE_FORMAT_WITHOUT_MILLISECOND);
                        String nextBpAssessmentDate =
                                Objects.isNull(callRegister.get(FieldConstants.NEXT_BP_ASSESSMENT_DATE))
                                        ? null : callRegister.get(FieldConstants.NEXT_BP_ASSESSMENT_DATE).toString();
                        String nextBgAssessmentDate =
                                Objects.isNull(callRegister.get(FieldConstants.NEXT_BG_ASSESSMENT_DATE))
                                        ? null : callRegister.get(FieldConstants.NEXT_BG_ASSESSMENT_DATE).toString();
                        String medicalReviewDate =
                                Objects.isNull(callRegister.get(FieldConstants.MEDICAL_REVIEW_DATE))
                                        ? null : callRegister.get(FieldConstants.MEDICAL_REVIEW_DATE).toString();
                        String dueDate = Objects.isNull(callRegister.get(FieldConstants.DUE_DATE))
                                ? null : callRegister.get(FieldConstants.DUE_DATE).toString();
                        if (Objects.nonNull(currentDate) && ((Objects.nonNull(nextBpAssessmentDate)
                                && (DateUtil.getCalendarDiff(DateUtil.formatDate(nextBpAssessmentDate,
                                        Constants.DATE_FORMAT), currentDate) >= lostToFollowupRemainingDays))
                                || (Objects.nonNull(nextBgAssessmentDate)
                                && (DateUtil.getCalendarDiff(DateUtil.formatDate(nextBgAssessmentDate,
                                        Constants.DATE_FORMAT), currentDate) >= lostToFollowupRemainingDays)))) {
                            overDueCategories.add(Constants.ASSESSMENT);
                        }
                        if (Objects.nonNull(medicalReviewDate) && Objects.nonNull(currentDate)
                                && (DateUtil.getCalendarDiff(DateUtil.formatDate(medicalReviewDate,
                                        Constants.DATE_FORMAT),
                                currentDate))
                                >= lostToFollowupRemainingDays) {
                            overDueCategories.add(Constants.MEDICAL_REVIEW);
                        }
                        followUpDetail.setOverDueCategories(overDueCategories);
                        if (Objects.nonNull(dueDate)) {
                            followUpDetail.setDueDate(DateUtil.formatDate(dueDate, Constants.DATE_FORMAT));
                        }
                        if (Objects.nonNull(followUpDetail.getDueDate()) && Objects.nonNull(currentDate)) {
                            followUpDetail.setReferredDateSince(DateUtil.getCalendarDiff(
                                    DateUtil.formatDate(dueDate, Constants.DATE_FORMAT), currentDate));
                        }
                    }
                    case (Constants.MEDICAL_REVIEW) -> {
                        String nextMedicalReviewDate =
                                Objects.isNull(callRegister.get(FieldConstants.NEXT_MEDICAL_REVIEW_DATE))
                                        ? null : callRegister.get(FieldConstants.NEXT_MEDICAL_REVIEW_DATE).toString();
                        long dateDiff = DateUtil.daysSincePast(DateUtil.formatDate(nextMedicalReviewDate,
                                Constants.MEDICAL_REVIEW_DATE_FORMAT));
                        if (medicalReviewFollowupRemainingDays < dateDiff) {
                            followUpDetail.setReferredDateSince(dateDiff - medicalReviewFollowupRemainingDays);
                        }
                        if (Objects.nonNull(nextMedicalReviewDate)) {
                            followUpDetail.setDueDate(DateUtil.formatDate(nextMedicalReviewDate,
                                    Constants.MEDICAL_REVIEW_DATE_FORMAT));
                        }
                    }
                }
                return followUpDetail;
            }
            return null;
        }).filter(Objects::nonNull).toList();
        return followUpList != null ? followUpList : Collections.emptyList();
    }

    /**
     * Set the followup basic details
     *
     * @param followUpDetail basic followup details
     * @param callRegister call register to get the details
     */
    private void setFollowupBasicDetails(FollowUpDTO followUpDetail, Map<String, Object> callRegister) {
        followUpDetail.setId(Objects.nonNull(callRegister.get(Constants.CALL_REGISTER_ID)) ?
            Long.parseLong(String.valueOf(callRegister.get(Constants.CALL_REGISTER_ID))) :
            Long.parseLong(String.valueOf(callRegister.get(Constants.ID))));
        followUpDetail.setRetryAttempts(Objects.nonNull(callRegister.get(FieldConstants.ATTEMPTS))
                ? followUpCallAttempts - Integer.parseInt(callRegister.get(FieldConstants.ATTEMPTS).toString())
                : followUpCallAttempts);
        followUpDetail.setType(AppointmentType.valueOf(String.valueOf(callRegister.get(FieldConstants.TYPE))));
        if (Objects.nonNull(callRegister.get(FieldConstants.IS_INITIATED))) {
            followUpDetail.setIsInitiated(Boolean.valueOf(callRegister.get(FieldConstants.IS_INITIATED).toString()));
        }
        if (Objects.nonNull(callRegister.get(FieldConstants.IS_COMPLETED))) {
            followUpDetail.setIsCompleted(Boolean.valueOf(callRegister.get(FieldConstants.IS_COMPLETED).toString()));
        }
        followUpDetail.setMemberId(String.valueOf(callRegister.get(FieldConstants.MEMBER_ID)));
        followUpDetail.setReferredSiteId(String.valueOf(callRegister.get(FieldConstants.REFERRED_SITE_ID)));
        followUpDetail.setCreatedAt(DateUtil.convertToTimestamp(callRegister.get(FieldConstants.CREATED_AT).toString()));
        followUpDetail.setUpdatedAt(DateUtil.convertToTimestamp(callRegister.get(FieldConstants.UPDATED_AT).toString()));
        followUpDetail.setDeleted(Boolean.parseBoolean(callRegister.get(FieldConstants.IS_DELETED).toString()));
        followUpDetail.setActive(Boolean.parseBoolean(callRegister.get(FieldConstants.IS_ACTIVE).toString()));
        followUpDetail.setCountyName(Objects.isNull(callRegister.get(FieldConstants.DISTRICT_NAME))
                ? null : callRegister.get(FieldConstants.DISTRICT_NAME).toString());
        followUpDetail.setSubCountyName(Objects.isNull(callRegister.get(FieldConstants.CHIEFDOM_NAME))
                ? null : callRegister.get(FieldConstants.CHIEFDOM_NAME).toString());
        followUpDetail.setCommunityHealthUnitName(
                Objects.isNull(callRegister.get(FieldConstants.HEALTH_FACILITY_NAME))
                        ? null : callRegister.get(FieldConstants.HEALTH_FACILITY_NAME).toString());
        followUpDetail.setMemberId(Objects.isNull(callRegister.get(FieldConstants.MEMBER_ID))
                ? null : callRegister.get(FieldConstants.MEMBER_ID).toString());
        followUpDetail.setPatientId(Objects.isNull(callRegister.get(FieldConstants.PATIENT_ID))
                ? null : callRegister.get(FieldConstants.PATIENT_ID).toString());
        if (Objects.isNull(followUpDetail.getVillageId())) {
            followUpDetail.setVillageId(Objects.isNull(callRegister.get(FieldConstants.VILLAGE_ID))
                    ? null : callRegister.get(FieldConstants.VILLAGE_ID).toString());
        }
        if (Objects.isNull(followUpDetail.getVillageName())) {
            followUpDetail.setVillageName(Objects.isNull(callRegister.get(FieldConstants.VILLAGE_NAME))
                    ? null : callRegister.get(FieldConstants.VILLAGE_NAME).toString());
        }
        followUpDetail.setIsWrongNumber(Objects.isNull(callRegister.get(FieldConstants.IS_WRONG_NUMBER))
                ? null : Boolean.parseBoolean(callRegister.get(FieldConstants.IS_WRONG_NUMBER).toString()));
    }

    /**
     * Create the sort list based on the screening date, attempts and patient id.
     *
     * @param sorts             the sort list to add sort fields
     * @param patientRequestDTO patientRequestDTO to get sort fields conditions.
     * @param type the sort type
     */
    private void addSorting(List<Sort.Order> sorts, PatientRequestDTO patientRequestDTO, String type) {
        switch (type) {
            case (Constants.SCREENED) -> {
                if (!Objects.isNull(patientRequestDTO.getSort())
                        && Boolean.TRUE.equals(patientRequestDTO.getSort().getIsScreeningDueDate())) {
                    sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.SCREENING_DATE_TIME));
                } else if (!Objects.isNull(patientRequestDTO.getSort()) &&
                        Boolean.FALSE.equals(patientRequestDTO.getSort().getIsScreeningDueDate())) {
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.SCREENING_DATE_TIME));
                } else {
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.ATTEMPTS));
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.SCREENING_DATE_TIME));
                }
                sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.PATIENT_ID));
            }
            case (Constants.ASSESSMENT) -> {
                if (!Objects.isNull(patientRequestDTO.getSort())
                        && Boolean.TRUE.equals(patientRequestDTO.getSort().getIsAssessmentDueDate())) {
                    sorts.add(Sort.Order.asc(FieldConstants.ASSESSMENT_DATE));
                } else {
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.ATTEMPTS));
                    sorts.add(Sort.Order.asc(FieldConstants.ASSESSMENT_DATE));
                }
                sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.PATIENT_ID));
            }
            case (Constants.LOST_TO_FOLLOW_UP) -> {
                if (Objects.nonNull(patientRequestDTO.getSort())
                        && Boolean.TRUE.equals(patientRequestDTO.getSort().getIsMedicalReviewDueDate())) {
                    sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.NEXT_MEDICAL_REVIEW_DATE));
                } else if (Objects.nonNull(patientRequestDTO.getSort())
                        && Boolean.TRUE.equals(patientRequestDTO.getSort().getIsAssessmentDueDate())) {
                    sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.ASSESSMENT_DATE));
                } else {
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.ATTEMPTS));
                    sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.NEXT_MEDICAL_REVIEW_DATE));
                }
                sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.PATIENT_ID));
            }
            case (Constants.MEDICAL_REVIEW) -> {
                if(!Objects.isNull(patientRequestDTO.getSort()) &&
                        Boolean.TRUE.equals(patientRequestDTO.getSort().getIsMedicalReviewDueDate())) {
                    sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.NEXT_MEDICAL_REVIEW_DATE));
                } else {
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.ATTEMPTS));
                    sorts.add(new Sort.Order(Sort.Direction.ASC, FieldConstants.NEXT_MEDICAL_REVIEW_DATE));
                }
                sorts.add(new Sort.Order(Sort.Direction.DESC, FieldConstants.PATIENT_ID));
            }
        }
    }

    /**
     * <p>
     * This function Creates and updates lost to followup type patients registers for the patient
     * </p>
     *
     * @param callRegister {@link CallRegister} request that contains the patient related details
     */
    private void addLostToFollowup(CallRegister callRegister) {
        CallRegister lostToFollowupRegister = constructLostToFollowupRegister(callRegister);
        List<CallRegister> lostToFollowupRegisters
                = callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(
                lostToFollowupRegister.getMemberId(), AppointmentType.LOST_TO_FOLLOW_UP);

        if (!lostToFollowupRegisters.isEmpty()) {
            lostToFollowupRegisters.forEach(lostCallRegister ->
            {
                lostCallRegister.setIsCompleted(Boolean.TRUE);
                lostCallRegister.setDeleted(Boolean.TRUE);
                if (Objects.isNull(lostToFollowupRegister.getNextMedicalReviewDate())) {
                    lostToFollowupRegister.setNextMedicalReviewDate(lostCallRegister.getNextMedicalReviewDate());
                }

                if (Objects.isNull(lostToFollowupRegister.getNextBPAssessmentDate())) {
                    lostToFollowupRegister.setNextBPAssessmentDate(lostCallRegister.getNextBPAssessmentDate());
                }

                if (Objects.isNull(lostToFollowupRegister.getNextBGAssessmentTime())) {
                    lostToFollowupRegister.setNextBGAssessmentTime(lostCallRegister.getNextBGAssessmentTime());
                }
            });
            callRegisterRepository.saveAll(lostToFollowupRegisters);
        }
        callRegisterRepository.save(lostToFollowupRegister);
    }

    /**
     * <p>
     * This function maps the call register to create lost to followup
     * </p>
     *
     * @param callRegister {@link CallRegister} request that contains the call register details of the patient
     */
    private CallRegister constructLostToFollowupRegister(CallRegister callRegister) {
        CallRegister lostToFollowupRegister = new CallRegister();
        lostToFollowupRegister.setMemberId(callRegister.getMemberId());
        lostToFollowupRegister.setPatientId(callRegister.getPatientId());
        lostToFollowupRegister.setIsInitiated(callRegister.getIsInitiated());
        lostToFollowupRegister.setType(AppointmentType.LOST_TO_FOLLOW_UP);
        lostToFollowupRegister.setIsWrongNumber(callRegister.getIsWrongNumber());
        lostToFollowupRegister.setVillageId(callRegister.getVillageId());
        lostToFollowupRegister.setNextMedicalReviewDate(callRegister.getNextMedicalReviewDate());
        lostToFollowupRegister.setNextBGAssessmentTime(callRegister.getNextBGAssessmentTime());
        lostToFollowupRegister.setNextBPAssessmentDate(callRegister.getNextBPAssessmentDate());
        lostToFollowupRegister.setIsCompleted(callRegister.getIsCompleted());
        lostToFollowupRegister.setReferredSiteId(callRegister.getReferredSiteId());
        return lostToFollowupRegister;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteNcdCallRegister(FollowUpDTO followUpDTO) {
        if (Objects.nonNull(followUpDTO.getMemberId())) {
            List<CallRegister> callRegisters
                    = callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(
                            followUpDTO.getMemberId(), followUpDTO.getType());
            if (!callRegisters.isEmpty()) {
                callRegisters.forEach(callRegister -> {
                    callRegister.setIsCompleted(Boolean.TRUE);
                    callRegister.setDeleted(Boolean.TRUE);
                });
                callRegisterRepository.saveAll(callRegisters);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void transferCallRegisters(String memberId, String siteReference) {
        if (Objects.nonNull(memberId) && Objects.nonNull(siteReference)) {
            List<CallRegister> callRegisters
                    = callRegisterRepository.findByMemberIdAndIsCompletedAndIsDeletedFalse(memberId, Constants.BOOLEAN_FALSE);
            if (!callRegisters.isEmpty()) {
                callRegisters.forEach(callRegister -> {
                    callRegister.setReferredSiteId(siteReference);
                });
                callRegisterRepository.saveAll(callRegisters);
            }
        }
    }
}
