package com.mdtlabs.coreplatform.spiceservice.report.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDto;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.report.service.PerformanceMonitoringService;

/**
 * <p>
 * This service class contain all the business logic for performance monitoring module and perform
 * all the performance monitoring operation here.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on July 30, 2024
 */
@Service
public class PerformanceMonitoringServiceImpl implements PerformanceMonitoringService {

    private final UserServiceApiInterface userServiceApiInterface;

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final CallRegisterRepository callRegisterRepository;

    public PerformanceMonitoringServiceImpl(UserServiceApiInterface userServiceApiInterface, FhirServiceApiInterface fhirServiceApiInterface, CallRegisterRepository callRegisterRepository) {
        this.userServiceApiInterface = userServiceApiInterface;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.callRegisterRepository = callRegisterRepository;
    }

    /**
     * Create search request Dto for Get users
     *
     * @param requestDTO Request Object
     * @return SearchRequestDTO
     */
    private SearchRequestDTO getSearchRequestDTO(FilterRequestDTO requestDTO) {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        searchRequestDTO.setSkip(requestDTO.getSkip());
        searchRequestDTO.setLimit(requestDTO.getLimit());
        if (Objects.isNull(requestDTO.getUserIds())) {
            searchRequestDTO.setUserId(UserContextHolder.getUserDto().getId());
        } else {
            searchRequestDTO.setUserIds(requestDTO.getUserIds());
            searchRequestDTO.setVillageIds(
                    requestDTO.getVillageIds().stream().map(Long::valueOf).collect(Collectors.toSet()));
        }
        return searchRequestDTO;
    }

    @Override
    public List<PerformanceReport> getChwPerformanceMonitoringReport(FilterRequestDTO requestDTO) {
        List<PerformanceReport> performanceReports = new ArrayList<>();
        if (!Objects.isNull(requestDTO.getUserIds()) && requestDTO.getUserIds().isEmpty()) {
            return performanceReports;
        }
        SearchRequestDTO searchRequestDTO = getSearchRequestDTO(requestDTO);
        List<UserVillageDTO> users = userServiceApiInterface.getUsersByPeerSupervisorId(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), CommonUtil.getClient(), searchRequestDTO);
        if (!Objects.isNull(users) && !users.isEmpty()) {
            requestDTO.setFhirIds(new HashSet<>());
            requestDTO.setVillageIds(new HashSet<>());
            requestDTO.setUserFhirIds(new HashMap<>());
            users.forEach(user -> {
                requestDTO.getFhirIds().add(user.getFhirId());
                requestDTO.getVillageIds().add(String.valueOf(user.getVillageId()));
                requestDTO.getUserFhirIds().put(String.valueOf(user.getId()), user.getFhirId());
            });
            Map<String, Map<String, PerformanceReport>> reports = fhirServiceApiInterface.getPerformanceMonitoringReport(
                    CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
            getFollowUpDetails(requestDTO, reports);
            users.forEach(value -> {
                PerformanceReport performanceReport = new PerformanceReport();
                if (!Objects.isNull(reports) && !Objects.isNull(reports.get(String.valueOf(value.getVillageId()))) &&
                        !Objects.isNull(reports.get(String.valueOf(value.getVillageId())).get(value.getFhirId()))) {
                    Map<String, PerformanceReport> map = reports.get(String.valueOf(value.getVillageId()));
                    performanceReport = map.get(value.getFhirId());
                }
                performanceReport.setUserId(value.getFhirId());
                performanceReport.setVillageId(String.valueOf(value.getVillageId()));
                performanceReport.setChwName(
                        StringUtil.concatString(value.getFirstName(), Constants.EMPTY_SPACE, value.getLastName()));
                performanceReport.setVillageName(value.getVillageName());
                performanceReports.add(performanceReport);
            });
        }
        return performanceReports;
    }

    /**
     * Get follow up details based on the call register details
     *
     * @param requestDTO Request Details
     * @param reports    Report Details
     */
    private void getFollowUpDetails(FilterRequestDTO requestDTO, Map<String, Map<String, PerformanceReport>> reports) {
        ModelMapper modelMapper = new ModelMapper();
        List<String> appointmentTypes = List.of(AppointmentType.REFERRED.name(), AppointmentType.MEDICAL_REVIEW.name(), AppointmentType.HH_VISIT.name());
        List<Map<String, Object>> callRegisters = callRegisterRepository.findByVillageIds(requestDTO.getVillageIds(),
                requestDTO.getFromDate(), requestDTO.getToDate(), appointmentTypes);
        List<Map<String, Object>> callRegistersWithDetails = callRegisterRepository.findCallRegisterByVillageIdsWithDetails(
                requestDTO.getVillageIds(), requestDTO.getFromDate(), requestDTO.getToDate(), appointmentTypes);
        List<CallRegisterDto> callRegisterDtoList = callRegisters.stream()
                .map(user -> modelMapper.map(user, CallRegisterDto.class)).toList();
        Map<String, List<CallRegisterDto>> dueVisits = new HashMap<>();
        callRegisterDtoList.forEach(callRegisterDto -> {
            dueVisits.computeIfAbsent(callRegisterDto.getPatientId(), k -> new ArrayList<>());
            dueVisits.get(callRegisterDto.getPatientId()).add(callRegisterDto);
        });

        dueVisits.keySet().forEach(key -> {
            Set<String> visitTypes = new HashSet<>();
            Set<String> callTypes = new HashSet<>();
            dueVisits.get(key).forEach(value -> {
                Long userId = getUserId(requestDTO, value, Boolean.FALSE);
                if (!Objects.isNull(userId)) {
                    String fhirId = requestDTO.getUserFhirIds().get(String.valueOf(userId));
                    reports.computeIfAbsent(value.getVillageId(), k -> new HashMap<>());
                    reports.get(value.getVillageId()).computeIfAbsent(fhirId, k -> new PerformanceReport());
                    updateVisitCount(value, reports, fhirId, visitTypes, callTypes);
                }
            });
        });

        List<CallRegisterDto> callRegisterDtoListWithDetails = callRegistersWithDetails.stream()
                .map(user -> modelMapper.map(user, CallRegisterDto.class)).toList();

        callRegisterDtoListWithDetails.forEach(value -> {
            Long userId = getUserId(requestDTO, value, Boolean.TRUE);
            if (!Objects.isNull(userId)) {
                String fhirId = requestDTO.getUserFhirIds().get(String.valueOf(userId));
                reports.computeIfAbsent(value.getVillageId(), k -> new HashMap<>());
                reports.get(value.getVillageId()).computeIfAbsent(fhirId, k -> new PerformanceReport());
                addFollowUpCount(Constants.FOLLOW_UP_COND_CALL, reports.get(value.getVillageId()).get(fhirId), null,
                        Boolean.TRUE, value.getType());
            }
        });
    }

    /**
     * Update visit count based on the call register details
     *
     * @param value      CallRegisterDto
     * @param reports    Map<String, Map<String, PerformanceReport>>
     * @param userId     String
     * @param visitTypes Set<String>
     */
    protected void updateVisitCount(CallRegisterDto value, Map<String, Map<String, PerformanceReport>> reports,
                                    String userId, Set<String> visitTypes, Set<String> callTypes) {
        if (value.getAttempts() == Constants.ZERO) {
            addFollowUpCount(Constants.FOLLOW_UP_DUE_CALL, reports.get(value.getVillageId()).get(userId), callTypes,
                    Boolean.FALSE, value.getType());
        }
        if (String.valueOf(AppointmentType.HH_VISIT).equals(value.getType()) &&
                value.getVisits() == Constants.ZERO) {
            addFollowUpCount(Constants.FOLLOW_UP_DUE_VISIT, reports.get(value.getVillageId()).get(userId), visitTypes,
                    Boolean.FALSE, value.getType());
        }
    }

    /**
     * Add follow up count based on the visit type
     *
     * @param value             String
     * @param performanceReport Performance Report
     * @param types             visitTypes
     */
    protected void addFollowUpCount(String value, PerformanceReport performanceReport, Set<String> types,
                                    boolean allowToadd, String type) {
        if ((!Objects.isNull(types) && types.add(type)) || allowToadd) {
            if (value.equals(Constants.FOLLOW_UP_DUE_VISIT)) {
                performanceReport.setFollowUpDueVisit();
            } else if (value.equals(Constants.FOLLOW_UP_DUE_CALL)) {
                performanceReport.setFollowUpDueCalls();
            } else if (value.equals(Constants.FOLLOW_UP_COND_CALL)) {
                performanceReport.setFollowUpCondCalls();
            }
        }
    }

    /**
     * Get user id based on the call register details
     *
     * @param requestDTO      Request Details
     * @param callRegisterDto CallRegister Details
     * @return userId
     */
    protected Long getUserId(FilterRequestDTO requestDTO, CallRegisterDto callRegisterDto, boolean returnCreatedBy) {
        String createdByStr = String.valueOf(callRegisterDto.getCreatedBy());
        String updatedByStr = String.valueOf(callRegisterDto.getUpdatedBy());
        Long result = null;
        if (requestDTO.getUserFhirIds().containsValue(createdByStr) || returnCreatedBy) {
            result = callRegisterDto.getCreatedBy();
        } else if (requestDTO.getUserFhirIds().containsValue(updatedByStr)) {
            result = callRegisterDto.getUpdatedBy();
        } else if (!Objects.isNull(callRegisterDto.getChwId())) {
            result = Long.valueOf(callRegisterDto.getChwId());
        }
        return result;
    }
}
