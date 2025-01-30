package com.mdtlabs.coreplatform.spiceservice.report.service.impl;

import java.util.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageDTO;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDto;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;

@ExtendWith(MockitoExtension.class)
class PerformanceMonitoringServiceImplTest {

    @Mock
    UserServiceApiInterface userServiceApiInterface;

    @Mock
    FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    CallRegisterRepository callRegisterRepository;

    @InjectMocks
    PerformanceMonitoringServiceImpl performanceMonitoringService;

    @Test
    void testGetChwPerformanceMonitoringReport_EmptyUserIds() {
        // given
        FilterRequestDTO requestDTO = new FilterRequestDTO();
        requestDTO.setUserIds(new HashSet<>());

        // when
        List<PerformanceReport> result = performanceMonitoringService.getChwPerformanceMonitoringReport(requestDTO);

        // then
        Assertions.assertTrue(result.isEmpty(), "Result should be empty when userIds are empty");
    }

    @Test
    void testGetChwPerformanceMonitoringReport_WithUsers() {
        // given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        FilterRequestDTO requestDTO = new FilterRequestDTO();
        requestDTO.setVillageIds(Set.of("1", "2"));
        requestDTO.setUserIds(Set.of(1L, 2L));

        List<UserVillageDTO> users = Arrays.asList(new UserVillageDTO(1L, "village1", "", 123L, "John", "Doe"));

        // when
        when(userServiceApiInterface.getUsersByPeerSupervisorId(any(), any(), any(), any())).thenReturn(users);
        when(fhirServiceApiInterface.getPerformanceMonitoringReport(any(), any(), any())).thenReturn(new HashMap<>());

        List<PerformanceReport> result = performanceMonitoringService.getChwPerformanceMonitoringReport(requestDTO);

        // then
        Assertions.assertFalse(result.isEmpty(), "Result should not be empty when there are valid users");
        verify(userServiceApiInterface, times(1)).getUsersByPeerSupervisorId(any(), any(), any(), any());
        verify(fhirServiceApiInterface, times(1)).getPerformanceMonitoringReport(any(), any(), any());
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetFollowUpDetails() {
        // given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        FilterRequestDTO requestDTO = new FilterRequestDTO();
        requestDTO.setUserFhirIds(new HashMap<>());
        requestDTO.setUserFhirIds(Map.of("1", "1"));
        requestDTO.setVillageIds(new HashSet<>(Arrays.asList("village1")));

        Map<String, Map<String, PerformanceReport>> reports = new HashMap<>();
        reports.put("village1", new HashMap<>());
        com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO searchRequestDTO = new com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO();
        searchRequestDTO.setUserId(1L);


        List<Map<String, Object>> callRegisters = new ArrayList<>();
        callRegisters.add(Map.of("patientId", "123", "createdBy", "1", "updatedBy", "1", "chwId", "1"));
        HashSet<String> set = new HashSet<>();
        set.add(null);

        when(callRegisterRepository.findByVillageIds(any(), any(), any(), any())).thenReturn(callRegisters);

        // when
        when(userServiceApiInterface.getUsersByPeerSupervisorId(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), CommonUtil.getClient(), searchRequestDTO)).thenReturn(List.of(new UserVillageDTO(), new UserVillageDTO()));
        when(callRegisterRepository.findByVillageIds(any(), any(), any(), any())).thenReturn(new ArrayList<>(Arrays.asList(Map.of("createdBy", "1"), Map.of("patientId", "456"))));
        when(callRegisterRepository.findCallRegisterByVillageIdsWithDetails(any(), any(), any(), any()))
                .thenReturn(new ArrayList<>(Arrays.asList(Map.of("createdBy", "1"), Map.of("patientId", "456"))));
        when(fhirServiceApiInterface.getPerformanceMonitoringReport(
                CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(reports);
        performanceMonitoringService.getChwPerformanceMonitoringReport(requestDTO);

        // then
        verify(callRegisterRepository, times(1)).findByVillageIds(any(), any(), any(), any());
        TestDataProvider.cleanUp();
    }

    @Test
    void testUpdateVisitCount() {
        // given
        CallRegisterDto callRegisterDto = new CallRegisterDto();
        callRegisterDto.setAttempts(0);
        callRegisterDto.setType("HH_VISIT");
        callRegisterDto.setVillageId("village1");
        callRegisterDto.setVisits(0);

        Map<String, Map<String, PerformanceReport>> reports = new HashMap<>();
        reports.put("village1", new HashMap<>());
        reports.get("village1").put("fhir1", new PerformanceReport());

        Set<String> visitTypes = new HashSet<>();
        Set<String> callTypes = new HashSet<>();

        // when
        performanceMonitoringService.updateVisitCount(callRegisterDto, reports, "fhir1", visitTypes, callTypes);

        // then
        Assertions.assertTrue(visitTypes.contains("HH_VISIT"), "Visit type should be added to visitTypes");
    }

    @Test
    void testAddFollowUpCount() {
        // given
        PerformanceReport performanceReport = new PerformanceReport();
        Set<String> types = new HashSet<>();

        // when
        performanceMonitoringService.addFollowUpCount(Constants.FOLLOW_UP_COND_CALL, performanceReport, types, false, "HH_VISIT");

        // then
        Assertions.assertEquals(1, performanceReport.getFollowUpCondCalls(), "Follow-up due visits should be updated");
    }

    @Test
    void testGetUserId_ReturnsCreatedBy() {
        // given
        FilterRequestDTO requestDTO = new FilterRequestDTO();
        requestDTO.setUserFhirIds(Map.of("1", "fhir1"));

        CallRegisterDto callRegisterDto = new CallRegisterDto();
        callRegisterDto.setCreatedBy(1L);
        callRegisterDto.setUpdatedBy(2L);

        // when
        Long result = performanceMonitoringService.getUserId(requestDTO, callRegisterDto, true);

        // then
        Assertions.assertEquals(1L, result, "getUserId should return createdBy when returnCreatedBy is true");
    }

    @Test
    void testGetUserId_ReturnsUpdatedBy() {
        // given
        FilterRequestDTO requestDTO = new FilterRequestDTO();
        requestDTO.setUserFhirIds(Map.of("2", "2"));

        CallRegisterDto callRegisterDto = new CallRegisterDto();
        callRegisterDto.setCreatedBy(1L);
        callRegisterDto.setUpdatedBy(2L);

        // when
        Long result = performanceMonitoringService.getUserId(requestDTO, callRegisterDto, false);

        // then
        Assertions.assertEquals(2L, result, "getUserId should return updatedBy when returnCreatedBy is false");
    }
}
