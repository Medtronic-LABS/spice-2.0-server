package com.mdtlabs.coreplatform.spiceservice.medicalreview.service;

import java.util.*;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.medicalreview.service.impl.MedicalReviewServiceImpl;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;

/**
 * <p>
 * HouseholdServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in HouseholdService class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MedicationServiceTest {

    @InjectMocks
    MedicalReviewServiceImpl medicalReviewService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private FollowUpService followUpService;

    @Mock
    private CallRegisterRepository callRegisterRepository;

    @Mock
    private CustomizedModulesService customizedModulesService;

    @Mock
    private StaticDataService staticDataService;

    @Test
    void createGeneralMedicalReview() {
        //given
        GeneralMedicalReviewDTO generalMedicalReviewDTO = TestDataProvider.getGeneralMedicalReviewDTO();


        //when
        RequestDTO request = new RequestDTO();
        request.setMemberId("122");
        when(fhirServiceApiInterface.getHouseholdMemberById(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getHouseHoldMember());
        when(fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getPregnancyInfo());
        when(fhirServiceApiInterface.createIccmGeneralMedialReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), generalMedicalReviewDTO)).thenReturn(new HashMap<String, String>());

        //then
        Map<String, String> response = medicalReviewService.createGeneralMedicalReview(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void getGeneralMedicalReviewDetails() {
        //given
        GeneralMedicalReviewSummaryDetailsDTO generalMedicalReviewDTO = TestDataProvider.getGeneralMedicalReviewSummaryDetailsDTO();
        RequestDTO requestDTO = new RequestDTO();

        //when
        when(fhirServiceApiInterface.getIccmGeneralMedialReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(generalMedicalReviewDTO);

        //then
        GeneralMedicalReviewSummaryDetailsDTO response = medicalReviewService.getGeneralMedicalReviewDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void saveSummaryDetails() {
        //given
        GeneralMedicalReviewSummaryDTO generalMedicalReviewDTO = TestDataProvider.getGeneralMedicalReviewSummaryDTO();
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        //when
        when(fhirServiceApiInterface.createMedicalReviewSummary(CommonUtil.getAuthToken(), CommonUtil.getClient(), generalMedicalReviewDTO)).thenReturn(generalMedicalReviewDTO);
        when(callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                generalMedicalReviewDTO.getMemberId(), List.of(generalMedicalReviewDTO.getCategory()), List.of(AppointmentType.REFERRED, AppointmentType.MEDICAL_REVIEW), Boolean.FALSE
        )).thenReturn(callRegisters);

        //then
        GeneralMedicalReviewSummaryDTO response = medicalReviewService.saveSummaryDetails(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);
        generalMedicalReviewDTO.setNextVisitDate(null);
        response = medicalReviewService.saveSummaryDetails(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);

        generalMedicalReviewDTO.setCategory(Constants.ICCM);
        response = medicalReviewService.saveSummaryDetails(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);

        generalMedicalReviewDTO.setNextVisitDate(Calendar.getInstance().getTime());
        response = medicalReviewService.saveSummaryDetails(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);

        generalMedicalReviewDTO.setCategory(Constants.RMNCH);
        response = medicalReviewService.saveSummaryDetails(generalMedicalReviewDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createMedicalReview() {
        //given
        MedicalReviewPregnancyDTO requestDTO = TestDataProvider.getMedicalReviewPregnancyDTO();

        //when
        when(fhirServiceApiInterface.createMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, String>());

        //then
        Map<String, String> response = medicalReviewService.createMedicalReview(requestDTO);
        Assertions.assertNotNull(response);

        PregnancyInfo pregnancyInfo = TestDataProvider.getPregnancyInfo();
        pregnancyInfo.setPncVisitNo(Constants.ONE);
        requestDTO.getEncounter().setVisitNumber(Constants.ONE);
        when(fhirServiceApiInterface.getPatientVitals(any(),
                any(), any())).thenReturn(pregnancyInfo);
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        when(callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                any(), any(), any(), any())).thenReturn(callRegisters);

        response = medicalReviewService.createMedicalReview(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createLabourMotherAndNeonate() {
        //given
        MotherNeonateDTO requestDTO = TestDataProvider.getMotherNeonateDTO();
        MotherDTO motherDTO = new MotherDTO();
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setMemberId("Admin123");
        motherDTO.setEncounter(encounterDetailsDTO);
        requestDTO.setMotherDTO(motherDTO);
        //when
        when(fhirServiceApiInterface.createLabourMotherAndNeonateMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, String>());

        //then
        Map<String, String> response = medicalReviewService.createLabourMotherAndNeonate(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void createICCMUnder2months() {
        //given
        UnderFiveIccmDTO requestDTO = TestDataProvider.getUnderFiveIccmDTO();
        requestDTO.setEncounter(TestDataProvider.getEncounterDetailsDTO());

        //when
        RequestDTO request = new RequestDTO();
        request.setMemberId("122");
        when(fhirServiceApiInterface.getHouseholdMemberById(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getHouseHoldMember());
        when(fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getPregnancyInfo());
        when(fhirServiceApiInterface.createICCMUnder2months(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, String>());

        //then
        Map<String, String> response = medicalReviewService.createICCMUnder2months(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void createWeight() {
        //given
        ObservationDTO requestDTO = TestDataProvider.getObservationDTO();

        //when
        when(fhirServiceApiInterface.createWeight(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(requestDTO);

        //then
        ObservationDTO response = medicalReviewService.createWeight(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void getHistory() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        MedicalReviewHistoryDTO medicalReviewHistoryDTO = TestDataProvider.getMedicalReviewHistoryDTO();
        //when
        when(fhirServiceApiInterface.getMedicalReviewHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(medicalReviewHistoryDTO);

        //then
        MedicalReviewHistoryDTO response = medicalReviewService.getHistory(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void getPncHistory() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();
        MedicalReviewHistoryDTO medicalReviewHistoryDTO = TestDataProvider.getMedicalReviewHistoryDTO();

        //when
        when(fhirServiceApiInterface.getPncMedicalReviewHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(medicalReviewHistoryDTO);
        //then
        MedicalReviewHistoryDTO response = medicalReviewService.getPncHistory(requestDTO);
        Assertions.assertNotNull(response);

    }

    @BeforeEach
    void init() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
    }

    @AfterEach
    void cleanup() {
        TestDataProvider.cleanUp();
    }

    @Test
    void getBirthHistory() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        BirthHistoryDTO birthHistoryDTO = new BirthHistoryDTO();

        //when
        when(fhirServiceApiInterface.getBirthHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(birthHistoryDTO);
        //then
        BirthHistoryDTO response = medicalReviewService.getBirthHistory(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void createBp() {
        //given
        ObservationDTO requestDTO = TestDataProvider.getObservationDTO();

        //when
        when(fhirServiceApiInterface.createBp(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(requestDTO);
        //then
        ObservationDTO response = medicalReviewService.createBp(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void getIccmUnder2MSummary() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(fhirServiceApiInterface.getIccmUnder2MSummary(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new IccmResponseDTO());

        //then
        IccmResponseDTO response = medicalReviewService.getIccmUnder2MSummary(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void createICCMUnder5Years() {
        //given
        UnderFiveIccmDTO requestDTO = TestDataProvider.getUnderFiveIccmDTO();
        EncounterDetailsDTO encounterDetailsDTO = new EncounterDetailsDTO();
        encounterDetailsDTO.setMemberId("Admin123");
        requestDTO.setEncounter(encounterDetailsDTO);

        //when
        RequestDTO request = new RequestDTO();
        request.setMemberId("Admin123");
        when(fhirServiceApiInterface.getHouseholdMemberById(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getHouseHoldMember());
        when(fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getPregnancyInfo());
        when(fhirServiceApiInterface.createICCMUnder5Years(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        Map<String, String> response = medicalReviewService.createICCMUnder5Years(requestDTO);
        Assertions.assertNotNull(response);

    }

    @Test
    void savePncMedicalReview() {
        //given
        PncMedicalReviewDTO requestDTO = TestDataProvider.getPncMedicalReviewDTO();
        PncMotherMedicalReviewDTO pncMother = new PncMotherMedicalReviewDTO();
        EncounterDetailsDTO encounterDetails = new EncounterDetailsDTO();
        encounterDetails.setVisitNumber(Constants.ONE);
        encounterDetails.setMemberId("Admin12");
        pncMother.setEncounter(encounterDetails);
        requestDTO.setPncMother(pncMother);
        RequestDTO request = new RequestDTO();
        request.setMemberId("Admin12");
        //when
        when(fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request)).thenReturn(TestDataProvider.getPregnancyInfo());
        when(fhirServiceApiInterface.createPncMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, String>());
        //then
        Map<String, String> response = medicalReviewService.savePncMedicalReview(requestDTO);
        Assertions.assertNotNull(response);

        requestDTO.getPncMother().setIsMotherAlive(Boolean.FALSE);
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        when(callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(
                any(), any(), any(), any())).thenReturn(callRegisters);
        requestDTO.getPncChild().setIsChildAlive(Boolean.FALSE);
        requestDTO.getPncChild().setEncounter(encounterDetails);
        requestDTO.getPncChild().getEncounter().setPatientId("Admin12");
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        householdMemberDTO.setId(Constants.DISTRICT_ID);
        when(fhirServiceApiInterface.getHouseholdMemberByPatientId(any(),
                any(), any())).thenReturn(householdMemberDTO);
        response = medicalReviewService.savePncMedicalReview(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getIccmUnder5YSummary() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(fhirServiceApiInterface.getIccmUnder5YSummary(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new IccmResponseDTO());
        //then
        IccmResponseDTO response = medicalReviewService.getIccmUnder5YSummary(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPregnancyMedicalReviewDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(fhirServiceApiInterface.getPregnancyMedicalReviewDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new MedicalReviewPregnancySummaryDetailsDTO());
        //then
        MedicalReviewPregnancySummaryDetailsDTO response = medicalReviewService.getPregnancyMedicalReviewDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPNCMedicalReviewDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(fhirServiceApiInterface.getPncDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new PncMedicalReviewDTO());
        //then
        PncMedicalReviewDTO response = medicalReviewService.getPNCMedicalReviewDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientWeight() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(fhirServiceApiInterface.getWeight(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, Double>());
        //then
        Map<String, Double> response = medicalReviewService.getPatientWeight(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientBp() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(fhirServiceApiInterface.getBp(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new HashMap<String, Double>());
        //then
        Map<String, Double> response = medicalReviewService.getPatientBp(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getLabourMotherAndNeonateDetails() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        //when
        when(fhirServiceApiInterface.getLabourMotherAndNeonateDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new MotherNeonateDTO());

        //then
        MotherNeonateDTO response = medicalReviewService.getLabourMotherAndNeonateDetails(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateConfirmDiagnosis() {
        //given
        ConfirmDiagnosisDTO confirmDiagnosisDTO = TestDataProvider.getConfirmDiagnosisDTO();

        //when
        doNothing().when(fhirServiceApiInterface).updateConfirmDiagnosis(TestConstants.BEARER_TEST, com.mdtlabs.coreplatform.commonservice.common.Constants.CLIENT_SPICE_MOBILE, confirmDiagnosisDTO);

        //then
        medicalReviewService.updateConfirmDiagnosis(confirmDiagnosisDTO);
        verify(fhirServiceApiInterface, atLeastOnce()).updateConfirmDiagnosis(CommonUtil.getAuthToken(), CommonUtil.getClient(), confirmDiagnosisDTO);
    }

    @Test
    void testMedicalReviewCountTest() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();
        Map<String, Integer> response = Map.of(TestConstants.PRESCRIPTION_DAYS_COMPLETED_COUNT, TestConstants.INT_ONE);

        //when
        when(fhirServiceApiInterface.getMedicalReviewCount(TestConstants.BEARER_TEST, com.mdtlabs.coreplatform.commonservice.common.Constants.CLIENT_SPICE_MOBILE, request)).thenReturn(response);

        //then
        Map<String, Integer> actualResponse = medicalReviewService.getMedicalReviewCount(request);
        assertNotNull(actualResponse);
        assertEquals(TestConstants.INT_ONE, actualResponse.get(TestConstants.PRESCRIPTION_DAYS_COMPLETED_COUNT));

    }

    @Test
    void getNCDMedicalReviewHistory() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(fhirServiceApiInterface.getNCDMedicalReviewHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new NCDMedicalReviewHistoryDTO());

        //then
        NCDMedicalReviewHistoryDTO response = medicalReviewService.getNCDMedicalReviewHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getNCDMedicalReviewSummaryHistory() {
        //given
        MedicalReviewRequestDTO requestDTO = TestDataProvider.getMedicalReviewRequestDTO();

        //when
        when(fhirServiceApiInterface.getNCDMedicalReviewSummaryHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(new NCDMedicalReviewHistoryDTO());

        //then
        NCDMedicalReviewHistoryDTO response = medicalReviewService.getNCDMedicalReviewSummaryHistory(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void testUpdateViewStatus() {
        //given
        RequestDTO request = TestDataProvider.getRequestDTO();

        //when
        when(fhirServiceApiInterface.updateViewCount(TestConstants.BEARER_TEST,
                com.mdtlabs.coreplatform.commonservice.common.Constants.CLIENT_SPICE_MOBILE, request)).thenReturn(
                Boolean.TRUE);

        //then
        Boolean actualResponse = medicalReviewService.updateViewCount(request);
        assertNotNull(actualResponse);
        assertEquals(Boolean.TRUE, actualResponse);
    }

    @Test
    void createPncChild() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        doNothing().when(fhirServiceApiInterface).createPncChild("Bearer", "mob", householdMemberDTO);
        medicalReviewService.createPncChild(householdMemberDTO);
        assertNotNull(householdMemberDTO);
        assertEquals("Name", householdMemberDTO.getName()); // Example field validation
    }

    @Test
    void createPatientStatus() {
        PatientStatusDTO patientStatusDto = new PatientStatusDTO();
        patientStatusDto.setPatientReference(TestConstants.STRING_ONE);

        //when
        when(fhirServiceApiInterface.createPatientStatus(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                patientStatusDto)).thenReturn(patientStatusDto);
        doNothing().when(customizedModulesService).updateCustomizedModules(patientStatusDto.getMemberReference(), patientStatusDto.getPatientReference());
        medicalReviewService.createPatientStatus(patientStatusDto);
        assertNotNull(patientStatusDto, "PatientStatusDTO should not be null");
        assertEquals(TestConstants.STRING_ONE, patientStatusDto.getPatientReference(), "Patient reference should");
    }

    @Test
    void getPatientStatusDetails() {
        PatientStatusDTO patientStatusDTO = new PatientStatusDTO();

        //when
        when(fhirServiceApiInterface.getPatientStatusDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                patientStatusDTO)).thenReturn(patientStatusDTO);
        PatientStatusDTO response = medicalReviewService.getPatientStatusDetails(patientStatusDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createNcdMedicalReview() {
        NCDMedicalReviewDTO request = new NCDMedicalReviewDTO();

        //when
        when(fhirServiceApiInterface.createNcdMedicalReview(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(Map.of());
        Map<String, String> response = medicalReviewService.createNcdMedicalReview(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void createSummary() {
        NCDMedicalReviewDTO request = new NCDMedicalReviewDTO();
        request.setNextMedicalReviewDate(new Date());
        request.setPatientReference(TestConstants.STRING_ONE);
        request.setProvenance(TestDataProvider.getProvenance());

        //when
        doNothing().when(fhirServiceApiInterface).createSummary("Bearer", "mob", request);
        medicalReviewService.createSummary(request);
        assertNotNull(request.getNextMedicalReviewDate(), "Next medical review date should not be null");
        assertNotNull(request.getPatientReference(), "Patient reference should not be null");
        assertEquals(TestConstants.STRING_ONE, request.getPatientReference(), "Patient reference should match the expected value");
        assertNotNull(request.getProvenance(), "Provenance should not be null");
        verify(fhirServiceApiInterface, times(1)).createSummary(any(),any(),any());
    }

    @Test
    void ncdMedicalReviewSummary() {
        MedicalReviewRequestDTO request = TestDataProvider.getMedicalReviewRequestDTO();
        NcdMedicalReviewResponse response = new NcdMedicalReviewResponse();
        response.setClinicalNote("cNote");
        //when
        when(fhirServiceApiInterface.getNcdMedicalReviewDetails(any(),any(),any())).thenReturn(response);
        NcdMedicalReviewResponse result = medicalReviewService.ncdMedicalReviewSummary(request);
        System.out.println(result);
        verify(fhirServiceApiInterface, times(1)).getNcdMedicalReviewDetails(any(),any(),any());
        assertNotNull(result, "The result should not be null");
    }

    @Test
    void getMedicalReviewCount() {
        RequestDTO request = new RequestDTO();

        when(fhirServiceApiInterface.getMedicalReviewCount("Bearer", "mob", request)).thenReturn(Map.of());
        Map<String, Integer> response = medicalReviewService.getMedicalReviewCount(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateNCDAppointment() {
        NCDMedicalReviewDTO request = new NCDMedicalReviewDTO();

        doNothing().when(fhirServiceApiInterface).updateNCDAppointment("Bearer", "mob", request);
        medicalReviewService.updateNCDAppointment(request);
        verify(fhirServiceApiInterface, times(0)).updateNCDAppointment("Bearer", "mob", request);
    }

    @Test
    void getInstruction() {
        List<String> response = medicalReviewService.getInstruction();
        Assertions.assertNotNull(response);
    }

    @Test
    void getPatientLifestyleDetails() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        request.setPatientReference(TestConstants.STRING_ONE);
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setValue(TestConstants.STRING_ONE);

        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getPatientLifestyleDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), request)).thenReturn(new ArrayList<>());
        List<LifestyleResponseDTO> response = medicalReviewService.getPatientLifestyleDetails(request);
        Assertions.assertNotNull(response);
    }

}
