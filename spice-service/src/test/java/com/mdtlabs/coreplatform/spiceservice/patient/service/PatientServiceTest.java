package com.mdtlabs.coreplatform.spiceservice.patient.service;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.WgsData;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.patient.repository.WgsDataRepository;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.repository.PatientTransferRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider.getPatientDetailsDTO;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.patient.service.impl.PatientServiceImpl;

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
class PatientServiceTest {

    @InjectMocks
    PatientServiceImpl patientService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private AdminServiceApiInterface adminServiceApiInterface;

    @Mock
    private FollowUpService followUpService;

    @Mock
    private CallRegisterRepository callRegisterRepository;

    @Mock
    private WgsDataRepository wgsDataRepository;

    @Mock
    private PatientTransferRepository patientTransferRepository;

    @Test
    void searchPatient() {
        //given
        TestDataProvider.init();
        PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
        patientRequestDTO.setDistrictId(TestConstants.ONE);

        //when
        TestDataProvider.getStaticMock();
        when(adminServiceApiInterface.getVillageByDistrict(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                Map.of(Constants.DISTRICT_ID, patientRequestDTO.getDistrictId()))).thenReturn(List.of(TestDataProvider.getVillage()));
        when(fhirServiceApiInterface.searchPatient(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO)).thenReturn(new HashMap<String, Object>());

        //then
        Map<String, Object> response = patientService.searchPatient(patientRequestDTO);
        Assertions.assertNotNull(response);
        patientRequestDTO.setDistrictId(null);
        Assertions.assertThrows(SpiceValidation.class, () -> patientService.searchPatient(patientRequestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientDetails() {
        //given
        PatientDTO patientDTO = new PatientDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.getPatientDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientDTO)).thenReturn(new PatientDetailsDTO());

        //then
        PatientDetailsDTO response = patientService.getPatientDetails(patientDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientList() {
        //given
        TestDataProvider.init();
        PatientRequestDTO patientRequestDTO = new PatientRequestDTO();
        TestDataProvider.getStaticMock();

        //when
        when(adminServiceApiInterface.getFacilityVillagesByTenantId(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), TestConstants.ONE)).thenReturn(List.of(TestConstants.ONE));
        when(fhirServiceApiInterface.getPatientDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), new PatientDTO())).thenReturn(new PatientDetailsDTO());

        //then
        Map<String, Object> response = patientService.getPatientList(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientStatus() {
        //given
        TestDataProvider.init();
        RequestDTO patientRequestDTO = new RequestDTO();
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.getPatientStatus(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO)).thenReturn(new HashMap<String, Object>());

        //then
        Map<String, Object> response = patientService.getPatientStatus(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateStatusOfServiceRequest() {
        //given
        RequestDTO patientRequestDTO = new RequestDTO();
        TestDataProvider.init();
        TestDataProvider.getStaticMock();

        //then
        Boolean response = patientService.updateStatusOfServiceRequest(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getReferralTickets() {
        //given
        TestDataProvider.init();
        RequestDTO patientRequestDTO = new RequestDTO();
        TestDataProvider.getStaticMock();

        //then
        List<ReferralTicketDTO> response = patientService.getReferralTickets(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createReferralTicket() {
        //given
        TestDataProvider.init();
        ReferralDetailsDTO referralDetailsDTO = TestDataProvider.getReferralDetailsDTO();
        referralDetailsDTO.setReferred(Boolean.TRUE);
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.createReferralTicket(CommonUtil.getAuthToken(), CommonUtil.getClient(), referralDetailsDTO)).thenReturn(referralDetailsDTO);
        //then
        ReferralDetailsDTO response = patientService.createReferralTicket(referralDetailsDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createPatientByPatientId() {
        //given
        TestDataProvider.init();
        RequestDTO patientRequestDTO = new RequestDTO();
        TestDataProvider.getStaticMock();

        //when
        when(fhirServiceApiInterface.createPatientByPatientId(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientRequestDTO)).thenReturn(TestConstants.STRING_ONE);

        //then
        String response = patientService.createPatientByPatientId(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createDiagnosis() {
        //given
        TestDataProvider.init();
        DiagnosisDTO patientRequestDTO = new DiagnosisDTO();
        TestDataProvider.getStaticMock();

        //then
        patientService.createDiagnosis(patientRequestDTO);
        verify(fhirServiceApiInterface, atLeastOnce()).updatePatientDiagnosis(any(), any(), any());
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientDiagnosis() {
        //given
        TestDataProvider.init();
        RequestDTO patientRequestDTO = new RequestDTO();
        TestDataProvider.getStaticMock();

        //then
        List<DiagnosisDTO.DiseaseDTO> response = patientService.getPatientDiagnosis(patientRequestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientDiagnosisDetails() {
        //given
        TestDataProvider.init();
        RequestDTO patientRequestDTO = new RequestDTO();
        TestDataProvider.getStaticMock();

        //then
        patientService.getPatientDiagnosisDetails(patientRequestDTO);
        verify(fhirServiceApiInterface, atLeastOnce()).getPatientDiagnosisDetails(any(), any(), any());
        TestDataProvider.cleanUp();
    }

   @Test
    void getPregnancyInfoByVillages() {
        //given
       TestDataProvider.init();
        RequestDTO patientRequestDTO = new RequestDTO();
        patientRequestDTO.setSkip(0);
        patientRequestDTO.setLimit(10);
        PregnancyInfo pregnancyInfo = new PregnancyInfo();
        pregnancyInfo.setPncVisitNo(1);
        List<PregnancyInfo> pregnancyInfos = List.of(pregnancyInfo);

        //when
       TestDataProvider.getStaticMock();
       // when(fhirServiceApiInterface.getPregnancyInfoByVillages("BearerTest", "mob", patientRequestDTO)).thenReturn(pregnancyInfos);

        //then
        List<PregnancyInfo> response = patientService.getPregnancyInfoByVillages(patientRequestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void testNullGenderThrowsException() {
        WgsDataDTO dto = new WgsDataDTO();
        dto.setWeight(50.0);
        dto.setHeight(100.0);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientService.getPatientWgsData(dto));
    }

    @Test
    void testNullWeightThrowsException() {
        WgsDataDTO dto = new WgsDataDTO();
        dto.setGender(Constants.MALE);
        dto.setHeight(100.0);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientService.getPatientWgsData(dto));
    }

    @Test
    void testNullHeightAndAgeThrowsException() {
        WgsDataDTO dto = new WgsDataDTO();
        dto.setGender(Constants.MALE);
        dto.setWeight(50.0);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientService.getPatientWgsData(dto));
    }

    @Test
    void testInvalidAgeThrowsException() {
        WgsDataDTO dto = new WgsDataDTO();
        dto.setGender(Constants.MALE);
        dto.setWeight(50.0);
        dto.setAgeInMonths(-1.0); // Invalid age
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientService.getPatientWgsData(dto));
    }

    @Test
    void testInvalidHeightThrowsException() {
        WgsDataDTO dto = new WgsDataDTO();
        dto.setGender(Constants.MALE);
        dto.setWeight(50.0);
        dto.setHeight(130.0); // Invalid height
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientService.getPatientWgsData(dto));
    }

    @Test
    void testNoMatchingWGSDataThrowsException() {
        WgsDataDTO dto = new WgsDataDTO();
        dto.setGender(Constants.MALE);
        dto.setWeight(50.0);
        dto.setHeight(100.0);
        when(wgsDataRepository.findBySexAndGivenAndIndicator(anyInt(), anyDouble(), anyString())).thenReturn(Collections.emptyList());
        Assertions.assertThrows(DataNotFoundException.class, () -> patientService.getPatientWgsData(dto));
    }

    @ParameterizedTest
    @CsvSource({"9.6479, 0.0644, 0.10925, Constants.WFA, Constants.MALE, 55.0, 100.0, 12.0, 33.66",
            "8.9481, -0.2024, 0.12268, Constants.WFA, Constants.FEMALE, 55.0, 100.0, 12.0, 29.04",
            "15.3576, -0.3521, 0.08229, Constants.WFH, Constants.MALE, 55.0, 100.0, 12.0, 23.8",
            "15.2246, -0.3833, 0.09088, Constants.WFH, Constants.MALE, 55.0, 100.0, 12.0, 21.03"})
    void testValidInputsReturnsResponseMap(double mValue, double lValue, double sValue, String indicator, String gender,
                                           double weight, double height, double ageInMonths, double expectedResult) {
        // Setup valid DTO and mock repository response...
        // Assume we have a valid list of WGS data.
        WgsData validWGSdata =new WgsData();
        validWGSdata.setM(mValue);
        validWGSdata.setL(lValue);
        validWGSdata.setS(sValue);
        validWGSdata.setIndicator(indicator);

        when(wgsDataRepository.findBySexAndGivenAndIndicator(anyInt(), anyDouble(), anyString())).thenReturn(Collections.singletonList(validWGSdata));

        // Create a valid DTO.
        WgsDataDTO wgsDataDTO = new WgsDataDTO();
        wgsDataDTO.setGender(gender);
        wgsDataDTO.setWeight(weight);
        wgsDataDTO.setHeight(height);
        wgsDataDTO.setAgeInMonths(ageInMonths);

        Map<String,Object> response = patientService.getPatientWgsData(wgsDataDTO);

        Assertions.assertEquals(1, response.size());
        Assertions.assertTrue(response.containsKey(indicator));
        Assertions.assertEquals(expectedResult, response.get(indicator));
    }
    @Test
    void testSearchPatients() {
        TestDataProvider.init();
        PatientRequestDTO requestDTO = new PatientRequestDTO();
        requestDTO.setType(Constants.ASSESSMENT);
        requestDTO.setTenantId(TestConstants.ONE);
        Map<String, Object> expected = Map.of("key", "value");
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.searchPatients("Bearer",
                "mob", requestDTO)).thenReturn(expected);
        Map<String, Object> result = patientService.searchPatients(requestDTO);
        Assertions.assertNotNull(result);
        TestDataProvider.cleanUp();
    }

    @Test
    void testSearchPatientDetails() {
        TestDataProvider.init();
        PatientDTO patientDTO = new PatientDTO();
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.searchPatientDetails("Bearer",
                "mob", patientDTO)).thenReturn(new PatientDetailsDTO());

        PatientDetailsDTO result = patientService.searchPatientDetails(patientDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void testCreatePregnancyDetails() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        pregnancyDetailsDTO.setMemberReference("123");
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.createPregnancyDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), pregnancyDetailsDTO)).thenReturn(pregnancyDetailsDTO);
        PregnancyDetailsDTO pregnancyDetails = patientService.createPregnancyDetails(pregnancyDetailsDTO);
        Assertions.assertEquals(pregnancyDetailsDTO.getMemberReference(), pregnancyDetails.getMemberReference());
        TestDataProvider.cleanUp();
    }

    @Test
    void testGetPregnancyDetails() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId("123");
        requestDTO.setMemberReference("123");
        requestDTO.setId("123");
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getPregnancyDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO)).thenReturn(new PregnancyDetailsDTO());
        PregnancyDetailsDTO pregnancyDetails = patientService.getPregnancyDetails(requestDTO);
        Assertions.assertNotNull(pregnancyDetails);
        TestDataProvider.cleanUp();
    }

    @Test
    void updatePatient() {
        //given
        TestDataProvider.init();
        EnrollmentRequestDTO enrollmentRequestDTO = new EnrollmentRequestDTO();

        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.updatePatient("BearerTest", "mob", enrollmentRequestDTO)).thenReturn(new EnrollmentResponseDTO());
        //then
        EnrollmentResponseDTO enrollmentResponseDto = patientService.updatePatient(enrollmentRequestDTO);
        Assertions.assertNotNull(enrollmentResponseDto);
        TestDataProvider.cleanUp();
    }

    @Test
    void deletePatientByPatientId() {
        //given
        TestDataProvider.init();
        RequestDTO requestData = new RequestDTO();
        requestData.setPatientId(TestConstants.PATIENT_ID);
        requestData.setMemberId(TestConstants.MEMBER_ID);
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        List<PatientTransfer> patientTransfers = List.of(TestDataProvider.getPatientTransfer());
        //when
        TestDataProvider.getStaticMock();
        doNothing().when(fhirServiceApiInterface).deletePatientByPatientId("Bearer","mob",requestData);
        when(callRegisterRepository.findByMemberIdAndIsDeletedFalse(TestConstants.MEMBER_ID)).thenReturn(callRegisters);
        when(patientTransferRepository.findByPatientFhirId(requestData.getPatientId())).thenReturn(patientTransfers);
        //then
        patientService.deletePatientByPatientId(requestData);
        TestDataProvider.cleanUp();
    }

    @Test
    void updatePregnancyANCRisk() {
        //given
        TestDataProvider.init();
        PregnancyDetailsDTO pregnancyDetailsDTO = new PregnancyDetailsDTO();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.updatePregnancyANCRisk(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                pregnancyDetailsDTO)).thenReturn(Boolean.TRUE);
        //then
        Boolean response = patientService.updatePregnancyANCRisk(pregnancyDetailsDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateReferredSite() {
        //given
        TestDataProvider.init();
        ScreeningLogRequestDTO requestDTO = new ScreeningLogRequestDTO();
        //when
        TestDataProvider.getStaticMock();
        doNothing().when(fhirServiceApiInterface).updateReferredSite("Bearer", "mob",
                requestDTO);
        //then
        patientService.updateReferredSite(requestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientDetailsByVillageIds() {
        TestDataProvider.init();
        RequestDTO request = new RequestDTO();
        List<PatientDetailsDTO> patientDetailsDTOS = List.of(getPatientDetailsDTO());
        request.setPatientId(TestConstants.PATIENT_ID);
        request.setVillageIds(List.of(TestConstants.STRING_ONE));
        //then
        TestDataProvider.getStaticMock();
        List<PatientDetailsDTO> response = patientService.getPatientDetailsByVillageIds(request);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

}
