package com.mdtlabs.coreplatform.spiceservice.patienttransfer.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.security.authentication.BadCredentialsException;

import static org.junit.Assert.assertThrows;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferUpdateRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.repository.PatientTransferRepository;
import com.mdtlabs.coreplatform.spiceservice.patienttransfer.service.impl.PatientTransferServiceImpl;

/**
 * <p>
 * PatientTransferServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in PatientTransferService class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 25 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientTransferServiceTest {

    @InjectMocks
    PatientTransferServiceImpl patientTransferService;

    @Mock
    private PatientTransferRepository patientTransferRepository;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private CustomizedModulesService customizedModulesService;

    @Mock
    private FollowUpService followUpService;

    @Test
    void createPatientTransfer() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        PatientTransferRequestDTO patientTransferRequestDTO = TestDataProvider.getPatientTransferRequestDTO();
        PatientTransfer patientTransfer = new PatientTransfer();
        patientTransfer.setTransferSite(new HealthFacility(patientTransferRequestDTO.getTransferSite()));
        patientTransfer.setOldSite(new HealthFacility(patientTransferRequestDTO.getOldSite()));
        patientTransfer.setTransferTo(new User(patientTransferRequestDTO.getTransferTo()));
        patientTransfer.setTransferBy(new User(UserContextHolder.getUserDto().getId()));
        patientTransfer.setTransferReason(patientTransferRequestDTO.getTransferReason());
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransfer.setTenantId(UserSelectedTenantContextHolder.get());
        patientTransfer.setPatientFhirId(patientTransferRequestDTO.getPatientReference());
        RequestDTO requestDto = new RequestDTO();
        requestDto.setPatientReference(patientTransferRequestDTO.getPatientReference());

        //when
        when(patientTransferRepository.findByPatientTrackIdAndTransferStatus(patientTransferRequestDTO.getPatientReference(),
                PatientTransferStatus.PENDING)).thenReturn(null);

        //then
        patientTransferService.createPatientTransfer(patientTransferRequestDTO);
        verify(patientTransferRepository, atLeastOnce()).save(patientTransfer);
        TestDataProvider.cleanUp();
    }

    @Test
    void validateCreatePatientTransfer() {
        //given
        PatientTransferRequestDTO patientReferencerequest = TestDataProvider.getPatientTransferRequestDTO();
        patientReferencerequest.setPatientReference(null);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(patientReferencerequest));
        patientReferencerequest.setPatientReference(TestConstants.EMPTY);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(patientReferencerequest));

        //given
        PatientTransferRequestDTO transferToRequest = TestDataProvider.getPatientTransferRequestDTO();
        transferToRequest.setTransferTo(null);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(transferToRequest));
        //given
        PatientTransferRequestDTO transferSiteRequest = TestDataProvider.getPatientTransferRequestDTO();
        transferSiteRequest.setTransferSite(null);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(transferSiteRequest));

        //given
        PatientTransferRequestDTO transferReasonRequest = TestDataProvider.getPatientTransferRequestDTO();
        transferReasonRequest.setTransferReason(null);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(transferReasonRequest));
        patientReferencerequest.setTransferReason(TestConstants.EMPTY);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(patientReferencerequest));

        //given
        PatientTransferRequestDTO oldSiteRequest = TestDataProvider.getPatientTransferRequestDTO();
        oldSiteRequest.setOldSite(null);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.createPatientTransfer(oldSiteRequest));
    }

    @Test
    void updatePatientTransferValidationTest() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        PatientTransferUpdateRequestDTO patientTransferUpdateRequestDTO = TestDataProvider.getPatientTransferUpdateRequestDTO();
        PatientTransfer patientTransfer = TestDataProvider.getPatientTransfer();

        //when
        when(patientTransferRepository.findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE)).thenReturn(patientTransfer);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.CANCELED);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.REJECTED);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.REMOVED);
        patientTransfer.setShow(Boolean.FALSE);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //when
        when(patientTransferRepository.findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE)).thenReturn(null);

        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //given
        HealthFacility healthFacility = patientTransfer.getTransferSite();
        healthFacility.setDeleted(Boolean.TRUE);
        patientTransfer.setTransferSite(healthFacility);

        //when
        when(patientTransferRepository.findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE)).thenReturn(patientTransfer);

        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void updatePatientTransferTest() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        PatientTransferUpdateRequestDTO patientTransferUpdateRequestDTO = TestDataProvider.getPatientTransferUpdateRequestDTO();
        PatientTransfer patientTransfer = TestDataProvider.getPatientTransfer();
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.ACCEPTED);
        User user = TestDataProvider.getUser();
        user.setId(TestConstants.TWO);

        //when
        when(patientTransferRepository.findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE)).thenReturn(patientTransfer);

        //then
        patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO);
        verify(patientTransferRepository, atLeastOnce()).findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE);

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.REMOVED);
        patientTransfer.setShow(Boolean.TRUE);

        //then
        patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO);
        verify(patientTransferRepository, atLeastOnce()).findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE);

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.REJECTED);
        patientTransferUpdateRequestDTO.setRejectReason(String.valueOf(PatientTransferStatus.REJECTED));
        patientTransfer.setShow(Boolean.TRUE);

        //then
        patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO);
        verify(patientTransferRepository, atLeastOnce()).findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE);

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.REJECTED);
        patientTransferUpdateRequestDTO.setRejectReason(null);
        patientTransfer.setShow(Boolean.TRUE);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.CANCELED);
        patientTransfer.setShow(Boolean.TRUE);

        //then
        patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO);
        verify(patientTransferRepository, atLeastOnce()).findByIdAndIsDeleted(patientTransferUpdateRequestDTO.getId(), Boolean.FALSE);

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.ACCEPTED);
        patientTransfer.setShow(Boolean.TRUE);
        patientTransfer.setTransferTo(user);

        //then
        Assertions.assertThrows(BadCredentialsException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));

        //given
        patientTransfer.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransferUpdateRequestDTO.setTransferStatus(PatientTransferStatus.PENDING);
        patientTransfer.setShow(Boolean.TRUE);

        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.updatePatientTransfer(patientTransferUpdateRequestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientTransferCountTest() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setHealthFacilityId(Constants.LONG_ONE);

        //then
        patientTransferService.getPatientTransferCount(requestDTO);
        verify(patientTransferRepository, atLeastOnce()).getPatientTransferCount(Constants.LONG_ONE, Constants.LONG_ONE);

        requestDTO.setHealthFacilityId(null);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.getPatientTransferCount(requestDTO));
        TestDataProvider.cleanUp();
    }

//    @Test
    void getPatientTransferListTest() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setHealthFacilityId(Constants.LONG_ONE);
        RequestDTO fhirRequestDTO = new RequestDTO();
        fhirRequestDTO.setPatientId(Constants.STRING_ONE);
        PatientTransfer patientTransfer = TestDataProvider.getPatientTransfer();
        patientTransfer.setId(TestConstants.ONE);
        patientTransfer.setMemberId(TestConstants.STRING_ONE);
        List<PatientTransfer> patientTransfers = List.of(patientTransfer);
        PatientDTO patientDTO = TestDataProvider.getPatientDTO();
        patientDTO.setPatientId(TestConstants.STRING_ONE);
        patientDTO.setType("assessment");
        patientDTO.setAge(26L);
        patientDTO.setGender(Constants.MALE);

        PatientDetailsDTO patientDetailsDTO = new PatientDetailsDTO();
        patientDetailsDTO.setPatientId(TestConstants.STRING_ONE);
        patientDetailsDTO.setName(TestConstants.NAME);
        patientDetailsDTO.setDateOfBirth(new Date());
        patientDetailsDTO.setPhoneNumber(TestConstants.PHONE_NUMBER);
        ConfirmDiagnosisDTO confirmDiagnosisDTO = new ConfirmDiagnosisDTO();

        List<Map<String, String>> diagnosis = new ArrayList<>();
        Map<String, String> diagnosisMap = new HashMap<>();
        diagnosisMap.put("NAME", "VALUE");
        diagnosis.add(diagnosisMap);
        confirmDiagnosisDTO.setDiagnosis(diagnosis);
        patientDetailsDTO.setConfirmDiagnosis(confirmDiagnosisDTO);

        //when
        when(patientTransferRepository.getIncomingList(Constants.LONG_ONE, TestConstants.ONE, PatientTransferStatus.PENDING)).thenReturn(patientTransfers);
        when(patientTransferRepository.getOutgoingList(Constants.LONG_ONE, Constants.LONG_ONE)).thenReturn(patientTransfers);
        when(fhirServiceApiInterface.getPatientById("BearerTest", "mob", fhirRequestDTO)).thenReturn(patientDTO);
        when(fhirServiceApiInterface.searchPatientDetails("BearerTest", "mob", patientDTO)).thenReturn(patientDetailsDTO);

        //then
        patientTransferService.getPatientTransferList(requestDTO);
        verify(patientTransferRepository, atLeastOnce()).getIncomingList(Constants.LONG_ONE, Constants.LONG_ONE, PatientTransferStatus.PENDING);

        requestDTO.setHealthFacilityId(null);
        Assertions.assertThrows(DataNotAcceptableException.class, () -> patientTransferService.getPatientTransferList(requestDTO));
        TestDataProvider.cleanUp();
    }

    @Test
    void getPatientTransferList() {
        //given
        TestDataProvider.init();
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();

        RequestDTO requestDTO1 = new RequestDTO();
        requestDTO1.setPatientId(TestConstants.STRING_ONE);

        //then
        assertThrows(DataNotAcceptableException.class, () -> patientTransferService.getPatientTransferList(requestDTO));
        //given
        requestDTO.setHealthFacilityId(TestConstants.ONE);
        PatientTransfer patientTransfer = TestDataProvider.getPatientTransfer();
        patientTransfer.setId(TestConstants.ONE);
        patientTransfer.setMemberId(TestConstants.STRING_ONE);
        PatientDTO patientDTO = TestDataProvider.getPatientDTO();
        patientTransfer.setPatientFhirId(TestConstants.STRING_ONE);
        patientDTO.setPatientId(TestConstants.STRING_ONE);
        patientDTO.setType("assessment");
        patientDTO.setAge(26L);
        patientDTO.setGender(Constants.MALE);
        patientDTO.setId(TestConstants.STRING_ONE);
        requestDTO.setPatientId(TestConstants.PATIENT_ID);
        PatientDTO patientDTO1 = new PatientDTO();
        patientDTO1.setType("assessment");
        patientDTO.setType(com.mdtlabs.coreplatform.commonservice.common.Constants.ASSESSMENT);
        PatientDetailsDTO patientDetailsDTO = TestDataProvider.getPatientDetailsDTO();
        patientDetailsDTO.setConfirmDiagnosis(TestDataProvider.getConfirmDiagnosisDTO());
        patientDetailsDTO.getConfirmDiagnosis().setDiagnosis(TestDataProvider.getConfirmDiagnosisDTO().getDiagnosis());
        List<PatientTransfer> incomingList = List.of(TestDataProvider.getPatientTransfer());
        List<PatientTransfer> outgoingList = List.of(TestDataProvider.getPatientTransfer());

        //when
        TestDataProvider.getStaticMock();
        when(patientTransferRepository.getIncomingList(TestConstants.ONE, TestConstants.ONE, PatientTransferStatus.PENDING)).thenReturn(incomingList);
        when(patientTransferRepository.getOutgoingList(TestConstants.ONE, TestConstants.ONE)).thenReturn(outgoingList);
        when(fhirServiceApiInterface.getPatientById("BearerTest", "mob", requestDTO1)).thenReturn(patientDTO);
        when(fhirServiceApiInterface.searchPatientDetails("BearerTest","mob", patientDTO1)).thenReturn(patientDetailsDTO);

        //then
        Map<String, Object> response = patientTransferService.getPatientTransferList(requestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(response);
    }


}
