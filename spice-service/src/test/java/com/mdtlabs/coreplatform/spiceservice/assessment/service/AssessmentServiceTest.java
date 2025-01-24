package com.mdtlabs.coreplatform.spiceservice.assessment.service;

import java.util.List;
import java.util.Objects;

import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.assessment.service.impl.AssessmentServiceImpl;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;

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
class AssessmentServiceTest {
    @InjectMocks
    AssessmentServiceImpl assessmentService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private FollowUpService followUpService;

    @Mock
    private CallRegisterRepository callRegisterRepository;

    @Test
    void createAssessment() {
        //given
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();
        assessmentDTO.setReferralTicketType("Referred");
        List<CallRegister> callRegisters = List.of(TestDataProvider.getCallRegister());
        List<String> referralTicketType =  List.of(assessmentDTO.getReferralTicketType());
        List<AppointmentType> appointmentType =  List.of(AppointmentType.HH_VISIT);

        //when
        when(fhirServiceApiInterface.assessmentCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(), assessmentDTO)).thenReturn(assessmentDTO);
        when(callRegisterRepository.findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(assessmentDTO.getEncounter().getMemberId(),
                referralTicketType, appointmentType, Boolean.FALSE)).thenReturn(callRegisters);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId("122");
        when(fhirServiceApiInterface.getHouseholdMemberById(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO)).thenReturn(TestDataProvider.getHouseHoldMember());
        when(fhirServiceApiInterface.getPatientVitals(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO)).thenReturn(TestDataProvider.getPregnancyInfo());
        //then
        AssessmentDTO response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        assessmentDTO.setAssessmentType(Constants.ANC);
        assessmentDTO.setAssessmentDetails(TestDataProvider.getAssessmentDetailsDTO(Constants.ANC));
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        assessmentDTO.setAssessmentType(Constants.PNC_MOTHER);
        assessmentDTO.setAssessmentDetails(TestDataProvider.getAssessmentDetailsDTO(Constants.PNC_MOTHER));
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        assessmentDTO.setAssessmentType(Constants.PNC_CHILD_REVIEW);
        assessmentDTO.setAssessmentDetails(TestDataProvider.getAssessmentDetailsDTO(Constants.PNC_CHILD_REVIEW));
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        assessmentDTO.setAssessmentType(Constants.CHILDHOOD_VISIT);
        assessmentDTO.setAssessmentDetails(TestDataProvider.getAssessmentDetailsDTO(Constants.CHILDHOOD_VISIT));
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        assessmentDTO.setAssessmentType(Constants.ICCM);
        assessmentDTO.setAssessmentDetails(TestDataProvider.getAssessmentDetailsDTO(Constants.ICCM));
        assessmentDTO.setSummary(null);
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        EncounterDetailsDTO encounterDetailsDTO = TestDataProvider.getEncounterDetailsDTO();
        encounterDetailsDTO.setReferred(Boolean.FALSE);
        assessmentDTO.setEncounter(encounterDetailsDTO);
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        assessmentDTO.setSummary(TestDataProvider.getSummary());
        response = assessmentService.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);

        TestDataProvider.cleanUp();
    }

}
