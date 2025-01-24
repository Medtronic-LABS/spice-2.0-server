package com.mdtlabs.coreplatform.fhirmapper.assessment.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;

/**
 * <p>
 * AssessmentControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in AssessmentController class.
 * </p>
 *
 * @author Nandhakumar
 * @since Feb 8, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AssessmentControllerTest {

    @Mock
    AssessmentService assessmentService;

    @InjectMocks
    AssessmentController assessmentController;

    /**
     * Creates new Assessment  Unit Test case
     */
    @Test
    void createAssessment() {
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        when(assessmentService.createAssessment(assessmentDTO)).thenReturn(assessmentDTO);
        AssessmentDTO responsevalue = assessmentController.createAssessment(assessmentDTO);
        Assertions.assertNotNull(responsevalue);
    }

    @Test
    void createTB() {
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        when(assessmentService.createTB(assessmentDTO)).thenReturn(new ResponseEntity<>(HttpStatus.OK));
        ResponseEntity responsevalue = assessmentController.createTB(assessmentDTO);
        Assertions.assertNotNull(responsevalue);
    }

    @Test
    void getLatestBpLog() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        when(assessmentService.getLatestBpLog(assessmentDTO)).thenReturn(TestDataProvider.getBpLogRequest());
        BpLogDTO response = assessmentController.getLatestBpLog(assessmentDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getLatestGlucoseLog() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentDTO();
        when(assessmentService.getLatestGlucoseLog(assessmentDTO)).thenReturn(TestDataProvider.getGlucoseLogRequest());
        GlucoseLogDTO response = assessmentController.getLatestGlucoseLog(assessmentDTO);
        Assertions.assertNotNull(response);
    }
    
}
