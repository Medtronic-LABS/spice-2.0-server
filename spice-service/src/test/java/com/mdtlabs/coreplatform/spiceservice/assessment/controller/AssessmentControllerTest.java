package com.mdtlabs.coreplatform.spiceservice.assessment.controller;

import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
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

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.assessment.service.AssessmentService;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;

/**
 * <p>
 * AssessmentControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in AssessmentController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AssessmentControllerTest {
    @InjectMocks
    AssessmentController assessmentController;

    @Mock
    AssessmentService assessmentService;

    @Test
    void createAssessment() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();
        ResponseEntity<AssessmentDTO> responseEntity = new ResponseEntity<>(assessmentDTO, HttpStatus.OK);

        //when
        when(assessmentService.createAssessment(assessmentDTO)).thenReturn(responseEntity.getBody());

        //then
        ResponseEntity<AssessmentDTO> response = assessmentController.createAssessment(assessmentDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createBpLog() {
        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();

        //when
        doNothing().when(assessmentService).createBpAssessment(assessmentDTO);

        //then
        SuccessResponse<String> response = assessmentController.createBpLog(assessmentDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void createGlucoseLog() {

        //given
        AssessmentDTO assessmentDTO = TestDataProvider.getAssessmentData();

        //when
        doNothing().when(assessmentService).createGlucoseLog(assessmentDTO);

        //then
        SuccessResponse<String> response = assessmentController.createGlucoseLog(assessmentDTO);
        Assertions.assertNotNull(response);

    }
}
