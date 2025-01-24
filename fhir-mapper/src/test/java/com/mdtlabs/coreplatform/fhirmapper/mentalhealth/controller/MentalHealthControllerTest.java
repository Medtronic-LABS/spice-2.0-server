package com.mdtlabs.coreplatform.fhirmapper.mentalhealth.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.mentalhealth.service.MentalHealthService;

/**
 * <p>
 *  MentalHealthControllerTest class used to test all possible positive
 *  and negative cases for all methods and conditions used in MentalHealthController class.
 * </p>
 *
 * @author Jaganathan created on Jan 30, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MentalHealthControllerTest {

    @InjectMocks
    private MentalHealthController mentalHealthController;

    @Mock
    private MentalHealthService mentalHealthService;

    @Test
    @DisplayName("MentalHealthCreate Test")
    void createMentalHealth() {
        //given
        AssessmentDTO request = new AssessmentDTO();
        request.setPhq4(TestDataProvider.getMentalHealthRequest());

        doNothing().when(mentalHealthService).createMentalHealth(request);

        ResponseEntity<String> response = mentalHealthController.createMentalHealth(request);
        verify(mentalHealthService,atLeastOnce()).createMentalHealth(request);
        Assertions.assertEquals(response.getStatusCode(), HttpStatusCode.valueOf(201));
    }

    @Test
    void getMentalHealthDetails() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        when(mentalHealthController.getMentalHealthDetails(request)).thenReturn(new MentalHealthDTO());
        MentalHealthDTO response = mentalHealthController.getMentalHealthDetails(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMentalHealthCondition() {
        AssessmentDTO request = TestDataProvider.getAssessmentDTO();
        when(mentalHealthController.getMentalHealthCondition(request)).thenReturn(request);
        AssessmentDTO response = mentalHealthController.getMentalHealthCondition(request);
        Assertions.assertNotNull(response);
    }

    @Test
    void createMentalHealthCondition() {
        AssessmentDTO request = TestDataProvider.getAssessmentDTO();
        request.setPhq4(TestDataProvider.getMentalHealthRequest());
        doNothing().when(mentalHealthService).createMentalHealthCondition(request);
        ResponseEntity<String> response = mentalHealthController.createMentalHealthCondition(request);
        verify(mentalHealthService,atLeastOnce()).createMentalHealthCondition(request);
        Assertions.assertEquals(response.getStatusCode(), HttpStatusCode.valueOf(201));
    }

}
