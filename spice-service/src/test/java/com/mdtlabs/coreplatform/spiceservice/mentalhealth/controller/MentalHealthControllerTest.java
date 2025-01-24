package com.mdtlabs.coreplatform.spiceservice.mentalhealth.controller;

import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.MentalHealthService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class MentalHealthControllerTest {
    @InjectMocks
    MentalHealthController mentalHealthController;
    @Mock
    MentalHealthService mentalHealthService;

    @Test
    void createMentalHealth() {
        AssessmentDTO request = TestDataProvider.getAssessmentData();
        doNothing().when(mentalHealthService).createMentalHealth(request);
        mentalHealthController.createMentalHealth(request);
    }

    @Test
    void getMentalHealthDetails() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        MentalHealthDTO mentalHealthDTO = new MentalHealthDTO();
        //when
        when(mentalHealthService.getMentalHealthDetails(request)).thenReturn(mentalHealthDTO);
        mentalHealthController.getMentalHealthDetails(request);
    }

    @Test
    void createMentalHealthCondition() {
        AssessmentDTO request = TestDataProvider.getAssessmentData();
        doNothing().when(mentalHealthService).createMentalHealthCondition(request);
        mentalHealthController.createMentalHealthCondition(request);
    }

    @Test
    void getMentalHealthCondition() {
        AssessmentDTO request = TestDataProvider.getAssessmentData();
        when(mentalHealthService.getMentalHealthCondition(request)).thenReturn(null);
        mentalHealthController.getMentalHealthCondition(request);
    }
}
