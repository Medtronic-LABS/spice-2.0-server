package com.mdtlabs.coreplatform.spiceservice.mentalhealth.service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.impl.MentalHealthServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class MentalHealthServiceTest {

    @InjectMocks
    private MentalHealthServiceImpl mentalHealthService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Test
    void createMentalHealth() {
        TestDataProvider.init();
        AssessmentDTO mentalHealth = TestDataProvider.getAssessmentData();
        when(fhirServiceApiInterface.createMentalHealth(CommonUtil.getAuthToken(), CommonUtil.getClient(), mentalHealth)).thenReturn(null);
        mentalHealthService.createMentalHealth(mentalHealth);
        TestDataProvider.cleanUp();
    }

    @Test
    void getMentalHealthDetails() {
        TestDataProvider.init();
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        when(fhirServiceApiInterface.getMentalHealthDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(null);
        mentalHealthService.getMentalHealthDetails(requestDTO);
        TestDataProvider.cleanUp();
    }

    @Test
    void createMentalHealthCondition() {
        TestDataProvider.init();
        AssessmentDTO mentalHealth = TestDataProvider.getAssessmentData();
        when(fhirServiceApiInterface.createMentalHealthCondition(CommonUtil.getAuthToken(), CommonUtil.getClient(), mentalHealth)).thenReturn(null);
        mentalHealthService.createMentalHealthCondition(mentalHealth);
        TestDataProvider.cleanUp();
    }

    @Test
    void getMentalHealthCondition() {
        TestDataProvider.init();
        AssessmentDTO mentalHealth = TestDataProvider.getAssessmentData();
        when(fhirServiceApiInterface.getMentalHealthCondition(CommonUtil.getAuthToken(), CommonUtil.getClient(), mentalHealth)).thenReturn(null);
        mentalHealthService.getMentalHealthCondition(mentalHealth);
        TestDataProvider.cleanUp();
    }

}
