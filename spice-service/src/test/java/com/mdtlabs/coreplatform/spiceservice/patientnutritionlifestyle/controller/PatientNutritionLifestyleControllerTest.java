package com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.service.PatientNutritionLifestyleService;

/**
 * <p>
 *     Test class for {@link PatientNutritionLifestyleController}
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientNutritionLifestyleControllerTest {

    @Mock
    private PatientNutritionLifestyleService patientNutritionLifestyleService;

    @InjectMocks
    private PatientNutritionLifestyleController patientNutritionLifestyleController;

    @Test
    void addPatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        when(patientNutritionLifestyleService.addPatientNutritionLifestyle(any(PatientNutritionLifestyle.class)))
                .thenReturn(patientNutritionLifestyle);

        SuccessResponse<PatientNutritionLifestyle> response = patientNutritionLifestyleController
                .addPatientNutritionLifestyle(patientNutritionLifestyle);

        Assertions.assertEquals(HttpStatus.CREATED, response.getStatusCode());
    }

    @Test
    void getPatientNutritionLifeStyleList() {
        List<PatientNutritionLifestyle> patientNutritionLifestyles = Collections.singletonList(new PatientNutritionLifestyle());
        when(patientNutritionLifestyleService.getPatientNutritionLifeStyleList(any(RequestDTO.class)))
                .thenReturn(patientNutritionLifestyles);

        RequestDTO requestDTO = new RequestDTO();
        SuccessResponse<PatientNutritionLifestyle> response = patientNutritionLifestyleController
                .getPatientNutritionLifeStyleList(requestDTO);

        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updatePatientNutritionLifestyle() {
        PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles = new PatientNutritionLifestyleUpdateDTO();
        patientNutritionLifestyles.setLifestyles(new ArrayList<>());
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        patientNutritionLifestyles.getLifestyles().add(patientNutritionLifestyle);
        when(patientNutritionLifestyleService.updatePatientNutritionLifestyle(any()))
                .thenReturn(patientNutritionLifestyles);

        SuccessResponse<PatientNutritionLifestyleUpdateDTO> response = patientNutritionLifestyleController
                .updatePatientNutritionLifestyle(patientNutritionLifestyles);

        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deletePatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();

        SuccessResponse<PatientNutritionLifestyle> response = patientNutritionLifestyleController
                .deletePatientNutritionLifestyle(patientNutritionLifestyle);

        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
}