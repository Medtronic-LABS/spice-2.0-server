package com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.controller;

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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.patientnutritionlifestyle.service.PatientNutritionLifestyleService;

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
    void addPatientNutritionLifestyle_success() {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        when(patientNutritionLifestyleService.addPatientNutritionLifestyle(any(PatientNutritionLifestyle.class)))
                .thenReturn(patientNutritionLifestyle);

        PatientNutritionLifestyle response = patientNutritionLifestyleController
                .addPatientNutritionLifestyle(patientNutritionLifestyle);

        assertEquals(patientNutritionLifestyle, response);
    }

    @Test
    void getPatientNutritionLifeStyleList_success() {
        List<PatientNutritionLifestyle> patientNutritionLifestyles = Collections.singletonList(new PatientNutritionLifestyle());
        when(patientNutritionLifestyleService.getPatientNutritionLifeStyleList(any(RequestDTO.class)))
                .thenReturn(patientNutritionLifestyles);

        RequestDTO requestDTO = new RequestDTO();
        List<PatientNutritionLifestyle> response = patientNutritionLifestyleController
                .getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(patientNutritionLifestyles, response);
    }

    @Test
    void updatePatientNutritionLifestyle_success() {
        PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles = new PatientNutritionLifestyleUpdateDTO();
        when(patientNutritionLifestyleService.updatePatientNutritionLifestyle(any()))
                .thenReturn(patientNutritionLifestyles);
        PatientNutritionLifestyleUpdateDTO response = patientNutritionLifestyleController
                .updatePatientNutritionLifestyle(patientNutritionLifestyles);

        assertEquals(patientNutritionLifestyles, response);
    }

    @Test
    void deletePatientNutritionLifestyle_success() {
        //given
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();

        //when
        when(patientNutritionLifestyleService.removePatientNutritionLifestyle(patientNutritionLifestyle)).thenReturn(new PatientNutritionLifestyle());

        PatientNutritionLifestyle result = patientNutritionLifestyleController.deletePatientNutritionLifestyle(patientNutritionLifestyle);
        Assertions.assertNotNull(result);
    }

    @Test
    void getPatientNutritionLifeStyleList_emptyList() {
        when(patientNutritionLifestyleService.getPatientNutritionLifeStyleList(any(RequestDTO.class)))
                .thenReturn(Collections.emptyList());

        RequestDTO requestDTO = new RequestDTO();
        List<PatientNutritionLifestyle> response = patientNutritionLifestyleController
                .getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(Collections.emptyList(), response);
    }

}