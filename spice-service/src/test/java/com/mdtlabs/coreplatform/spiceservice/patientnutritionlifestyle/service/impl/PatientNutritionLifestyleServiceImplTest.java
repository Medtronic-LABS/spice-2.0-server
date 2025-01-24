package com.mdtlabs.coreplatform.spiceservice.patientnutritionlifestyle.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * <p>
 *     Test class for {@link PatientNutritionLifestyleServiceImpl}
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Oct 07, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PatientNutritionLifestyleServiceImplTest {

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @InjectMocks
    private PatientNutritionLifestyleServiceImpl patientNutritionLifestyleService;

    @BeforeEach
    public void setup() {
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
    }

    @AfterEach
    public void cleanUp() {
        TestDataProvider.cleanUp();
    }

    @Test
    void addPatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        when(fhirServiceApiInterface.addPatientNutritionLifestyle(any(), any(), any(PatientNutritionLifestyle.class)))
                .thenReturn(patientNutritionLifestyle);

        PatientNutritionLifestyle result = patientNutritionLifestyleService.addPatientNutritionLifestyle(patientNutritionLifestyle);

        assertEquals(patientNutritionLifestyle, result);
    }

    @Test
    void getPatientNutritionLifeStyleList() {
        List<PatientNutritionLifestyle> patientNutritionLifestyles = Collections.singletonList(new PatientNutritionLifestyle());
        when(fhirServiceApiInterface.getPatientNutritionLifeStyleList(any(), any(), any(RequestDTO.class)))
                .thenReturn(patientNutritionLifestyles);

        RequestDTO requestDTO = new RequestDTO();
        List<PatientNutritionLifestyle> result = patientNutritionLifestyleService.getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(patientNutritionLifestyles, result);
    }

    @Test
    void updatePatientNutritionLifestyle() {
        PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles = new PatientNutritionLifestyleUpdateDTO();
        patientNutritionLifestyles.setLifestyles(new ArrayList<>());
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();
        patientNutritionLifestyles.getLifestyles().add(patientNutritionLifestyle);
        when(fhirServiceApiInterface.updatePatientNutritionLifestyle(any(), any(), any()))
                .thenReturn(patientNutritionLifestyles);

        PatientNutritionLifestyleUpdateDTO result = patientNutritionLifestyleService.updatePatientNutritionLifestyle(patientNutritionLifestyles);

        assertEquals(patientNutritionLifestyles, result);
    }

    @Test
    void removePatientNutritionLifestyle() {
        PatientNutritionLifestyle patientNutritionLifestyle = new PatientNutritionLifestyle();

        when(fhirServiceApiInterface.deletePatientNutritionLifestyle(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientNutritionLifestyle)).thenReturn(patientNutritionLifestyle);

        PatientNutritionLifestyle response = patientNutritionLifestyleService.removePatientNutritionLifestyle(patientNutritionLifestyle);
        assertNotNull(response);
    }

    @Test
    void addPatientNutritionLifestyle_nullInput() {
        PatientNutritionLifestyle result = patientNutritionLifestyleService.addPatientNutritionLifestyle(null);

        assertNull(result);
    }

    @Test
    void getPatientNutritionLifeStyleList_emptyList() {
        when(fhirServiceApiInterface.getPatientNutritionLifeStyleList(any(), any(), any(RequestDTO.class)))
                .thenReturn(Collections.emptyList());

        RequestDTO requestDTO = new RequestDTO();
        List<PatientNutritionLifestyle> result = patientNutritionLifestyleService.getPatientNutritionLifeStyleList(requestDTO);

        assertEquals(Collections.emptyList(), result);
    }

    @Test
    void updatePatientNutritionLifestyle_nullInput() {
        PatientNutritionLifestyleUpdateDTO result = patientNutritionLifestyleService.updatePatientNutritionLifestyle(null);

        assertNull(result);
    }

    @Test
    void removePatientNutritionLifestyle_nullInput() {
        PatientNutritionLifestyle patientNutritionLifestyle =null;
        when(fhirServiceApiInterface.deletePatientNutritionLifestyle(any(), any(), any())).thenReturn(patientNutritionLifestyle);
        PatientNutritionLifestyle response = patientNutritionLifestyleService.removePatientNutritionLifestyle(null);
        assertNull(response);
    }
}