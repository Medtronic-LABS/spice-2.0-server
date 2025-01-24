package com.mdtlabs.coreplatform.fhirmapper.registration.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.registration.service.RegistrationService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RegistrationControllerTest {

    @InjectMocks
    private RegistrationController registrationController;

    @Mock
    private RegistrationService registrationService;

    @Test
    void testRegisterPatientAndMember() {
        EnrollmentRequestDTO request = TestDataProvider.getEnrollmentRequestDTO();
        EnrollmentResponseDTO response = new EnrollmentResponseDTO();

        when(registrationService.registerPatientAndMember(request)).thenReturn(response);
        
        EnrollmentResponseDTO actualResponse = registrationController.registerPatientAndMember(request);
        Assertions.assertNotNull(actualResponse);
    }

    @Test
    void testValidatePatient() {
        registrationController.validatePatient(new BioDataDTO());
        verify(registrationService, times(1)).validatePatient(new BioDataDTO());
    }


    @Test
    void updateMemberSignature() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        when(registrationService.updateMemberSignature(request)).thenReturn(Boolean.TRUE);
        boolean actualResponse = registrationController.updateMemberSignature(request);
        Assertions.assertTrue(actualResponse);
    }
}
