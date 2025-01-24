package com.mdtlabs.coreplatform.spiceservice.registration.controller;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.http.HttpStatus;


import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.registration.service.RegistrationService;

@ExtendWith(MockitoExtension.class)
class RegistrationControllerTest {

    @InjectMocks
    RegistrationController registrationController;

    @Mock
    RegistrationService registrationService;

    @Test
    void testRegisterPatientAndMember() throws JsonProcessingException {
        EnrollmentRequestDTO enrollmentRequestDTO = new EnrollmentRequestDTO();
        ObjectMapper objectMapper = new ObjectMapper();
        String requestJson = objectMapper.writeValueAsString(enrollmentRequestDTO);
        MockMultipartFile signatureFile = new MockMultipartFile("signatureFile", "signature.jpg", MediaType.IMAGE_JPEG_VALUE, "signature content".getBytes());

        registrationController.registerPatientAndMember(requestJson, signatureFile);
        verify(registrationService, times(1)).registerPatientAndMember(enrollmentRequestDTO, signatureFile);
    }

    @Test
    void testRegisterPatientAndMemberInvalidJson() {
        String requestJson = "{\"invalidJson\": \"missing closing bracket\"}";
        MockMultipartFile signatureFile = new MockMultipartFile("signatureFile", "signature.jpg", MediaType.IMAGE_JPEG_VALUE, "signature content".getBytes());
        assertThrows(SpiceValidation.class, () ->registrationController.registerPatientAndMember(requestJson, signatureFile));
    }

    @Test
    void testValidatePatient() {
        PatientValidationResponseDTO mockResponse = new PatientValidationResponseDTO();
        when(registrationService.isPatientRegisteredInRelatedPerson(any(BioDataDTO.class))).thenReturn(mockResponse);

        SuccessResponse<PatientValidationResponseDTO> response = registrationController.validatePatient(new BioDataDTO());
        assertNotNull(response);
        assertEquals(HttpStatus.OK,response.getStatusCode() ) ;
    }

    @Test
    void testValidatePatientWhenPatientDetailsNotNull() {
        PatientValidationResponseDTO mockResponse = new PatientValidationResponseDTO();
        mockResponse.setPatientDetails(new BioDataDTO());
        when(registrationService.isPatientRegisteredInRelatedPerson(any(BioDataDTO.class))).thenReturn(mockResponse);

        SuccessResponse<PatientValidationResponseDTO> response = registrationController.validatePatient(new BioDataDTO());
        assertNotNull(response);
        assertEquals(HttpStatus.CONFLICT,response.getStatusCode() );
    }
}