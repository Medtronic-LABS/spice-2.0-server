package com.mdtlabs.coreplatform.adminservice.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.dto.ClassificationDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.MedicationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.DosageForm;
import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;
import com.mdtlabs.coreplatform.adminservice.service.impl.MedicationServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

@ExtendWith(MockitoExtension.class)
class MedicationControllerTest {

    @InjectMocks
    private MedicationController medicationController;

    @Mock
    private MedicationServiceImpl medicationService;

    @Mock
    private ModelMapper modelMapper;

    @Test
    void testAddMedication() {
        //given
        List<MedicationDTO> medicationDTOs = new ArrayList<>();
        List<Medication> medications = new ArrayList<>();

        //when
        when(medicationService.createMedication(medicationDTOs)).thenReturn(medications);

        //then
        SuccessResponse<Medication> actualMedication = medicationController.addMedication(medicationDTOs);
        assertNotNull(actualMedication);
        assertEquals(HttpStatus.CREATED, actualMedication.getStatusCode());
    }

    @Test
    void testUpdateMedication() {
        //given
        Medication medication = TestDataProvider.getMedication();
        MedicationDTO medicationDTO = TestDataProvider.getMedicationDTOs().get(0);

        //when
        when(medicationService.updateMedication(medicationDTO)).thenReturn(medication);

        //then
        SuccessResponse<Medication> actualMedication = medicationController.updateMedication(medicationDTO);
        assertNotNull(actualMedication);
        assertEquals(HttpStatus.OK, actualMedication.getStatusCode());
    }

    @Test
    void testGetMedicationById() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        Medication medication = TestDataProvider.getMedication();

        //when
        when(medicationService.getMedicationById(requestDTO)).thenReturn(medication);

        //then
        SuccessResponse<Medication> actualMedication = medicationController.getMedicationById(requestDTO);
        assertNotNull(actualMedication);
        assertEquals(HttpStatus.OK, actualMedication.getStatusCode());
    }

    @Test
    void testGetAllMedicationsEmpty() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
       ResponseListDTO<MedicationDTO> medications = new ResponseListDTO<>();

        //when
        when(medicationService.getAllMedications(requestDTO)).thenReturn(medications);

        //then
        SuccessResponse<MedicationDTO> actualMedications = medicationController.getAllMedications(requestDTO);
        assertNotNull(actualMedications);
        assertEquals(HttpStatus.OK, actualMedications.getStatusCode());
    }

    @Test
    void testDeleteMedicationById() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        //when
        when(medicationService.deleteMedicationById(requestDTO)).thenReturn(Boolean.TRUE);

        //then
        SuccessResponse<Boolean> deleteMedicationById = medicationController.deleteMedicationById(requestDTO);
        assertNotNull(deleteMedicationById);
        assertEquals(HttpStatus.OK, deleteMedicationById.getStatusCode());
    }


    @Test
    void testValidateMedication() {
        //given
        MedicationDTO medicationDTO = TestDataProvider.getMedicationDTOs().get(0);

        //when
        when(medicationService.validateMedication(medicationDTO)).thenReturn(Boolean.TRUE);
        //then
        ResponseEntity<Boolean> validatedMedication = medicationController.validateMedication(medicationDTO);
        assertNotNull(validatedMedication);
        assertEquals(HttpStatus.OK, validatedMedication.getStatusCode());
    }

    @Test
    void getMedicationByName() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();

        //when
        when(medicationService.getMedicationByName(requestDTO)).thenReturn(new Medication());

        //then
        ResponseEntity<Medication> response = medicationController.getMedicationByName(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getClassifications() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setCountryId(TestConstants.ONE);

        //when
        when(medicationService.getClassifications(requestDTO.getCountryId())).thenReturn(List.of(new ClassificationDTO()));

        //then
        SuccessResponse<ClassificationDTO> response = medicationController.getClassifications(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getDosageForms() {
        //when
        when(medicationService.getDosageForms()).thenReturn(List.of(new DosageForm()));

        //then
        SuccessResponse<DosageForm> response = medicationController.getDosageForms();
        Assertions.assertNotNull(response);
    }

    @Test
    void searchMedicationByName() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setCountryId(TestConstants.ONE);

        //when
        when(medicationService.searchMedicationByName(requestDTO)).thenReturn(new ResponseListDTO<MedicationDTO>());

        //then
        SuccessResponse<MedicationDTO> response = medicationController.searchMedicationByName(requestDTO);
        Assertions.assertNotNull(response);
    }
}
