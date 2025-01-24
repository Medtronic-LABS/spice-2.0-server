package com.mdtlabs.coreplatform.adminservice.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.mdtlabs.coreplatform.adminservice.model.dto.ClassificationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.Classification;
import com.mdtlabs.coreplatform.adminservice.model.entity.DosageForm;
import com.mdtlabs.coreplatform.adminservice.repository.ClassificationRepository;
import com.mdtlabs.coreplatform.adminservice.repository.DosageFormRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.internal.InheritingConfiguration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.mdtlabs.coreplatform.adminservice.model.dto.MedicationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.Medication;
import com.mdtlabs.coreplatform.adminservice.repository.MedicationRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.MedicationServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MedicationServiceTest {
    
    @InjectMocks
    MedicationServiceImpl medicationService;

    @Mock
    MedicationRepository medicationRepository;

    @Mock
    ModelMapper modelMapper;

    @Mock
    private ClassificationRepository classificationRepository;

    @Mock
    private DosageFormRepository dosageFormRepository;

    @Test
    void addDuplicateMedication() {
        //given
        List<MedicationDTO> medications = TestDataProvider.getMedicationDTOs();
        medications.add(TestDataProvider.getMedicationDTO());
        //then
        Assertions.assertThrows(DataNotAcceptableException.class,
                () -> medicationService.createMedication(medications));
    }

    @Test
    void addMedication() {
        //given
        List<MedicationDTO> medications = TestDataProvider.getMedicationDTOs();
        List<Medication> medications2 = TestDataProvider.getMedications();
        new ModelMapper().map(medications, new TypeToken<List<Medication>>() {}.getType());
        //when
        when(medicationRepository.saveAll(medications2)).thenReturn(medications2);
        when(modelMapper.map(medications, new TypeToken<List<Medication>>() {}.getType())).thenReturn(medications2);
        //then
        List<Medication> actualMedications = medicationService.createMedication(medications);
        assertNotNull(actualMedications);
        assertFalse(actualMedications.isEmpty());
        assertEquals(medications.size(), actualMedications.size());
        assertEquals(medications2.get(0), actualMedications.get(0));
    }

    @Test
    void testUpdateMedication() {
        //given
        MedicationDTO request = TestDataProvider.getMedicationDTO();
        Medication medication = TestDataProvider.getMedication();

        //when
        when(medicationRepository.getMedicationByMandatoryFields(
                medication.getClassificationId(), medication.getBrandId(), medication.getDosageFormId(),
                medication.getCountryId(), medication.getName())).thenReturn(medication);
        when(medicationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(medication.getId())).thenReturn(medication);
        when(modelMapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(medicationRepository.save(medication)).thenReturn(medication);

        //then
        Medication actualMedication = medicationService.updateMedication(request);
        assertNotNull(actualMedication);
        assertEquals(medication.getName(), actualMedication.getName());
    }

    @Test
    void testValidateMedication() {
        //given
        MedicationDTO medicationDTO = TestDataProvider.getMedicationDTO();
        Medication medication = TestDataProvider.getMedication();

        //when
        when(medicationRepository.getMedicationByMandatoryFields(medication.getClassificationId(), medication.getBrandId(),
                medication.getDosageFormId(), medication.getCountryId(), medication.getName()))
                .thenReturn(medication);

                
        //then
        Boolean actualMedication = medicationService.validateMedication(medicationDTO);
        assertNotNull(actualMedication);
        assertTrue(actualMedication);

        medicationDTO.setName(null);
        medicationDTO.setClassificationId(null);
        medicationDTO.setBrandId(null);
        medicationDTO.setDosageFormId(null);
        medicationDTO.setCodeDetails(null);
        medicationDTO.setCountryId(null);
        assertThrows(DataNotAcceptableException.class, () ->medicationService.validateMedication(medicationDTO));
    }

    @Test
    void testGetMedicationById() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        Medication medication = TestDataProvider.getMedication();
        requestDTO.setId(TestConstants.ONE);

        //when
        when(medicationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(medication);

        //then
        Medication actualMedication = medicationService.getMedicationById(requestDTO);
        assertNotNull(actualMedication);
        assertEquals(medication.getName(), actualMedication.getName());
        assertEquals(medication, actualMedication);
    }

    @Test
    void testGetAllMedications() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setSearchTerm("par");
        requestDTO.setCountryId(TestConstants.ONE);
        requestDTO.setTenantId(TestConstants.ONE);
        List<Medication> medications = TestDataProvider.getMedications();
        Page<Medication> medicationPage = new PageImpl<>(medications);
        Pageable pageable = PageRequest.of(Constants.ZERO, TestConstants.TEN,
                Sort.by("updatedAt").descending());
        List<MedicationDTO> medicationDTOs = TestDataProvider.getMedicationDTOs();
        
        when(medicationRepository.getAllMedications(requestDTO.getSearchTerm(),
                requestDTO.getCountryId(), pageable)).thenReturn(medicationPage);
        when(modelMapper.map(medicationPage.stream().toList(), new TypeToken<List<MedicationDTO>>() {}.getType())).thenReturn(medicationDTOs);

        //then
        ResponseListDTO<MedicationDTO> actualMedications = medicationService.getAllMedications(requestDTO);
        assertNotNull(actualMedications);

        requestDTO.setCountryId(null);
        assertThrows(DataNotAcceptableException.class, ()->medicationService.getAllMedications(requestDTO));
    }

    @Test
    void getAllMedicationsInvalidSearch() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setCountryId(TestConstants.ONE);
        requestDTO.setTenantId(TestConstants.ONE);
        
        //then
        ResponseListDTO<MedicationDTO> actualMedications = medicationService.getAllMedications(requestDTO);
        assertNotNull(actualMedications);
         
        assertEquals(null, actualMedications.getData());
    }

    @Test
    void testDeleteMedicationById() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setId(TestConstants.ONE);
        Medication medication = TestDataProvider.getMedication();
        medication.setDeleted(true);

        //when
        when(medicationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(medication);
        when(medicationRepository.save(medication)).thenReturn(medication);

        //then
        Boolean actualMedication = medicationService.deleteMedicationById(requestDTO);
        assertNotNull(actualMedication);
        assertTrue(actualMedication);
    }


    @Test
    void testGetOtherMedication() {
        //given
        Medication medication = TestDataProvider.getMedication();

        //when
        when(medicationRepository.getOtherMedication(TestConstants.ONE, Constants.OTHER,
                Constants.OTHER, Constants.OTHER, Constants.OTHER)).thenReturn(medication);

        //then
        Medication actualMedication = medicationService.getOtherMedication(TestConstants.ONE);
        assertNotNull(actualMedication);
        assertEquals(medication.getName(), actualMedication.getName());
        assertEquals(medication, actualMedication);
    }

    @Test
    void throwDataNotFoundException() {
        //given
        MedicationDTO medicationDTO = TestDataProvider.getMedicationDTO();
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        Medication medication = TestDataProvider.getMedication();
        requestDTO.setId(TestConstants.ONE);

        //when
        when(medicationRepository.getMedicationByMandatoryFields(
                medication.getClassificationId(), medication.getBrandId(), medication.getDosageFormId(),
                medication.getCountryId(), medication.getName())).thenReturn(medication);
        when(medicationRepository
                .findByIdAndIsDeletedFalseAndIsActiveTrue(medication.getId())).thenReturn(null);
        when(medicationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getId())).thenReturn(null);

        //then
        assertThrows(DataNotFoundException.class, () -> medicationService.updateMedication(medicationDTO));
        assertThrows(DataNotFoundException.class, () -> medicationService.getMedicationById(requestDTO));
    }

    @Test
    void throwDataConflictException() {
        //given
        MedicationDTO medicationDTO = TestDataProvider.getMedicationDTO();
        Medication medication = TestDataProvider.getMedication();
        Medication existingMedication = TestDataProvider.getMedication();
        existingMedication.setId(2l);

        //when
        when(medicationRepository.getMedicationByMandatoryFields(
                medication.getClassificationId(), medication.getBrandId(), medication.getDosageFormId(),
                medication.getCountryId(), medication.getName())).thenReturn(existingMedication);

        //then
        assertThrows(DataConflictException.class, () -> medicationService.updateMedication(medicationDTO));
    }

    // @Test
    // void throwDataNotAcceptableException() {
    //     //given
    //     MedicationDTO medicationDTO = TestDataProvider.getMedicationDTO();
    //     MedicationDTO medicationValidation = new MedicationDTO();
    //     SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
    //     Medication medication = TestDataProvider.getMedication();
    //     medication.setId(TestConstants.ONE);
    //     medication.setCountryId(TestConstants.TWO);

    //     Medication existingMedication = TestDataProvider.getMedication();
    //     existingMedication.setName("Dolo");

    //     // Medication secondMedication = TestDataProvider.getMedication();
    //     MedicationDTO secondMedication = TestDataProvider.getMedicationDTO();

    //     secondMedication.setId(TestConstants.TWO);
    //     Medication existingSecondMedication = TestDataProvider.getMedication();
    //     existingSecondMedication.setId(TestConstants.TWO);
    //     existingSecondMedication.setCountryId(TestConstants.TWO);
    //     List<MedicationDTO> medications = new ArrayList<>();
    //     medications.add(medicationDTO);
    //     medications.add(medicationDTO);
    //     //when
    //     when(medicationRepository.getMedicationByMandatoryFields(
    //             medication.getClassificationId(), medication.getBrandId(), medication.getDosageFormId(),
    //             medication.getCountryId(), medication.getName(), medication.getTenantId())).thenReturn(medication);
    //     when(medicationRepository
    //             .findByIdAndIsDeletedFalseAndTenantId(TestConstants.ONE, medication.getTenantId())).thenReturn(existingMedication);
    //     when(medicationRepository
    //             .findByIdAndIsDeletedFalseAndTenantId(TestConstants.TWO, medication.getTenantId())).thenReturn(existingSecondMedication);

    //     //then
    //     assertThrows(DataNotAcceptableException.class, () -> medicationService.getAllMedications(requestDTO));
    //     // assertThrows(DataNotAcceptableException.class, () -> medicationService.deleteMedicationById(requestDTO));
    //     assertThrows(DataNotAcceptableException.class, () -> medicationService.updateMedication(medicationDTO));
    //     assertThrows(DataNotAcceptableException.class, () -> medicationService.updateMedication(secondMedication));
    //     assertThrows(DataNotAcceptableException.class, () -> medicationService.updateMedication(medicationValidation));
    //     assertThrows(DataNotAcceptableException.class, () -> medicationService.createMedication(medications));
    // }

    @Test
    void getMedicationByName() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();

        assertThrows(DataNotAcceptableException.class, () -> medicationService.getMedicationByName(requestDTO));
        requestDTO.setName("ABC");
        Medication medication = null;
        when(medicationRepository.findByNameAndIsDeletedFalseAndIsActiveTrue(requestDTO.getName())).thenReturn(medication);
        assertThrows(DataNotFoundException.class, () -> medicationService.getMedicationByName(requestDTO));


        medication = TestDataProvider.getMedication();
        when(medicationRepository.findByNameAndIsDeletedFalseAndIsActiveTrue(requestDTO.getName())).thenReturn(medication);
        Medication response = medicationService.getMedicationByName(requestDTO);
        assertNotNull(response);
    }


    @Test
    void getClassificationsTest() {
        when(classificationRepository.getClassifications(TestConstants.ONE)).thenReturn(List.of(new Classification()));
        List<ClassificationDTO> response = medicationService.getClassifications(TestConstants.ONE);
        assertNotNull(response);
    }

    @Test
    void getDosageFormsTest() {
        when(dosageFormRepository.findAll()).thenReturn(List.of(new DosageForm()));
        List<DosageForm> response = medicationService.getDosageForms();
        assertNotNull(response);
    }

    @Test
    void searchMedicationByNameTest(){
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        Medication medication = TestDataProvider.getMedication();
        request.setSearchTerm(null);
        assertThrows(DataNotAcceptableException.class, () -> medicationService.searchMedicationByName(request));

        request.setSearchTerm("DOLO");
        TestDataProvider.init();
        TestDataProvider.getStaticMock();
        when(medicationRepository.searchMedications(any(), any())).thenReturn(List.of(medication));
        ResponseListDTO<MedicationDTO> response = medicationService.searchMedicationByName(request);
        assertNotNull(response);
        TestDataProvider.cleanUp();
    }
}
