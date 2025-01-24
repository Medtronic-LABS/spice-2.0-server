package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.anyLong;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.modelmapper.internal.InheritingConfiguration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.mdtlabs.coreplatform.adminservice.model.dto.Code;
import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestCustomizationDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTestCustomization;
import com.mdtlabs.coreplatform.adminservice.repository.LabTestCustomizationRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.LabTestCustomizationServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class LabTestCustomizationServiceTest {

    @InjectMocks
    private LabTestCustomizationServiceImpl labTestCustomizationService;

    @Mock
    private LabTestCustomizationRepository labTestCustomizationRepository;

    @Mock
    private ModelMapper mapper;

    private LabTestCustomizationDTO labTestCustomizationDTO = TestDataProvider.getLabTestCustomizationDTO();

    private LabTestCustomization labTestCustomization = TestDataProvider.getLabTestCustomization();

    @Test
    @DisplayName("createLabTestCustomizationWithNull")
    void createLabTestCustomizationTest() {
        //given
        //when
        labTestCustomization.setTestName(labTestCustomizationDTO.getTestName());
        when(labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                labTestCustomizationDTO.getTestName(),
                labTestCustomizationDTO.getCountryId())).thenReturn(null);
        when(mapper.map(labTestCustomizationDTO, LabTestCustomization.class)).thenReturn(labTestCustomization);
        when(labTestCustomizationRepository.save(any(LabTestCustomization.class))).thenReturn(labTestCustomization);
        when(mapper.map(labTestCustomization, LabTestCustomizationDTO.class)).thenReturn(labTestCustomizationDTO);
        //then
        LabTestCustomizationDTO response = labTestCustomizationService
                .createLabTestCustomization(labTestCustomizationDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(labTestCustomizationDTO.getTestName(), response.getTestName());
    }

    @Test
    @DisplayName("CreateLabTestCustomizationWithNonNull")
    void createLabTestCustomizationTestWithNonNull() {
        //given
        //when
        when(labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                labTestCustomizationDTO.getTestName(),
                labTestCustomizationDTO.getCountryId())).thenReturn(labTestCustomization);
        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> {
            labTestCustomizationService
                   .createLabTestCustomization(labTestCustomizationDTO);
        });
    }

    @Test
    @DisplayName("getLabTestCustomizationWithNull")
    void getLabTestCustomizationWithNullTest() {
        //given
        SearchRequestDTO request = new SearchRequestDTO();
        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> {
            labTestCustomizationService.getLabTestCustomization(request);
        });

        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setName(TestConstants.NAME);
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        when(labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                searchRequestDTO.getName(), 1l)).thenReturn(null);
        //then
        LabTestCustomizationDTO response = labTestCustomizationService.getLabTestCustomization(searchRequestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(response);
    }

    @Test
    @DisplayName("getLabTestCustomization")
    void getLabTestCustomizationTest() {
        //given
        labTestCustomization.setTestName(labTestCustomizationDTO.getTestName());
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setName(TestConstants.NAME);
        searchRequestDTO.setCountryId(1L);
        //when
        when(labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                searchRequestDTO.getName(), searchRequestDTO.getCountryId())).thenReturn(labTestCustomization);
        when(mapper.map(labTestCustomization, LabTestCustomizationDTO.class)).thenReturn(labTestCustomizationDTO);
        //then
        LabTestCustomizationDTO response = labTestCustomizationService.getLabTestCustomization(searchRequestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(labTestCustomizationDTO.getTestName(), response.getTestName());
    }

    @Test
    @DisplayName("updateLabTestCustomizationWithNull")
    void updateLabTestCustomizationWithNullTest() {
        //given
        LabTestCustomizationDTO request = new LabTestCustomizationDTO();
        //when
        Assertions.assertThrows(DataNotFoundException.class, () -> {
            labTestCustomizationService.updateLabTestCustomization(request);
        });

        labTestCustomizationDTO.setId(1l);
        //when
        when(labTestCustomizationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(1l)).thenReturn(null);
        //then
        Assertions.assertThrows(DataNotAcceptableException.class, () -> {
            labTestCustomizationService.updateLabTestCustomization(labTestCustomizationDTO);
        });

        //given
        Code code = new Code();
        code.setUrl("snomed.com");
        code.setCode("1234ew");
        labTestCustomizationDTO.setCodeDetails(code);
        //when
        when(labTestCustomizationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(1l)).thenReturn(labTestCustomization);
        when(mapper.getConfiguration()).thenReturn(new InheritingConfiguration());
        when(mapper.map(labTestCustomizationDTO, LabTestCustomization.class)).thenReturn(labTestCustomization);
        when(labTestCustomizationRepository.save(labTestCustomization)).thenReturn(labTestCustomization);
        when(mapper.map(labTestCustomization, LabTestCustomizationDTO.class)).thenReturn(labTestCustomizationDTO);
        //then
        LabTestCustomizationDTO response = labTestCustomizationService.updateLabTestCustomization(labTestCustomizationDTO);
        Assertions.assertEquals(labTestCustomizationDTO, response);
        Assertions.assertEquals(labTestCustomizationDTO.getId(), response.getId());
        Assertions.assertEquals(labTestCustomizationDTO.getTestName(), response.getTestName());
    }

    @Test
    @DisplayName("deleteLabTestCustomizationTest")
    void deleteLabTestCustomizationTest() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> {
            labTestCustomizationService.deleteLabTestCustomization(requestDTO);
        });

        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        //when
        when(labTestCustomizationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(searchRequestDTO.getId()))
                .thenReturn(null);
        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> {
            labTestCustomizationService.deleteLabTestCustomization(searchRequestDTO);
        });

        //given
        SearchRequestDTO searchRequest = TestDataProvider.getSearchRequestDTO();
        //when
        when(labTestCustomizationRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(searchRequestDTO.getId()))
                .thenReturn(labTestCustomization);
        when(labTestCustomizationRepository.save(labTestCustomization)).thenReturn(labTestCustomization);
        //then
        labTestCustomizationService.deleteLabTestCustomization(searchRequest);
        verify(labTestCustomizationRepository).save(labTestCustomization);
    }

    @Test
    @DisplayName("getLabTestCustomizationByUniqueName")
    void getLabTestCustomizationByUniqueNameTest() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        labTestCustomization.setTestName(labTestCustomizationDTO.getTestName());
        //then
        Assertions.assertThrows(DataNotFoundException.class, () -> {
            labTestCustomizationService.getLabTestCustomizationByUniqueName(requestDTO);
        });

        //given
        SearchRequestDTO searchRequestDTO = TestDataProvider.getSearchRequestDTO();
        searchRequestDTO.setName(TestConstants.NAME);
        TestDataProvider.init();
        //when
        TestDataProvider.getStaticMock();
        when(labTestCustomizationRepository.findByUniqueNameAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                searchRequestDTO.getName(), searchRequestDTO.getCountryId())).thenReturn(null);
        //then
        LabTestCustomizationDTO response = labTestCustomizationService.getLabTestCustomizationByUniqueName(searchRequestDTO);
        TestDataProvider.cleanUp();
        Assertions.assertNull(response);

        //given
        SearchRequestDTO searchRequest = TestDataProvider.getSearchRequestDTO();
        searchRequest.setName(TestConstants.NAME);
        searchRequest.setCountryId(1l);
        //when
        when(labTestCustomizationRepository.findByUniqueNameAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                searchRequest.getName(), searchRequest.getCountryId())).thenReturn(labTestCustomization);
        when(mapper.map(labTestCustomization, LabTestCustomizationDTO.class)).thenReturn(labTestCustomizationDTO);
        //then
        LabTestCustomizationDTO result = labTestCustomizationService.getLabTestCustomizationByUniqueName(searchRequest);
        Assertions.assertNotNull(result);
        Assertions.assertEquals(labTestCustomizationDTO.getTestName(), result.getTestName());
    }

    @Test
    @DisplayName("getLabTestCustomizationsByCountryId")
    void getLabTestCustomizationsByCountryIdTest() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        TestDataProvider.init();
        Pageable pageable = PageRequest.of(0,10, Sort.by(new String[]{Constants.UPDATED_AT}).descending());
        //when
        TestDataProvider.getStaticMock();
        when(labTestCustomizationRepository.findAllByName(null, requestDTO.getCountryId(), pageable))
               .thenReturn(Page.empty());
        //then
        TestDataProvider.cleanUp();

        //given
        SearchRequestDTO searchRequest = TestDataProvider.getSearchRequestDTO();
        searchRequest.setSearchTerm(TestConstants.NAME);
        searchRequest.setCountryId(1L);
        Page<LabTestCustomization> page = new PageImpl<>(List.of(labTestCustomization));
        ///when
        when(labTestCustomizationRepository.findAllByName(anyString(), anyLong(), any()))
                .thenReturn(page);
        when(mapper.map(labTestCustomization, LabTestCustomizationDTO.class)).thenReturn(labTestCustomizationDTO);
        //then
        ResponseListDTO<LabTestCustomizationDTO> result = labTestCustomizationService.listLabTestCustomization(searchRequest);
        Assertions.assertNotNull(result);
    }

    @Test
    @DisplayName("validateLabTestCustomization")
    void validateLabTestCustomizationTest() {
        //given
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        //then
        Assertions.assertThrows(SpiceValidation.class, () -> {
            labTestCustomizationService.validateLabTestCustomization(searchRequestDTO);
        });

        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO(TestConstants.NAME, TestConstants.ONE);
        //when
        when(labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getName(), requestDTO.getCountryId())).thenReturn(labTestCustomization);
        //then
        Assertions.assertThrows(SpiceValidation.class, () -> {
            labTestCustomizationService.validateLabTestCustomization(requestDTO);
        });

        //given
        SearchRequestDTO request = TestDataProvider.getSearchRequestDTO();
        request.setName(TestConstants.NAME);
        request.setCountryId(1L);
        //when
        when(labTestCustomizationRepository.findByTestNameIgnoreCaseAndCountryIdAndIsDeletedFalseAndIsActiveTrue(
                request.getName(), request.getCountryId())).thenReturn(null);
        //then
        Boolean response = labTestCustomizationService.validateLabTestCustomization(request);
        Assertions.assertTrue(response);
        Assertions.assertEquals(Boolean.TRUE, response);
    }
}
