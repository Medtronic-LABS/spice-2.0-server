package com.mdtlabs.coreplatform.adminservice.service;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.mdtlabs.coreplatform.adminservice.model.dto.LabTestDTO;
import com.mdtlabs.coreplatform.adminservice.model.entity.LabTest;
import com.mdtlabs.coreplatform.adminservice.repository.LabTestRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.LabTestServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;

@ExtendWith(MockitoExtension.class)
class LabTestServiceTest {
    
     @InjectMocks
    LabTestServiceImpl labTestService;

    @Mock
    LabTestRepository labTestRepository;

    @Test
    void testcreateLabTest() {
        
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        LabTest labTest = TestDataProvider.getLabTest();

        when(labTestRepository.save(labTest)).thenReturn(labTest);
        LabTest response = labTestService.createLabTest(labTestDTO);
        assertNotNull(response);

        labTestDTO.setTenantId(null);
        assertThrows(DataNotAcceptableException.class, () -> labTestService.createLabTest(labTestDTO));

        LabTestDTO secondlabTestDTO = null;
        assertThrows(BadRequestException.class, () -> labTestService.createLabTest(secondlabTestDTO));
    }

    @Test
    void testvalidateLabTest() {
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        LabTest labTest = TestDataProvider.getLabTest();

        when(labTestRepository.findByCountryIdAndNameAndIsDeletedAndTenantId(labTestDTO.getCountryId(),
        labTestDTO.getName(), false, labTestDTO.getTenantId())).thenReturn(labTest);

        assertThrows(DataConflictException.class, () -> labTestService.validateLabTest(labTestDTO));

        labTestDTO.setId(1l);
        labTest.setId(2l);
        assertThrows(DataConflictException.class, () -> labTestService.validateLabTest(labTestDTO));
    }


    @Test
    void testgetAllLabTests() {

        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setLimit(10);

        List<LabTest> labTests = List.of(TestDataProvider.getLabTest());
        Page<LabTest> labTestsPage = new PageImpl<>(labTests);
        Pageable pageable = PageRequest.of(Constants.ZERO, requestDTO.getLimit(), Sort.by(Constants.UPDATED_AT)
                .descending());

        when(labTestRepository.getAllLabTests(requestDTO.getSearchTerm(), requestDTO.getCountryId(),
                requestDTO.getTenantId(), pageable)).thenReturn(labTestsPage);

        ResponseListDTO<LabTestDTO> actualResponse = labTestService.getAllLabTests(requestDTO);
        assertNotNull(actualResponse);
    }

    @Test
    void testRemoveLabTest() {
        //given
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        requestDTO.setId(TestConstants.ONE);
        LabTest labTest = TestDataProvider.getLabTest();

        //when
        when(labTestRepository.findByIdAndIsDeletedAndTenantId(requestDTO.getId(),
                Constants.BOOLEAN_FALSE, requestDTO.getTenantId())).thenReturn(labTest);
        when(labTestRepository.save(labTest)).thenReturn(labTest);

        //then
        boolean actualResponse = labTestService.removeLabTest(requestDTO);
        assertTrue(actualResponse);
    }

    @Test
    void testupdateLabTest() {
        LabTestDTO labTestDTO = TestDataProvider.getLabTestDTO();
        LabTest labTest = TestDataProvider.getLabTest();

        assertThrows(BadRequestException.class, () -> labTestService.updateLabTest(labTestDTO));
        labTestDTO.setId(1l);

        when(labTestRepository.findByIdAndIsDeletedAndTenantId(labTestDTO.getId(),
        Constants.BOOLEAN_FALSE, labTestDTO.getTenantId())).thenReturn(labTest);
        when(labTestRepository.save(labTest)).thenReturn(labTest);

        LabTest response = labTestService.updateLabTest(labTestDTO);
        assertNotNull(response);
    }

    @Test
    void getLabTestByIdTest() {
        SearchRequestDTO requestDTO = TestDataProvider.getSearchRequestDTO();
        when(labTestRepository.findByIdAndIsDeletedAndTenantId(requestDTO.getId(),
                Constants.BOOLEAN_FALSE, requestDTO.getTenantId())).thenReturn(TestDataProvider.getLabTest());
        LabTest response = labTestService.getLabTestById(requestDTO);
        assertNotNull(response);


        when(labTestRepository.findByIdAndIsDeletedAndTenantId(requestDTO.getId(),
                Constants.BOOLEAN_FALSE, requestDTO.getTenantId())).thenReturn(null);
        assertThrows(DataNotFoundException.class,()->labTestService.getLabTestById(requestDTO));


        requestDTO.setId(null);
        requestDTO.setTenantId(null);
        assertThrows(DataNotAcceptableException.class,()->labTestService.getLabTestById(requestDTO));

    }
}
