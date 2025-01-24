package com.mdtlabs.coreplatform.adminservice.service.impl;

import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramDetailsDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.ProgramRepository;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ProgramServiceImplTest {

    @Mock
    private ProgramRepository programRepository;

    @Mock
    private HealthFacilityService healthFacilityService;

    @InjectMocks
    private ProgramServiceImpl programService;

    private ProgramRequestDTO programRequestDTO;

    @BeforeEach
    void setUp() {
        programRequestDTO = new ProgramRequestDTO();
        programRequestDTO.setName("Test Program");
        programRequestDTO.setTenantId(1L);
        programRequestDTO.setHealthFacilities(Set.of(1L, 2L));
        Country country = new Country();
        country.setId(123L);
        programRequestDTO.setCountry(country);
    }

    @Test
    void testCreateProgram_Success() {
        Country country = new Country();
        country.setId(123L);
        when(programRepository.findByNameAndTenantIdAndIsDeleted(anyString(), anyLong(), eq(false)))
                .thenReturn(null);  // No existing program
        when(programRepository.save(any(Program.class)))
                .thenReturn(new Program("Test Program", 1L, country));
        when(healthFacilityService.getHealthFacilitiesByIds(programRequestDTO.getHealthFacilities())).thenReturn(Set.of(new HealthFacility()));
        Program result = programService.createProgram(programRequestDTO);

        assertNotNull(result);
        assertEquals("Test Program", result.getName());
        verify(programRepository, times(1)).save(any(Program.class));
    }

    @Test
    void testCreateProgram_Conflict() {
        when(programRepository.findByNameAndTenantIdAndIsDeleted(anyString(), anyLong(), eq(false)))
                .thenReturn(new Program());

        assertThrows(DataConflictException.class, () -> programService.createProgram(programRequestDTO));
    }

    @Test
    void testGetProgramById_Success() {
        Country country = new Country();
        country.setId(123L);
        Program program = new Program("Test Program", 1L, country);
        when(programRepository.findByIdAndIsDeletedAndTenantId(anyLong(), eq(false), anyLong()))
                .thenReturn(program);

        Program result = programService.getProgramById(1L, 1L, false);

        assertNotNull(result);
        assertEquals("Test Program", result.getName());
    }

    @Test
    void testGetProgramById_NotFound() {
        when(programRepository.findByIdAndIsDeletedAndTenantId(anyLong(), eq(false), anyLong()))
                .thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> programService.getProgramById(1L, 1L, false));
    }

    @Test
    void testRemoveProgram_Success() {
        Country country = new Country();
        country.setId(123L);
        Program program = new Program("Test Program", 1L, country);
        when(programRepository.findByIdAndIsDeletedAndTenantId(anyLong(), eq(false), anyLong()))
                .thenReturn(program);
        when(programRepository.save(any(Program.class)))
                .thenReturn(program);

        CommonRequestDTO requestDto = new CommonRequestDTO();
        requestDto.setId(1L);
        requestDto.setTenantId(1L);

        boolean result = programService.removeProgram(requestDto);

        assertTrue(result);
        assertTrue(program.isDeleted());
        verify(programRepository, times(1)).save(program);
    }

    @Test
    void testGetProgramDetails_ThrowsDataNotAcceptableWhenTenantIdIsNull() {
        CommonRequestDTO requestDto = new CommonRequestDTO();
        assertThrows(DataNotAcceptableException.class, () -> programService.getProgramDetails(requestDto));
    }


    @Test
    void testGetProgramDetails_Success() {
        Country country = new Country();
        country.setId(123L);
        Program program = new Program("Test Program", 1L, country);
        program.setHealthFacilities(Set.of(new HealthFacility()));
        when(programRepository.findByIdAndIsDeletedAndTenantId(any(), eq(Boolean.FALSE), any())).thenReturn(program);

        CommonRequestDTO requestDto = new CommonRequestDTO();
        requestDto.setId(1L);
        requestDto.setTenantId(1L);

        ProgramDetailsDTO result = programService.getProgramDetails(requestDto);

        assertNotNull(result);
        assertEquals("Test Program", result.getName());
    }

    @Test
    void testUpdateProgram_Success() {
        Country country = new Country();
        country.setId(123L);
        Program existingProgram = new Program("Test Program", 1L, country);
        when(programRepository.findByIdAndIsDeleted(anyLong(), eq(false)))
                .thenReturn(existingProgram);
        when(programRepository.save(any(Program.class)))
                .thenReturn(existingProgram);

        ProgramRequestDTO updatedProgram = new ProgramRequestDTO();
        updatedProgram.setId(1L);
        updatedProgram.setHealthFacilities(Set.of(1L, 2L));
        updatedProgram.setDeletedHealthFacilities(Set.of(1L, 2L));
        when(healthFacilityService.getHealthFacilitiesByIds(programRequestDTO.getHealthFacilities())).thenReturn(Set.of(new HealthFacility()));

        Program result = programService.updateProgram(updatedProgram);

        assertNotNull(result);
        assertEquals(1L, result.getTenantId());
    }

    @Test
    void testUpdateProgram_NotFound() {

        ProgramRequestDTO updatedProgram = new ProgramRequestDTO();
        updatedProgram.setId(1L);
        when(programRepository.findByIdAndIsDeleted(1, false)).thenReturn(null);

        assertThrows(DataNotFoundException.class, () -> programService.updateProgram(updatedProgram));
    }

    @Test
    void testUpdateProgramNullThrowsBadRequest() {
        assertThrows(BadRequestException.class, () -> programService.updateProgram(null));
    }

    @Test
    void testUpdateProgramWithNameThrowsDataNotAcceptable() {
        assertThrows(DataNotAcceptableException.class, () -> programService.updateProgram(programRequestDTO));
    }


    @Test
    void testGetAllProgramsThrowsDataNotAcceptableWhenTenantIdIsNull() {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        searchRequestDTO.setTenantId(null);
        searchRequestDTO.setCountryId(1L);
        searchRequestDTO.setSearchTerm("Test");

       assertThrows(DataNotAcceptableException.class, () -> programService.getAllPrograms(searchRequestDTO));
    }

    @Test
    void testGetAllPrograms_Success() {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        searchRequestDTO.setTenantId(1L);
        searchRequestDTO.setCountryId(1L);
        searchRequestDTO.setSearchTerm("Test");

        Page<Program> programs = mock(Page.class);

        when(programRepository.getAllProgram(anyString(), anyLong(), anyLong(), any(Pageable.class)))
                .thenReturn(programs);

        ResponseListDTO<ProgramListDTO> result = programService.getAllPrograms(searchRequestDTO);

        assertNotNull(result);
        assertEquals(0, result.getData().size());  // Adjust according to mock Page setup
    }

    @Test
    void testGetProgramsByHealthFacilityIds_Success() {
        List<Long> healthFacilityIds = Arrays.asList(1L, 2L);

        when(programRepository.findProgramsByHealthFacilityIds(healthFacilityIds))
                .thenReturn(List.of(new Program()));

        List<Program> result = programService.getProgramsByHealthFacilityIds(healthFacilityIds);

        assertNotNull(result);
        assertEquals(1, result.size());
    }
}