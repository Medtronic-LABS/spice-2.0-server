package com.mdtlabs.coreplatform.adminservice.controller;

import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramDetailsDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramRequestDTO;
import com.mdtlabs.coreplatform.adminservice.service.ProgramService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class ProgramControllerTest {

    @Mock
    private ProgramService programService;

    @InjectMocks
    private ProgramController programController;

    @Test
    void testCreateProgram() {
        ProgramRequestDTO programRequest = new ProgramRequestDTO();

        programController.createProgram(programRequest);
        verify(programService, times(1)).createProgram(any(ProgramRequestDTO.class));
    }

    @Test
    void testGetProgramById() {
        CommonRequestDTO requestDto = new CommonRequestDTO();
        ProgramDetailsDTO program = new ProgramDetailsDTO();

        when(programService.getProgramDetails(requestDto)).thenReturn(program);
        programController.getProgramById(requestDto);

        verify(programService, times(1)).getProgramDetails(any(CommonRequestDTO.class));
    }

    @Test
    void testGetPrograms() {
        SearchRequestDTO searchRequest = new SearchRequestDTO();
        ResponseListDTO<ProgramListDTO> response = new ResponseListDTO<>();

        when(programService.getAllPrograms(any(SearchRequestDTO.class))).thenReturn(response);

        programController.getPrograms(searchRequest);
        verify(programService, times(1)).getAllPrograms(any(SearchRequestDTO.class));
    }

    @Test
    void testRemoveProgram() {
        CommonRequestDTO requestDto = new CommonRequestDTO();
        programController.removeProgram(requestDto);

        verify(programService, times(1)).removeProgram(any(CommonRequestDTO.class));
    }

    @Test
    void testUpdateProgram() {
        ProgramRequestDTO programRequest = new ProgramRequestDTO();
        programController.updateProgram(programRequest);

        verify(programService, times(1)).updateProgram(any(ProgramRequestDTO.class));
    }

    @Test
    void testGetProgramsByHealthFacilityIds() {
        List<Long> siteIds = Arrays.asList(1L, 2L);
        List<Program> programs = new ArrayList<>();
        when(programService.getProgramsByHealthFacilityIds(siteIds)).thenReturn(programs);
        programController.getProgramsByHealthFacilityIds(siteIds);
        verify(programService, times(1)).getProgramsByHealthFacilityIds(siteIds);
    }
}