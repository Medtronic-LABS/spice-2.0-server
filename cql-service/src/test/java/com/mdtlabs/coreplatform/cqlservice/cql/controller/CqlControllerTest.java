package com.mdtlabs.coreplatform.cqlservice.cql.controller;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.cqlservice.constants.CqlConstants;
import com.mdtlabs.coreplatform.cqlservice.cql.service.CqlService;
import com.mdtlabs.coreplatform.cqlservice.model.dto.AncResultDTO;
import com.mdtlabs.coreplatform.cqlservice.model.dto.CqlRequestDTO;
import com.mdtlabs.coreplatform.cqlservice.util.TestConstants;
import com.mdtlabs.coreplatform.cqlservice.util.TestDataProvider;

@ExtendWith(MockitoExtension.class)
class CqlControllerTest {

    @Mock
    private CqlService cqlService;

    @InjectMocks
    private CqlController cqlController;

    @Test
    void evaluatePatientByLibraryAndExpressions() {
        //given
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        Map<String, Object> result = Map.of(CqlConstants.PATIENT, Constants.EMPTY);

        //when
        when(cqlService.evaluatePatientByLibraryAndExpressions(cqlRequestDTO)).thenReturn(result);

        //then
        ResponseEntity<Map<String, Object>> actual =
                cqlController.evaluatePatientByLibraryAndExpressions(cqlRequestDTO);
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
        assertFalse(actual.getBody().isEmpty());
    }

    @Test
    void evaluatePatientByLibrary() {
        //given
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        Map<String, Object> result = Map.of(CqlConstants.PATIENT, Constants.EMPTY);

        //when
        when(cqlService.evaluatePatientByLibrary(cqlRequestDTO)).thenReturn(result);

        //then
        ResponseEntity<Map<String, Object>> actual =
                cqlController.evaluatePatientByLibrary(cqlRequestDTO);
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
        assertFalse(actual.getBody().isEmpty());
    }

    @Test
    void evaluatePatient() {
        //given
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        Map<String, Object> result = Map.of(CqlConstants.PATIENT, Constants.EMPTY);

        //when
        when(cqlService.evaluatePatient(cqlRequestDTO)).thenReturn(result);

        //then
        ResponseEntity<Map<String, Object>> actual =
                cqlController.evaluatePatient(cqlRequestDTO);
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
        assertFalse(actual.getBody().isEmpty());
    }

    @Test
    void evaluateByEncounterId() {
        //given
        Map<String, Object> result = Map.of(CqlConstants.PATIENT, Constants.EMPTY);

        //when
        when(cqlService.evaluateByEncounter(TestConstants.ONE)).thenReturn(result);

        //then
        ResponseEntity<Map<String, Object>> actual =
                cqlController.evaluateByEncounterId(TestConstants.ONE);
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
        assertFalse(actual.getBody().isEmpty());
    }

    @Test
    void getResultByPatientId() {
        //given
        Map<String, Object> result = Map.of(CqlConstants.PATIENT, Constants.EMPTY);

        //when
        when(cqlService.getResultByPatientId(TestConstants.ONE)).thenReturn(result);

        //then
        ResponseEntity<Map<String, Object>> actual =
                cqlController.getResultByPatientId(TestConstants.ONE);
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
        assertFalse(actual.getBody().isEmpty());
    }

    @Test
    void getAncResultByPatientId() {
        //given
        Map<String, Object> result = Map.of(CqlConstants.PATIENT, Constants.EMPTY);

        //when
        when(cqlService.getAncResultByPatientId(TestConstants.ONE)).thenReturn(result);

        //then
        ResponseEntity<Map<String, Object>> actual =
                cqlController.getAncResultByPatientId(TestConstants.ONE);
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
        assertFalse(actual.getBody().isEmpty());
    }

    @Test
    void clearCqlAndTerminology() {
        //given
        String result = "Cleared Successfully";

        //when
        doNothing().when(cqlService).clearCqlAncTerminology();

        //then
        ResponseEntity<String> actual =
                cqlController.clearCqlAndTerminology();
        assertEquals(actual.getBody(), result);
        assertNotNull(actual.getBody());
    }

    @Test
    void getAncResultByVillages() {
        //given
        CqlRequestDTO requestDTO = new CqlRequestDTO();

        //when
        when(cqlService.getAncResultByVillageIdsAndCreatedAtDate(requestDTO)).thenReturn(List.of(new AncResultDTO()));

        //then
        ResponseEntity<List<AncResultDTO>> response = cqlController.getAncResultByVillages(requestDTO);
        Assertions.assertNotNull(response);
    }
}