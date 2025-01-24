package com.mdtlabs.coreplatform.cqlservice.cql.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IQuery;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;
import org.cqframework.cql.elm.execution.VersionedIdentifier;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.opencds.cqf.cql.engine.execution.EvaluationResult;
import org.opencds.cqf.cql.engine.execution.InMemoryLibraryLoader;
import org.opencds.cqf.cql.evaluator.CqlEvaluator;
import org.springframework.test.util.ReflectionTestUtils;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.cqlservice.constants.CqlConstants;
import com.mdtlabs.coreplatform.cqlservice.cql.repository.CqlResultRepository;
import com.mdtlabs.coreplatform.cqlservice.cql.service.impl.CqlServiceImpl;
import com.mdtlabs.coreplatform.cqlservice.model.dto.AncResultDTO;
import com.mdtlabs.coreplatform.cqlservice.model.dto.CqlRequestDTO;
import com.mdtlabs.coreplatform.cqlservice.model.entity.CqlResult;
import com.mdtlabs.coreplatform.cqlservice.util.CqlUtils;
import com.mdtlabs.coreplatform.cqlservice.util.TestConstants;
import com.mdtlabs.coreplatform.cqlservice.util.TestDataProvider;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CqlServiceTest {

    @Mock
    CqlResultRepository cqlResultRepository;

    @Spy
    @InjectMocks
    CqlServiceImpl cqlService;

    MockedConstruction<CqlEvaluator> cqlEvaluatorMockedConstruction;

    EvaluationResult evaluationResult;

    MockedStatic<CqlUtils> cqlUtilsMockedStatic;

    void setup() {
        //given
        Map<String, Object> results = new HashMap<>(Map.of(CqlConstants.PATIENT, Constants.EMPTY));
        ReflectionTestUtils.setField(cqlService, "cqlPath", TestConstants.FILE_PATH);
        ReflectionTestUtils.setField(cqlService, "valuesetPath", TestConstants.FILE_PATH);
        cqlUtilsMockedStatic = Mockito.mockStatic(CqlUtils.class);
        evaluationResult = Mockito.mock(EvaluationResult.class);
        InMemoryLibraryLoader inMemoryLibraryLoader = Mockito.mock(InMemoryLibraryLoader.class);
        ReflectionTestUtils.setField(evaluationResult, "expressionResults", results);

        //when
        cqlUtilsMockedStatic.when(CqlUtils::getFhirContext).thenReturn(FhirContext.forCached(FhirVersionEnum.R4));
        cqlUtilsMockedStatic.when(() -> CqlUtils.getCqlFiles(TestConstants.FILE_PATH)).thenReturn(new ArrayList<>());
        cqlUtilsMockedStatic.when(() -> CqlUtils.buildLibraryLoader(new ArrayList<>()))
                .thenReturn(inMemoryLibraryLoader);
        cqlUtilsMockedStatic.when(() -> CqlUtils.getTerminologyResourceBundle(TestConstants.FILE_PATH))
                .thenReturn(new Bundle());
        cqlUtilsMockedStatic.when(() -> CqlUtils.getIdFromHistoryUrl(any(String.class))).thenReturn(TestConstants.ONE);
        cqlUtilsMockedStatic.when(() -> CqlUtils.getIdFromReference(any(String.class))).thenReturn(TestConstants.ONE);
    }

    void cleanUp() {
        cqlUtilsMockedStatic.close();
    }

    @Test
    void evaluatePatientByLibraryWithException() {
        // given
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        cqlRequestDTO.setResourceBundle(CqlUtils.getFhirContext().newJsonParser().encodeResourceToString(new Bundle()));
        // then
        assertThrows(BadRequestException.class, () -> cqlService.evaluatePatientByLibrary(cqlRequestDTO));

        //given
        cqlRequestDTO.setResourceBundle(TestConstants.TEST_BUNDLE_PATIENTS);
        // then
        assertThrows(BadRequestException.class, () -> cqlService.evaluatePatientByLibrary(cqlRequestDTO));
    }

    @Test
    void evaluatePatientByLibraryAndExpressions() {
        // given
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        cqlRequestDTO.setResourceBundle(TestConstants.TEST_BUNDLE);
        setup();

        //when
        cqlEvaluatorMockedConstruction = Mockito.mockConstruction(CqlEvaluator.class, (mock, context) -> {
            when(mock.evaluate(any(VersionedIdentifier.class), anySet(), anyMap())).thenReturn(evaluationResult);
        });
        when(cqlResultRepository.findByResourceIdAndIsLatestTrue(any(String.class))).thenReturn(null);
        when(cqlResultRepository.save(any(CqlResult.class))).thenReturn(new CqlResult());

        //then
        Map<String, Object> actual = cqlService.evaluatePatientByLibraryAndExpressions(cqlRequestDTO);
        assertNotNull(actual);
        assertFalse(actual.isEmpty());
        cleanUp();
        cqlEvaluatorMockedConstruction.close();
    }

    @Test
    void evaluatePatient() {
        // given
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        cqlRequestDTO.setResourceBundle(TestConstants.TEST_BUNDLE);
        setup();

        //when
        cqlEvaluatorMockedConstruction = Mockito.mockConstruction(CqlEvaluator.class, (mock, context) -> {
            when(mock.evaluate(any(VersionedIdentifier.class), anyMap())).thenReturn(evaluationResult);
        });
        when(cqlResultRepository.findByResourceIdAndIsLatestTrue(any(String.class))).thenReturn(new CqlResult());
        when(cqlResultRepository.save(any(CqlResult.class))).thenReturn(new CqlResult());

        //then
        Map<String, Object> actual = cqlService.evaluatePatient(cqlRequestDTO);
        assertNotNull(actual);
        assertFalse(actual.isEmpty());
        cleanUp();
        cqlEvaluatorMockedConstruction.close();
    }

    @Test
    void getResultByPatientId() {
        // given
        String patientId = TestConstants.ONE;
        Map<String, Object> expectedResponse = new HashMap<>();
        // when
        when(cqlResultRepository.findByPatientIdAndIsLatestTrue(patientId)).thenReturn(TestDataProvider.getCqlResult());
        // then
        Map<String, Object> actualResponse = cqlService.getResultByPatientId(patientId);
        assertEquals(expectedResponse, actualResponse);

        // when
        when(cqlResultRepository.findByPatientIdAndIsLatestTrue(patientId)).thenReturn(null);
        // then
        assertThrows(DataNotFoundException.class, () -> cqlService.getResultByPatientId(patientId));
    }

    @Test
    void getAncResultByPatientId() {
        // given
        String patientId = TestConstants.ONE;
        Map<String, Object> expectedResponse = new HashMap<>();
        // when
        when(cqlResultRepository.findByPatientIdAndIsLatestTrue(patientId)).thenReturn(TestDataProvider.getCqlResult());
        // then
        Map<String, Object> actualResponse = cqlService.getAncResultByPatientId(patientId);
        assertEquals(expectedResponse, actualResponse);

        // when
        when(cqlResultRepository.findByPatientIdAndIsLatestTrue(patientId)).thenReturn(null);
        // then
        actualResponse = cqlService.getAncResultByPatientId(patientId);
        assertTrue(actualResponse.isEmpty());
    }

    @Test
    void evaluateByEncounter() {
        // given
        String encounterId = TestConstants.ONE;
        Map<String, Object> expectedResponse = new HashMap<>();
        ReflectionTestUtils.setField(cqlService, "fhirServerUrl", TestConstants.URL);
        IGenericClient iGenericClient = Mockito.mock(IGenericClient.class);
        String searchUrl = StringUtil.concatString(TestConstants.URL, CqlConstants.ENCOUNTER_SLASH, encounterId,
                CqlConstants.EVERYTHING_URL);
        Bundle bundle = TestDataProvider.getBundle();
        FhirContext fhirContext = Mockito.mock(FhirContext.class);
        IUntypedQuery untypedQuery = Mockito.mock(IUntypedQuery.class);
        IQuery iQuery = Mockito.mock(IQuery.class);
        IParser iParser = Mockito.mock(IParser.class);
        CqlRequestDTO requestDTO = TestDataProvider.getCqlRequestDTO();
        requestDTO.setExpressions(null);
        setup();
        TestDataProvider.init();

        // when
        TestDataProvider.getStaticMock();
        cqlUtilsMockedStatic.when(() -> CqlUtils.getFhirClient(TestConstants.URL, TestConstants.CLIENT_VALUE,
                TestConstants.TOKEN)).thenReturn(iGenericClient);
        when(iGenericClient.search()).thenReturn(untypedQuery);
        when(untypedQuery.byUrl(searchUrl)).thenReturn(iQuery);
        when(iQuery.returnBundle(Bundle.class)).thenReturn(iQuery);
        when(iQuery.execute()).thenReturn(bundle);
        when(fhirContext.newJsonParser()).thenReturn(iParser);
        when(iParser.encodeResourceToString(bundle)).thenReturn(TestConstants.TEST_BUNDLE);
        doReturn(expectedResponse).when(cqlService).evaluatePatientByLibrary(requestDTO);

        // then
        Map<String, Object> actualResponse = cqlService.evaluateByEncounter(encounterId);
        assertEquals(expectedResponse, actualResponse);
        TestDataProvider.cleanUp();
        cqlUtilsMockedStatic.close();
    }

    @Test
    void getAncResultByVillageIdsAndCreatedAtDate() {
        CqlRequestDTO cqlRequestDTO = TestDataProvider.getCqlRequestDTO();
        cqlRequestDTO.setVillageIds(List.of(TestConstants.ONE));
        cqlRequestDTO.setLastSyncTime(TestDataProvider.getDate());

        //when
        when(cqlResultRepository.findByVillageIdInAndUpdatedAtGreaterThanAndUpdatedAtLessThanEqualAndIsLatestTrue(cqlRequestDTO.getVillageIds(),
                cqlRequestDTO.getLastSyncTime(), cqlRequestDTO.getCurrentSyncTime())).thenReturn(List.of(TestDataProvider.getCqlResult()));
        when(cqlResultRepository.findByVillageIdInAndUpdatedAtLessThanEqualAndIsLatestTrue(cqlRequestDTO.getVillageIds(),
                cqlRequestDTO.getCurrentSyncTime())).thenReturn(List.of(TestDataProvider.getCqlResult()));

        //then
        List<AncResultDTO> response = cqlService.getAncResultByVillageIdsAndCreatedAtDate(cqlRequestDTO);
        Assertions.assertNotNull(response);
        cqlRequestDTO.setLastSyncTime(null);
        response = cqlService.getAncResultByVillageIdsAndCreatedAtDate(cqlRequestDTO);
        Assertions.assertNotNull(response);
    }
}