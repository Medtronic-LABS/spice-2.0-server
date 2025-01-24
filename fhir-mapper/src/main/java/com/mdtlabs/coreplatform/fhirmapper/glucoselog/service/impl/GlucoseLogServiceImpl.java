package com.mdtlabs.coreplatform.fhirmapper.glucoselog.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Observation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.BloodGlucoseConverter;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;
import com.mdtlabs.coreplatform.fhirmapper.glucoselog.service.GlucoseLogService;

/**
 * This class implements the GlucoseLogService interface and contains actual
 * business logic to perform operations on GlucoseLog entity.
 *
 * @author Tamilmani
 * @since Sep 05, 2024
 */
@Service
public class GlucoseLogServiceImpl implements GlucoseLogService {

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final BloodGlucoseConverter bloodGlucoseConverter;

    private final SymptomConverter symptomConverter;

    @Autowired
    public GlucoseLogServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil,
                                 BloodGlucoseConverter bloodGlucoseConverter, SymptomConverter symptomConverter) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.bloodGlucoseConverter = bloodGlucoseConverter;
        this.symptomConverter = symptomConverter;
    }

    /**
     * {@inheritDoc}
     */
    public PatientGlucoseLogDTO getPatientGlucoseLogsWithSymptoms(RequestDTO patientGlucoseLogRequestData) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = restApiUtil.getBatchRequest(constructGlucoseLogObservationUrl(patientGlucoseLogRequestData));
        return constructPatientGlucoseLogDTO(patientGlucoseLogRequestData, bundle);
    }

    /**
     * {@inheritDoc}
     */
    public List<GlucoseLogDTO> getPatientGlucoseLogs(RequestDTO requestDTO) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = restApiUtil.getBatchRequest(constructGlucoseLogObservationUrl(requestDTO));
        List<GlucoseLogDTO> glucoseLogDTOS = new ArrayList<>();
        for (Bundle.BundleEntryComponent bundleEntryComponent : bundle.getEntry()) {
            Observation observation = ((Observation) bundleEntryComponent.getResource());
            glucoseLogDTOS.add(bloodGlucoseConverter.convertObservationToGlucoseLogDTO(observation));
        }
        return glucoseLogDTOS;
    }

    /**
     * Construct the {@link PatientGlucoseLogDTO} which contains the patient glucose log history.
     *
     * @param patientGlucoseLogRequestData the request data to construct the patient glucose log.
     * @param bundle                       the bundle containing the patient glucose log history as a Observations entries
     * @return PatientGlucoseLogDTO
     */
    private PatientGlucoseLogDTO constructPatientGlucoseLogDTO(RequestDTO patientGlucoseLogRequestData, Bundle bundle) {
        PatientGlucoseLogDTO glucoseLogs = new PatientGlucoseLogDTO();
        constructGlucoseLogListDTO(glucoseLogs, bundle);
        setLatestGlucoseLogSymptom(glucoseLogs, patientGlucoseLogRequestData);
        setTotalCount(glucoseLogs, patientGlucoseLogRequestData);
        glucoseLogs.setLimit(patientGlucoseLogRequestData.getLimit());
        glucoseLogs.setSkip(patientGlucoseLogRequestData.getSkip());
        List<Map<String, Object>> thresholds = new ArrayList<>();
        thresholds.add(Map.of(Constants.FBS, Constants.FBS_MMOL_L, Constants.RBS, Constants.RBS_MMOL_L, Constants.UNIT,
                Constants.GLUCOSE_UNIT_MMOL_L));
        thresholds.add(Map.of(Constants.FBS, Constants.FBS_MG_DL, Constants.RBS, Constants.RBS_MG_DL, Constants.UNIT,
                Constants.GLUCOSE_UNIT_MG_DL));
        glucoseLogs.setGlucoseThreshold(thresholds);
        return glucoseLogs;
    }

    /**
     * Set the total count of the patient blood glucose observations.
     *
     * @param glucoseLogDTO                PatientGlucoseLogDTO to set the total count of the patient blood glucose
     * @param patientGlucoseLogRequestData RequestDTO to get the total count of the patient blood glucose
     */
    private void setTotalCount(PatientGlucoseLogDTO glucoseLogDTO, RequestDTO patientGlucoseLogRequestData) {
        String url = StringUtil.concatString(Constants.OBSERVATION, Constants.QUESTION_MARK,
                String.format(Constants.IDENTIFIER_PARAM, StringUtil.concatString(
                        FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                        Constants.VERTICAL_BAR,
                        Constants.OBSERVATION_BLOOD_GLUCOSE)),
                Constants.AND, String.format(Constants.PERFORMER_PARAM, patientGlucoseLogRequestData.getMemberId()),
                Constants.AND, Constants.TOTAL_COUNT_PARAM);
        Bundle bundle = restApiUtil.getBatchRequest(url);
        glucoseLogDTO.setTotal(bundle.getTotal());
    }

    /**
     * Construct and set the latest symptom information.
     *
     * @param patientGlucoseLogs           patient glucose log information
     * @param patientGlucoseLogRequestData RequestDTO  to retrieve patient glucose log information.
     */
    private void setLatestGlucoseLogSymptom(PatientGlucoseLogDTO patientGlucoseLogs,
                                            RequestDTO patientGlucoseLogRequestData) {
        if (Constants.ZERO == patientGlucoseLogRequestData.getSkip()
                && !patientGlucoseLogs.getGlucoseLogList().isEmpty()) {
            GlucoseLogDTO glucoseLogDTO = null;
            if (patientGlucoseLogRequestData.getSortOrder().equals(Constants.ONE)) {
                RequestDTO requestDTO = new RequestDTO();
                requestDTO.setSortOrder(Constants.MINUS_ONE);
                requestDTO.setLimit(Constants.ONE);
                requestDTO.setMemberId(patientGlucoseLogRequestData.getMemberId());
                requestDTO.setSkip(Constants.ZERO);
                Bundle latestBundle = restApiUtil.getBatchRequest(constructGlucoseLogObservationUrl(requestDTO));
                List<GlucoseLogDTO> bgLogs = new ArrayList<>();
                if (Objects.nonNull(latestBundle.getEntry()) && !latestBundle.getEntry().isEmpty()) {
                    Observation observation = ((Observation) latestBundle.getEntry().getFirst().getResource());
                    bgLogs.add(bloodGlucoseConverter.convertObservationToGlucoseLogDTO(observation));
                }
                glucoseLogDTO = bgLogs.getFirst();
            } else {
                glucoseLogDTO = patientGlucoseLogs.getGlucoseLogList().getFirst();
            }
            Bundle bundle = restApiUtil.getBatchRequest(
                    constructGlucoseLogSymptomObservationUrl(patientGlucoseLogRequestData.getMemberId()));
            patientGlucoseLogs.setLatestGlucoseLog(setLatestGlucoseLog(patientGlucoseLogs.getGlucoseLogList(), glucoseLogDTO));
            patientGlucoseLogs.getLatestGlucoseLog().setSymptoms(symptomConverter.getSymptomListByBundle(bundle));
        }
    }

    private GlucoseLogDTO setLatestGlucoseLog(List<GlucoseLogDTO> glucoseLogDTOList, GlucoseLogDTO latestGlucoseLogDTO) {
        Double glucoseValue = latestGlucoseLogDTO.getGlucoseValue();
        if (Objects.isNull(glucoseValue)) {
            for (GlucoseLogDTO glucoseLog : glucoseLogDTOList) {
                if (Objects.nonNull(glucoseLog.getGlucoseValue())) {
                    latestGlucoseLogDTO.setGlucoseType(glucoseLog.getGlucoseType());
                    latestGlucoseLogDTO.setGlucoseUnit(glucoseLog.getGlucoseUnit());
                    latestGlucoseLogDTO.setGlucoseValue(glucoseLog.getGlucoseValue());
                    latestGlucoseLogDTO.setGlucoseDateTime(glucoseLog.getGlucoseDateTime());
                    break;
                }
            }
        }
        return latestGlucoseLogDTO;
    }

    /**
     * Construct the patient glucose log using bundle of observation
     *
     * @param patientGlucoseLogs patient glucose log observation for response
     * @param bundle the bundle of observation used to construct the patient glucose log information.
     */
    private void constructGlucoseLogListDTO(PatientGlucoseLogDTO patientGlucoseLogs, Bundle bundle) {
        List<GlucoseLogDTO> glucoseLogDTOS = new ArrayList<>();
        for (Bundle.BundleEntryComponent bundleEntryComponent : bundle.getEntry()) {
            Observation observation = ((Observation) bundleEntryComponent.getResource());
            glucoseLogDTOS.add(bloodGlucoseConverter.convertObservationToGlucoseLogDTO(observation));
        }
        patientGlucoseLogs.setGlucoseLogList(glucoseLogDTOS);
    }

    /**
     * Construct the glucose log observation url based on the request data.
     *
     * @param patientGlucoseLogRequestData the request data to construct the glucose log observation url.
     * @return the glucose log observation url.
     */
    private String constructGlucoseLogObservationUrl(RequestDTO patientGlucoseLogRequestData) {
        return StringUtil.concatString(Constants.OBSERVATION, Constants.QUESTION_MARK,
                String.format(Constants.IDENTIFIER_PARAM, StringUtil.concatString(
                        FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                        Constants.VERTICAL_BAR,
                        Constants.OBSERVATION_BLOOD_GLUCOSE)),
                Constants.AND, String.format(Constants.PERFORMER_PARAM, patientGlucoseLogRequestData.getMemberId()),
                Constants.AND, patientGlucoseLogRequestData.getSortOrder() == -1 ? Constants.PARAM_SORT_DESC : Constants.PARAM_SORT_ASC,
                Constants.EQUAL_SYMBOL, Observation.SP_DATE, Constants.AND,
                String.format(Constants.COUNT_PARAM, patientGlucoseLogRequestData.getLimit()),
                Constants.AND, String.format(Constants.OFFSET_PARAM, patientGlucoseLogRequestData.getSkip()),
                Constants.AND, String.format(Constants.COMPONENT_CODE_TEXT_PARAM,String.join(Constants.COMMA,
                        MetaCodeConstants.HBA1C, FhirConstants.FBS, FhirConstants.RBS)));
    }

    /**
     * Construct the latest observation url based on the request data.
     *
     * @param memberId .
     * @return the symptom observation url.
     */
    private String constructGlucoseLogSymptomObservationUrl(String memberId) {
        return StringUtil.concatString(Constants.OBSERVATION, Constants.QUESTION_MARK,
                String.format(Constants.IDENTIFIER_PARAM, StringUtil.concatString(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                        Constants.VERTICAL_BAR, Constants.OBSERVATION_PATIENT_SYMPTOM)),
                Constants.AND, String.format(Constants.PERFORMER_PARAM, memberId),
                Constants.AND, Constants.DATE_PARAM_ASC, Constants.AND,
                String.format(Constants.COUNT_PARAM, Constants.ONE));
    }
}
