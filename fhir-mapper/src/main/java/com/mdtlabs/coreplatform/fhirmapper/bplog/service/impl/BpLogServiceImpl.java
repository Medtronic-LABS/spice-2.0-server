package com.mdtlabs.coreplatform.fhirmapper.bplog.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Observation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.bplog.service.BpLogService;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.MetaCodeConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.SymptomConverter;

/**
 * <p>
 * This class implements the BpLogService interface and contains actual business
 * logic to perform operations on BpLog entity.
 * </p>
 */
@Service
public class BpLogServiceImpl implements BpLogService {

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final SymptomConverter symptomConverter;

    @Autowired
    public BpLogServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil, SymptomConverter symptomConverter) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.symptomConverter = symptomConverter;
    }

    /**
     * {@inheritDoc}
     */
    public PatientBpLogsDTO getPatientBpLogsWithSymptoms(RequestDTO patientBpLogRequestData) {
        fhirUtils.initiateCodesMap();
        Bundle bundle = restApiUtil.getBatchRequest(constructBpLogObservationUrl(patientBpLogRequestData));
        return constructPatientBpLogDTO(patientBpLogRequestData, bundle);
    }

    /**
     * Construct the {@link PatientBpLogsDTO} which contains the patient bp log history.
     *
     * @param patientBpLogRequestData the request data to construct the patient bp log.
     * @param bundle the bundle containing the patient bp log history as a Observations entries
     * @return PatientBpLogsDTO
     */
    private PatientBpLogsDTO constructPatientBpLogDTO(RequestDTO patientBpLogRequestData, Bundle bundle) {
        PatientBpLogsDTO bpLogsDTO = new PatientBpLogsDTO();
        constructBpLogListDTO(bpLogsDTO, bundle);
        setLatestBpLogSymptom(bpLogsDTO, patientBpLogRequestData);
        setTotalCount(bpLogsDTO, patientBpLogRequestData);
        bpLogsDTO.setLimit(patientBpLogRequestData.getLimit());
        bpLogsDTO.setSkip(patientBpLogRequestData.getSkip());
        bpLogsDTO.setBpThreshold(Map.of(Constants.SYSTOLIC, Constants.BP_THRESHOLD_SYSTOLIC, Constants.DIASTOLIC,
                Constants.BP_THRESHOLD_DIASTOLIC));
        return bpLogsDTO;
    }

    /**
     * Set the total count of the patient blood pressure observations.
     *
     * @param bpLogsDTO bpLogsDTO to set the total count of the patient blood pressure
     * @param patientBpLogRequestData RequestDTO to get the total count of the patient blood pressure
     */
    private void setTotalCount(PatientBpLogsDTO bpLogsDTO, RequestDTO patientBpLogRequestData) {
        String url = StringUtil.concatString(Constants.OBSERVATION, Constants.QUESTION_MARK,
                String.format(Constants.IDENTIFIER_PARAM, StringUtil.concatString(
                        FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                        Constants.VERTICAL_BAR,
                Constants.OBSERVATION_BLOOD_PRESSURE)),
                Constants.AND, String.format(Constants.PERFORMER_PARAM, patientBpLogRequestData.getMemberId()), Constants.AND,
                Constants.TOTAL_COUNT_PARAM);
        Bundle bundle = restApiUtil.getBatchRequest(url);
        bpLogsDTO.setTotal(bundle.getTotal());
    }

    /**
     * Construct and set the latest symptom information.
     *
     * @param patientBpLogs patient bp log information
     * @param patientBpLogRequestData RequestDTO  to retrieve patient bp log information.
     */
    private void setLatestBpLogSymptom(PatientBpLogsDTO patientBpLogs, RequestDTO patientBpLogRequestData) {
        if (Constants.ZERO == patientBpLogRequestData.getSkip() && !patientBpLogs.getBpLogList().isEmpty()) {
            BpLogDTO bpLogList = null;
            if (patientBpLogRequestData.getSortOrder().equals(Constants.ONE)) {
                RequestDTO requestDTO = new RequestDTO();
                requestDTO.setSortOrder(Constants.MINUS_ONE);
                requestDTO.setLimit(Constants.ONE);
                requestDTO.setMemberId(patientBpLogRequestData.getMemberId());
                requestDTO.setSkip(Constants.ZERO);
                Bundle latestBundle = restApiUtil.getBatchRequest(constructBpLogObservationUrl(requestDTO));
                List<BpLogDTO> bpLogs = new ArrayList<>();
                if (Objects.nonNull(latestBundle.getEntry()) && !latestBundle.getEntry().isEmpty()) {
                    constructBpLog(latestBundle.getEntry().getFirst(), bpLogs);
                }
                bpLogList = bpLogs.getFirst();
            } else {
                bpLogList = patientBpLogs.getBpLogList().getFirst();
            }
            Bundle bundle = restApiUtil.getBatchRequest(
                    constructBpLogSymptomObservationUrl(bpLogList));
            patientBpLogs.setLatestBpLog(bpLogList);
            patientBpLogs.getLatestBpLog().setSymptoms(symptomConverter.getSymptomListByBundle(bundle, Boolean.TRUE));
        }
    }

    /**
     * Construct the patient bp log using bundle of observation
     *
     * @param patientBpLogs patient bp log observation for response
     * @param bundle the bundle of observation used to construct the patient bp log information.
     */
    private void constructBpLogListDTO(PatientBpLogsDTO patientBpLogs, Bundle bundle) {
        List<BpLogDTO> bpLogDTOS = new ArrayList<>();
        for (Bundle.BundleEntryComponent bundleEntryComponent : bundle.getEntry()) {
            constructBpLog(bundleEntryComponent, bpLogDTOS);
        }
        patientBpLogs.setBpLogList(bpLogDTOS);
    }

    /**
     * <p>
     * Construct and set the BP log information.
     * </p>
     *
     * @param bpLogDTOS  bp log information of the patient
     * @param bundleEntryComponent The saved bundle component of the observation.
     */
    private void constructBpLog(Bundle.BundleEntryComponent bundleEntryComponent, List<BpLogDTO> bpLogDTOS) {
        BpLogDTO bpLogDTO = new BpLogDTO();
        Observation observation = ((Observation) bundleEntryComponent.getResource());
        for (Observation.ObservationComponentComponent observationComponent : observation.getComponent()) {
            if (MetaCodeConstants.AVERAGE_SYSTOLIC_BLOOD_PRESSURE.equals(observationComponent.getCode().getText())) {
                bpLogDTO.setAvgSystolic(Double.parseDouble(observationComponent.getValueQuantity().getValue().toString()));
            } else if (MetaCodeConstants.AVERAGE_DIASTOLIC_BLOOD_PRESSURE.equals(observationComponent.getCode().getText())) {
                bpLogDTO.setAvgDiastolic(Double.parseDouble(observationComponent.getValueQuantity().getValue().toString()));
            }
        }
        if (Objects.nonNull(observation.getEffectiveDateTimeType())) {
            bpLogDTO.setBpTakenOn(observation.getEffectiveDateTimeType().getValue());
        }
        bpLogDTO.setCreatedAt(observation.getMeta().getLastUpdated());
        bpLogDTO.setEncounterId(fhirUtils.getIdFromReference(observation.getEncounter().getReference()));
        bpLogDTOS.add(bpLogDTO);
    }

    /**
     * Construct the bp log observation url based on the request data.
     *
     * @param patientBpLogRequestData the request data to construct the bp log observation url.
     * @return the bp log observation url.
     */
    private String constructBpLogObservationUrl(RequestDTO patientBpLogRequestData) {
        return StringUtil.concatString(Constants.OBSERVATION, Constants.QUESTION_MARK,
                String.format(Constants.IDENTIFIER_PARAM, StringUtil.concatString(
                        FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                        Constants.VERTICAL_BAR,
                        Constants.OBSERVATION_BLOOD_PRESSURE)),
                Constants.AND, String.format(Constants.PERFORMER_PARAM, patientBpLogRequestData.getMemberId()), Constants.AND,
                patientBpLogRequestData.getSortOrder() == -1 ? Constants.PARAM_SORT_DESC : Constants.PARAM_SORT_ASC,
                Constants.EQUAL_SYMBOL, Observation.SP_DATE,
                Constants.AND, String.format(Constants.COUNT_PARAM, patientBpLogRequestData.getLimit()),
                Constants.AND, String.format(Constants.OFFSET_PARAM, patientBpLogRequestData.getSkip()));
    }

    /**
     * Construct the latest observation url based on the request data.
     *
     * @param bpLogDTO construct the latest symptom observation url.
     * @return the bp log observation url.
     */
    private String constructBpLogSymptomObservationUrl(BpLogDTO bpLogDTO) {
        return StringUtil.concatString(Constants.OBSERVATION, Constants.QUESTION_MARK,
                String.format(Constants.IDENTIFIER_PARAM, StringUtil.concatString(FhirIdentifierConstants.OBSERVATION_TYPE_SYSTEM_URL,
                                Constants.VERTICAL_BAR, Constants.OBSERVATION_PATIENT_SYMPTOM)),
                Constants.AND, String.format(Constants.ENCOUNTER_PARAM, bpLogDTO.getEncounterId()));
    }
}
