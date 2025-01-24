package com.mdtlabs.coreplatform.fhirmapper.common;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.glucoselog.service.GlucoseLogService;
import com.mdtlabs.coreplatform.fhirmapper.labtest.service.InvestigationService;
import com.mdtlabs.coreplatform.fhirmapper.prescription.service.PrescriptionRequestService;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * <p>
 *     This class used to calculate the intensification lab test based on the criteria.
 * <p/>
 *
 */
@Service
public class IntensificationAlgorithm {

    @Value("${app.labTestIntensificationList}")
    private String labTestIntensificationList;

    @Value("${app.prescriptionPredictionDays}")
    private String predictionDays;

    private final InvestigationService investigationService;

    private final GlucoseLogService glucoseLogService;

    private final PrescriptionRequestService prescriptionRequestService;

    public IntensificationAlgorithm(InvestigationService investigationService,
                                    GlucoseLogService glucoseLogService,
                                    PrescriptionRequestService prescriptionRequestService) {
        this.investigationService = investigationService;
        this.glucoseLogService = glucoseLogService;
        this.prescriptionRequestService = prescriptionRequestService;
    }


    /**
     * <p>
     * Get lab test details by request data.
     * </p>
     *
     * @param requestData - Contains request dto
     * @return {@link Map } - Contains the list of patient intensification lab test details.
     */
    public Map<String, List<LabTestDTO>> getIntensificationDetails(RequestDTO requestData) {
        if (Objects.isNull(requestData.getPatientReference())) {
            throw new DataNotAcceptableException(1009);
        }
        Map<String, List<LabTestDTO>> intensificationResult = new HashMap<>();
        List<String> labTestNames = Arrays.asList(labTestIntensificationList.split(Constants.COMMA));
        List<LabTestDTO> patientInvestigations = investigationService.getInvestigationByNames(requestData, labTestNames);
        validateAndRemoveInvestigations(patientInvestigations, Constants.HBA1C);
        validateAndRemoveInvestigations(patientInvestigations, Constants.LIPID_PROFILE);
        validateAndRemoveInvestigations(patientInvestigations, Constants.RENAL_FUNCTION_TEST);
        patientInvestigations.forEach(patientInvestigation -> validateIntensificationCriteria(patientInvestigation,
                intensificationResult));
        return intensificationResult;
    }

    /**
     * Validate the data by checking lab test date and return 3 records
     * @param patientLabTest - Contains the patient lab test information.
     * @param intensificationResult - Contains the list of intensification lab test details.
     */
    private void validateIntensificationCriteria(LabTestDTO patientLabTest, Map<String, List<LabTestDTO>> intensificationResult) {
        SimpleDateFormat formatter = new SimpleDateFormat(Constants.DATE_FORMAT_YYYY_MM_DD);
        String labTestName = patientLabTest.getTestName();
        try {
            Integer dateDiff = DateUtil.getCalendarDiff(formatter.parse(formatter.format(patientLabTest.getRecommendedOn())), formatter.parse(formatter.format(new Date()))) + 1;
            if (Objects.isNull(intensificationResult.get(labTestName))) {
                if (Objects.nonNull(Constants.INVESTIGATION_INTENSIFICATION_CRITERIA.get(labTestName)) &&
                        dateDiff >= Constants.INVESTIGATION_INTENSIFICATION_CRITERIA.get(labTestName)) {
                    intensificationResult.put(labTestName, new ArrayList<>(Arrays.asList(patientLabTest)));
                }
            } else if (intensificationResult.get(labTestName).size() < Constants.THREE) {
                intensificationResult.get(labTestName).add(patientLabTest);
            }
        } catch (ParseException e) {
            Logger.logError(e);
        }
    }

    /**
     * Remove the labTests if criteria does not match
     * @param patientInvestigations - Contains LabTests
     * @param labTestName - Name to check the criteria
     */
    private void validateAndRemoveInvestigations(List<LabTestDTO> patientInvestigations, String labTestName) {
        SimpleDateFormat formatter = new SimpleDateFormat(Constants.DATE_FORMAT_YYYY_MM_DD);
        long labTestCount = patientInvestigations.stream()
                .filter(patientInvestigation -> patientInvestigation.getTestName().equals(labTestName))
                .filter(patientInvestigation -> {
                    try {
                        return DateUtil.getCalendarDiff(formatter.parse(formatter.format(patientInvestigation.getRecommendedOn())),
                                formatter.parse(formatter.format(new Date()))) + 1 < Constants.INVESTIGATION_INTENSIFICATION_CRITERIA.get(labTestName);
                    } catch (ParseException e) {
                        Logger.logError(e);
                    }
                    return false;
                })
                .count();
        if(labTestCount > 0) {
            patientInvestigations.removeIf(patientInvestigation -> patientInvestigation.getTestName().equals(labTestName));
        }
    }

    /**
     * Gets patient prescription suggestion using given member id
     *
     * @param memberId  member id of the patient
     * @return PrescriptionPredictionDTO entity
     */
    public PrescriptionPredictionDTO getMedicationSuggestion(String memberId) {
        PrescriptionPredictionDTO prescriptionPrediction = null;
        List<GlucoseLogDTO> recentBGLogs = this.getRecentGlucoseLog(memberId);
        List<PrescriptionDTO> prescriptionResults = new ArrayList<>();
        if (!recentBGLogs.isEmpty()) {
            GlucoseLogDTO latestGlucoseLog = recentBGLogs.get(0);
            if ((Constants.RBS.equals(latestGlucoseLog.getGlucoseType())
                    && 10 < latestGlucoseLog.getGlucoseValue())
                    || (Constants.FBS.equals(latestGlucoseLog.getGlucoseType())
                    && 7 < latestGlucoseLog.getGlucoseValue())
                    || (!Objects.isNull(latestGlucoseLog.getHba1c()) && latestGlucoseLog.getHba1c() > 7 )) {

                List<PrescriptionDTO> prescriptions = prescriptionRequestService.getPrescriptionsByMemberId(memberId);
                if (!Objects.isNull(prescriptions) &&  !prescriptions.isEmpty()) {
                    List<String> bgPrescriptions = getBGPrescriptions(prescriptions);
                    if (!bgPrescriptions.isEmpty()) {
                        Map<String, List<PrescriptionDTO>> prescriptionHistories
                                = prescriptionRequestService.getPrescriptionHistoryByPrescriptions(bgPrescriptions);
                        for (Map.Entry<String, List<PrescriptionDTO>> entry: prescriptionHistories.entrySet()) {
                            PrescriptionDTO result =  prescriptionHistoryCheck(entry.getValue());
                            if (!Objects.isNull(result)) {
                                prescriptionResults.add(new PrescriptionDTO(result.getMedicationName(),
                                        result.getDosageUnitValue(), result.getDosageUnitName(),
                                        result.getDosageFrequencyName(), result.getDosageFormName()));
                            }
                        }
                        if (!prescriptionResults.isEmpty()) {
                            prescriptionPrediction = new PrescriptionPredictionDTO(recentBGLogs, prescriptionResults);
                        }
                    }
                }
            }
        }
        return prescriptionPrediction;
    }

    /**
     * Checks a patient have same medication and dosage for long time
     *
     * @param histories patient prescription histories
     * @return PrescriptionDTO entity
     */
    private PrescriptionDTO prescriptionHistoryCheck(List<PrescriptionDTO> histories) {
        PrescriptionDTO result = null;
        PrescriptionDTO latestHistory = histories.get(0);
        try {
            Date prescripionEndDate = latestHistory.getEndDate();
            if (new Date().after(prescripionEndDate)) {
                for (PrescriptionDTO history : histories) {
                    if (compareMedications(latestHistory, history) && Integer.parseInt(predictionDays) <= (DateUtil.getCalendarDiff(
                            history.getPrescribedSince(), prescripionEndDate) + 1)) {
                        result = history;
                        break;
                    }
                    latestHistory = history;
                }
            }
        } catch (NumberFormatException e) {
            Logger.logError(e);
        }
        return result;
    }

    /**
     * Gets patient recent glucose log details using memberId
     *
     * @param memberId member id of the patient
     * @return recent glucose log details of the patient
     */
    public List<GlucoseLogDTO> getRecentGlucoseLog(String memberId) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setMemberId(memberId);
        requestDTO.setSkip(Constants.ZERO);
        requestDTO.setLimit(Constants.THREE);
        requestDTO.setSortOrder(Constants.MINUS_ONE);
       return glucoseLogService.getPatientGlucoseLogs(requestDTO);
    }

    /**
     * Gets patient glucose prescriptions using medication category
     *
     * @param prescriptions list of patient prescriptions
     * @return list of glucose prescription ids
     */
    private List<String> getBGPrescriptions(List<PrescriptionDTO> prescriptions) {
        Map<Long, MedicationDTO> medicationDTOMap =
                prescriptionRequestService.getMedicationDetails(prescriptions);
        return prescriptions.stream()
                .filter(prescriptionDTO -> {
                    MedicationDTO medicationDTO = medicationDTOMap
                            .get(Long.valueOf(prescriptionDTO.getMedicationId()));
                    return medicationDTO != null && medicationDTO.getCategory() != null
                            && Constants.CATEGORY_DM.equals(medicationDTO.getCategory().getName());
                })
                .map(PrescriptionDTO::getPrescriptionId)
                .toList();
    }

    /**
     * Compare two prescription dosage values
     *
     * @param prescriptionHistoryOne The PrescriptionDTO entity
     * @param prescriptionHistoryTwo The PrescriptionDTO entity
     *
     * @return true if both prescription have  dosage values
     */
    private boolean compareMedications(PrescriptionDTO prescriptionHistoryOne,
                                       PrescriptionDTO prescriptionHistoryTwo) {
        return ( prescriptionHistoryOne.getDosageFormName().equals(prescriptionHistoryTwo.getDosageFormName())
                && prescriptionHistoryOne.getDosageFrequencyName().equals(prescriptionHistoryTwo.getDosageFrequencyName())
                && prescriptionHistoryOne.getDosageUnitName().equals(prescriptionHistoryTwo.getDosageUnitName())
                && prescriptionHistoryOne.getDosageUnitValue().equals(prescriptionHistoryTwo.getDosageUnitValue()) );
    }
}
