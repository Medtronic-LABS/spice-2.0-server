package com.mdtlabs.coreplatform.fhirmapper.medicalreview.controller;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.GeneralMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.HistoryService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.IccmMedicalReviewService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.LabourService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyANCService;
import com.mdtlabs.coreplatform.fhirmapper.medicalreview.service.MedicalReviewPregnancyPNCService;
import com.mdtlabs.coreplatform.fhirmapper.ncdmedicalreview.service.NcdMedicalReviewService;

/**
 * <p>
 * This class is a controller class to perform operation on Patient
 * operations.
 * </p>
 *
 * @author Nandhakumar created on Apr 08, 2024
 */
@RestController
@RequestMapping(value = "/medical-review")
@Validated
public class MedicalReviewController {

    private GeneralMedicalReviewService generalMedicalReviewService;

    private MedicalReviewPregnancyPNCService medicalPncReviewService;

    private MedicalReviewPregnancyANCService medicalReviewANCService;

    private IccmMedicalReviewService iccmMedicalReviewService;

    private LabourService labourService;

    private HistoryService historyService;

    private NcdMedicalReviewService ncdMedicalReviewService;


    @Autowired
    public MedicalReviewController(GeneralMedicalReviewService generalMedicalReviewService,
                MedicalReviewPregnancyPNCService medicalPncReviewService,
                MedicalReviewPregnancyANCService medicalReviewANCService,
                IccmMedicalReviewService iccmMedicalReviewService,
                LabourService labourService,
                HistoryService historyService,
                NcdMedicalReviewService ncdMedicalReviewService) {
        this.generalMedicalReviewService = generalMedicalReviewService;
        this.medicalPncReviewService = medicalPncReviewService;
        this.medicalReviewANCService = medicalReviewANCService;
        this.iccmMedicalReviewService = iccmMedicalReviewService;
        this.labourService = labourService;
        this.historyService = historyService;
        this.ncdMedicalReviewService = ncdMedicalReviewService;
    }

    /**
     * Creates a general medical review.
     * <p>
     * This endpoint accepts a DTO containing patient information and creates a general medical review.
     * </p>
     *
     * @param requestDTO DTO containing patient information.
     * @return A map containing the ID of the created medical review.
     */
    @PostMapping("/iccm-general/create")
    public Map<String, String> createGeneralMedicalReview(@RequestBody GeneralMedicalReviewDTO requestDTO) {
        return generalMedicalReviewService.createGeneralMedicalReview(requestDTO);
    }

    /**
     * Retrieves the details of a general medical review based on the provided request data.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the patient's ID and reference,
     * and returns a {@link GeneralMedicalReviewSummaryDetailsDTO} containing the details of the general medical review.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID and reference.
     * @return A {@link GeneralMedicalReviewSummaryDetailsDTO} with the details of the general medical review.
     */
    @PostMapping("/iccm-general/details")
    public GeneralMedicalReviewSummaryDetailsDTO getMedicalReviewDetails(@RequestBody RequestDTO requestDTO) {
        return generalMedicalReviewService.getGeneralMedicalReviewDetails(requestDTO.getId(), requestDTO.getPatientReference());
    }

    /**
     * Creates a medical review for patients under 2 months old.
     * <p>
     * This endpoint accepts a {@link UnderFiveIccmDTO} containing patient details and assessment name,
     * and creates a medical review specifically for patients under 2 months old.
     * </p>
     *
     * @param iccmdto The {@link UnderFiveIccmDTO} containing patient details and the assessment name.
     * @return A {@link Map} containing the ID of the created medical review.
     */
    @PostMapping("/iccm-under-2months/create")
    public Map<String, String> createICCMUnder2months(@RequestBody UnderFiveIccmDTO iccmdto) {
        iccmdto.setAssessmentName(Constants.ICCM_UNDER_2M);
        return iccmMedicalReviewService.createMedicalReviewForUnder5years(iccmdto);
    }

    /**
     * Retrieves the summary details of a medical review for patients under 2 months old.
     * <p>
     * This endpoint processes a POST request containing a {@link MedicalReviewRequestDTO} with the patient's ID,
     * and returns an {@link IccmResponseDTO} containing the summary details of the medical review for patients under 2 months old.
     * </p>
     *
     * @param request The {@link MedicalReviewRequestDTO} containing the patient's ID and assessment name.
     * @return An {@link IccmResponseDTO} with the summary details of the medical review.
     */
    @PostMapping("/iccm-under-2months/details")
    public IccmResponseDTO getIccmUnder2MSummary(@RequestBody MedicalReviewRequestDTO request) {
        request.setAssessmentName(Constants.ICCM_UNDER_2M);
        return iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(request);
    }

    /**
     * Creates a medical review for patients under 5 years old.
     * <p>
     * This endpoint accepts a {@link UnderFiveIccmDTO} containing patient details and assessment name,
     * and creates a medical review specifically for patients under 5 years old.
     * </p>
     *
     * @param iccmdto The {@link UnderFiveIccmDTO} containing patient details and the assessment name.
     * @return A {@link Map} containing the ID of the created medical review.
     */
    @PostMapping("/iccm-under-5years/create")
    public Map<String, String> createICCMUnder5Years(@RequestBody UnderFiveIccmDTO iccmdto) {
        iccmdto.setAssessmentName(Constants.ICCM_ABOVE_2M_5Y);
        return iccmMedicalReviewService.createMedicalReviewForUnder5years(iccmdto);
    }

    /**
     * Retrieves the summary details of a medical review for patients under 5 years old.
     * <p>
     * This endpoint processes a POST request containing a {@link MedicalReviewRequestDTO} with the patient's ID,
     * and returns an {@link IccmResponseDTO} containing the summary details of the medical review for patients under 5 years old.
     * </p>
     *
     * @param request The {@link MedicalReviewRequestDTO} containing the patient's ID and assessment name.
     * @return An {@link IccmResponseDTO} with the summary details of the medical review.
     */
    @PostMapping("/iccm-under-5years/details")
    public IccmResponseDTO getIccmUnder2YSummary(@RequestBody MedicalReviewRequestDTO request) {
        request.setAssessmentName(Constants.ICCM_ABOVE_2M_5Y);
        return iccmMedicalReviewService.getMedicalReviewDetailsForUnderFive(request);
    }

    /**
     * Creates summary details for a general medical review.
     * <p>
     * This endpoint processes a POST request containing a {@link GeneralMedicalReviewSummaryDTO},
     * and saves the summary details for a general medical review.
     * </p>
     *
     * @param requestDTO The {@link GeneralMedicalReviewSummaryDTO} containing the summary details to be saved.
     * @return A {@link GeneralMedicalReviewSummaryDTO} containing the saved summary details.
     */
    @PostMapping("/summary-create")
    public GeneralMedicalReviewSummaryDTO createSummaryDetails(@RequestBody GeneralMedicalReviewSummaryDTO requestDTO) {
        return generalMedicalReviewService.saveSummaryDetails(requestDTO);
    }

    /**
     * Retrieves the history of medical reviews based on the provided request data.
     * <p>
     * This endpoint processes a POST request containing a {@link MedicalReviewRequestDTO} with the patient's ID and details,
     * and returns a {@link MedicalReviewHistoryDTO} containing the history of medical reviews.
     * </p>
     *
     * @param medicalReviewRequestDTO The {@link MedicalReviewRequestDTO} containing the patient's ID and details.
     * @return A {@link MedicalReviewHistoryDTO} with the history of medical reviews.
     */
    @PostMapping("/history")
    public MedicalReviewHistoryDTO getMedicalReviewHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return historyService.getHistory(medicalReviewRequestDTO);
    }

    /**
     * Retrieves the postnatal care (PNC) history of medical reviews for a child.
     * <p>
     * This endpoint processes a POST request containing a {@link MedicalReviewRequestDTO} with the patient's ID and details,
     * and returns a {@link MedicalReviewHistoryDTO} containing the PNC history of medical reviews for the child.
     * </p>
     *
     * @param medicalReviewRequestDTO The {@link MedicalReviewRequestDTO} containing the patient's ID and details.
     * @return A {@link MedicalReviewHistoryDTO} with the PNC history of medical reviews for the child.
     */
    @PostMapping("/pnc-history")
    public MedicalReviewHistoryDTO getMedicalReviewPncHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return historyService.getPncHistory(medicalReviewRequestDTO);
    }

    /**
     * Creates a medical review for antenatal care pregnancy.
     * <p>
     * This endpoint processes a POST request containing a {@link MedicalReviewPregnancyDTO} with the medical review data,
     * and creates a medical review for antenatal care pregnancy.
     * </p>
     *
     * @param requestDTO The {@link MedicalReviewPregnancyDTO} containing the medical review data to be saved.
     * @return A {@link Map} indicating the result of the operation.
     */
    @PostMapping("/anc-pregnancy/create")
    public Map<String, String> createGeneralMedicalReview(@RequestBody MedicalReviewPregnancyDTO requestDTO) {
        return medicalReviewANCService.createMedicalReview(requestDTO);
    }

    /**
     * Retrieves pregnancy summary details for antenatal care.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the unique identifier for the medical review,
     * and returns a {@link MedicalReviewPregnancySummaryDetailsDTO} containing pregnancy summary details for antenatal care.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the unique identifier for the medical review.
     * @return A {@link MedicalReviewPregnancySummaryDetailsDTO} containing pregnancy summary details.
     */
    @PostMapping("/anc-pregnancy/details")
    public MedicalReviewPregnancySummaryDetailsDTO getPregnancyMedicalReviewDetails(@RequestBody RequestDTO requestDTO) {
        if (requestDTO.isPreviousHistory()) {
            return medicalReviewANCService.getLatestEncounter(requestDTO);
        }
        return medicalReviewANCService.getPregnancyMedicalReviewDetails(requestDTO.getId(), requestDTO.getPatientReference());
    }

    /**
     * Creates a postnatal care (PNC) medical review.
     * <p>
     * This endpoint processes a POST request containing a {@link PncMedicalReviewDTO} with the PNC medical review data,
     * and creates a PNC medical review.
     * </p>
     *
     * @param requestDTO The {@link PncMedicalReviewDTO} containing the PNC medical review data to be saved.
     * @return A {@link Map} indicating the result of the operation.
     */
    @PostMapping("/pnc/create")
    public Map<String, String> createPnc(@RequestBody PncMedicalReviewDTO requestDTO) {
        return medicalPncReviewService.savePncMedicalReview(requestDTO);
    }

    /**
     * Retrieves details of a postnatal care (PNC) medical review.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the patient's ID and child's ID,
     * and returns a {@link PncMedicalReviewDTO} containing the details of the PNC medical review.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID and child's ID.
     * @return A {@link PncMedicalReviewDTO} with the details of the PNC medical review.
     */
    @PostMapping("/pnc/details")
    public PncMedicalReviewDTO getPncDetails(@RequestBody RequestDTO requestDTO) {
        return medicalPncReviewService.getPNCMedicalReviewDetails(requestDTO.getId(),
                requestDTO.getChildId(), requestDTO.getPatientReference());
    }

    /**
     * Creates a new record for a mother and neonate.
     * <p>
     * This endpoint processes a POST request containing a {@link MotherNeonateDTO} with data about the mother and neonate,
     * and creates a new record for them.
     * </p>
     *
     * @param requestDTO The {@link MotherNeonateDTO} containing data about the mother and neonate.
     * @return A {@link Map} representing the newly created record.
     */
    @PostMapping("/labour-mother-neonate/create")
    public Map<String, String> createLabourMotherAndNeonateMedicalReview(@RequestBody MotherNeonateDTO requestDTO) {
        return labourService.createLabourMotherAndNeonateMedicalReview(requestDTO);
    }

    /**
     * Retrieves details of a record for a mother and neonate.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the request data,
     * and returns a {@link MotherNeonateDTO} containing the details of the record for the mother and neonate.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the request data.
     * @return A {@link MotherNeonateDTO} with the details of the record for the mother and neonate.
     */
    @PostMapping("/labour-mother-neonate/details")
    MotherNeonateDTO getLabourMotherAndNeonateDetails(@RequestBody RequestDTO requestDTO) {
        return labourService.getLabourMotherAndNeonateDetails(requestDTO);
    }

    /**
     * Creates a blood pressure observation.
     * <p>
     * This endpoint processes a POST request containing an {@link ObservationDTO} with the blood pressure data,
     * and creates a blood pressure observation.
     * </p>
     *
     * @param requestDTO The {@link ObservationDTO} containing the blood pressure data.
     * @return An {@link ObservationDTO} indicating the result of the operation.
     */
    @PostMapping("/bp/create")
    public ObservationDTO createBp(@RequestBody ObservationDTO requestDTO) {
        requestDTO.setType(Constants.BP);
        return medicalReviewANCService.createValueObservation(requestDTO);
    }

    /**
     * Creates a weight observation.
     * <p>
     * This endpoint processes a POST request containing an {@link ObservationDTO} with the weight data,
     * and creates a weight observation.
     * </p>
     *
     * @param requestDTO The {@link ObservationDTO} containing the weight data.
     * @return An {@link ObservationDTO} indicating the result of the operation.
     */
    @PostMapping("/weight/create")
    public ObservationDTO createWeight(@RequestBody ObservationDTO requestDTO) {
        requestDTO.setType(Constants.WEIGHT);
        return medicalReviewANCService.createValueObservation(requestDTO);
    }

    /**
     * Retrieves the weight of a patient.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the patient's ID,
     * and returns a {@link Map} containing the weight of the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID.
     * @return A {@link Map} containing the weight of the patient.
     */
    @PostMapping("/weight")
    public Map<String, Double> getWeight(@RequestBody RequestDTO requestDTO) {
        requestDTO.setType(Constants.WEIGHT);
        return medicalReviewANCService.getPatientWeight(requestDTO);
    }

    /**
     * Retrieves the blood pressure of a patient.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the patient's ID,
     * and returns a {@link Map} containing the blood pressure of the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID.
     * @return A {@link Map} containing the blood pressure of the patient.
     */
    @PostMapping("/bp")
    public Map<String, Double> getBp(@RequestBody RequestDTO requestDTO) {
        requestDTO.setType(Constants.BP);
        return medicalReviewANCService.getPatientBp(requestDTO);
    }

    /**
     * Retrieves the birth history details based on the provided encounter ID.
     * <p>
     * This endpoint processes a POST request containing a {@link RequestDTO} with the encounter ID,
     * and returns a {@link BirthHistoryDTO} containing the birth history details.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the encounter ID.
     * @return A {@link BirthHistoryDTO} with the birth history details.
     */
    @PostMapping("/birth-history")
    public BirthHistoryDTO getBirthHistory(@RequestBody RequestDTO requestDTO) {
        return labourService.getBirthHistory(requestDTO.getMemberId());
    }

    /**
     * Creates PNC child member details
     * <p>
     * This endpoint processes a POST request containing a {@link HouseholdMemberDTO} with the member details
     * </p>
     *
     * @param householdMemberDTO The {@link HouseholdMemberDTO} containing the member details.
     */
    @PostMapping("/pnc-child/create")
    public void createPncChild(@RequestBody HouseholdMemberDTO householdMemberDTO) {
        medicalPncReviewService.createPncChild(householdMemberDTO);
    }

    /**
     * <p>
     * Used to create or update the patient status
     * <p/>
     *
     * @param patientStatus The {@link PatientStatusDTO} - Contains the patient status details.
     */
    @PostMapping("/patient-status/create")
    public PatientStatusDTO createPatientStatus(@RequestBody PatientStatusDTO patientStatus) {
        return generalMedicalReviewService.createPatientStatus(patientStatus);
    }

    /**
     * <p>
     * Used to get the patient status details based on the patient id.
     * <p/>
     *
     * @param patientStatus - Patient id to get the patient status.
     * @return {@link PatientStatusDTO} - Patient status details
     */
    @PostMapping("/patient-status/details")
    public PatientStatusDTO getPatientStatusDetails(@RequestBody PatientStatusDTO patientStatus) {
        return generalMedicalReviewService.getPatientStatusDetails(patientStatus);
    }

    /**
     * Creates medical review for NCD.
     *
     * @param patientStatus The {@link PatientStatusDTO} - Contains the patient status details.
     */
    @PostMapping("/ncd/create")
    public Map<String, String> createNcdMedicalReview(@RequestBody NCDMedicalReviewDTO request) {
        return ncdMedicalReviewService.createNcdMedicalReview(request);
    }

    /**
     * Gets ncd medical review summary.
     *
     * @param patientStatus The {@link PatientStatusDTO} - Contains the patient status details.
     */
    @PostMapping("/ncd/details")
    public NcdMedicalReviewResponse getNcdMedicalReviewDetails(@RequestBody MedicalReviewRequestDTO request) {
        return ncdMedicalReviewService.ncdMedicalReviewSummary(request);
    }

    /**
     * <p>
     * This function updates a confirm diagnosis for the given patient.
     * </p>
     *
     * @param confirmDiagnosis {@link ConfirmDiagnosisDTO} It contains the diagnosis details of the patient that
     *                         need to be created or updated
     */
    @PostMapping("/confirm-diagnosis/update")
    public void updateConfirmDiagnosis(@RequestBody ConfirmDiagnosisDTO confirmDiagnosis) {
        generalMedicalReviewService.updateConfirmDiagnosis(confirmDiagnosis);
    }

    /**
     * <p>
     * Creates a summary for the given NCD medical review.
     * </p>
     *
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     */
    @PostMapping("/ncd/summary-create")
    public void createSummary(@RequestBody NCDMedicalReviewDTO request) {
        ncdMedicalReviewService.addOrUpdateNextVisitDate(request);
    }

    /**
     * Gets medical review count.
     *
     * @param request {@link RequestDTO} - request dto
     */
    @PostMapping("/count")
    public Map<String, Integer> getMedicalReviewCount(@RequestBody RequestDTO request) {
        return generalMedicalReviewService.getMedicalReviewCount(request);
    }

    /**
     * <p>
     * The function retrieves NCD medical review history based on the provided
     * `MedicalReviewRequestDTO`.
     *
     * @param medicalReviewRequestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     *                                history, and any other relevant data needed to retrieve the NCD history
     * @return {@link NCDMedicalReviewHistoryDTO} The history based on given request is retrieved.
     */
    @PostMapping("/ncd/history-list")
    public NCDMedicalReviewHistoryDTO getNCDMedicalReviewHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return historyService.getNCDMedicalReviewHistory(medicalReviewRequestDTO);
    }

    /**
     * The function  retrieves the NCD medical review summary history
     * based on the provided `MedicalReviewRequestDTO`.
     *
     * @param medicalReviewRequestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     *                                history, and any other relevant data needed to retrieve the NCD history
     * @return {@link NCDMedicalReviewHistoryDTO} The history based on given request is retrieved.
     */
    @PostMapping("/ncd/history-summary")
    public NCDMedicalReviewHistoryDTO getNCDMedicalReviewSummaryHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return historyService.getNCDMedicalReviewSummaryHistory(medicalReviewRequestDTO);
    }

    /**
     * <p>
     * Used to Update Patient Nutrition Lifestyle and Patient Psychology by its Patient Track id and Patient Visit id.
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains Patient reference and Menu name.
     * @return A {@link Boolean} object corresponds to the View update
     */
    @PutMapping("/update-view-status")
    public Boolean updateViewCount(@RequestBody RequestDTO request) {
        return generalMedicalReviewService.updateViewCount(request);
    }

    /**
     * Gets patient lifestyle details
     *
     * @param request
     */
    @PostMapping("/patient-lifestyle-details")
    List<LifestyleResponseDTO> getPatientLifestyleDetails(@RequestBody RequestDTO request) {
        return ncdMedicalReviewService.getPatientLifestyle(request);
    }

    /**
     * <p>
     * Creates or updates an appointment of the given NCD medical review.
     * </p>
     *
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     */
    @PostMapping("/ncd/date/update")
    public void updateAppointmentDate(@RequestBody NCDMedicalReviewDTO request) {
        ncdMedicalReviewService.addOrUpdateNextVisitDate(request);
    }

    /**
     * Creates PNC child member
     * <p>
     * This endpoint processes a POST request containing a {@link HouseholdMemberDTO} with the member details
     * </p>
     *
     * @param householdMemberDTO The {@link HouseholdMemberDTO} containing the household member details.
     */
    @PostMapping("/pnc-child-create")
    public HouseholdMemberDTO createChild(@RequestBody HouseholdMemberDTO householdMemberDTO) {
        return medicalPncReviewService.createChild(householdMemberDTO);
    }
}
