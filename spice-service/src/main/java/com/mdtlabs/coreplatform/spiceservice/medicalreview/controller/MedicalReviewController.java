package com.mdtlabs.coreplatform.spiceservice.medicalreview.controller;

import java.util.List;
import java.util.HashMap;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;

import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MotherDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.spiceservice.medicalreview.service.MedicalReviewService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * <p>
 * This class is a controller class to perform operation on Patient
 * operations.
 * </p>
 *
 * @author Nanthinee sugumar created on Feb 28, 2024
 */
@RestController
@RequestMapping(value = "/medical-review")
@Validated
public class MedicalReviewController {

    private final MedicalReviewService medicalReviewService;

    public MedicalReviewController(MedicalReviewService medicalReviewService) {
        this.medicalReviewService = medicalReviewService;
    }

    /**
     * create medical Review
     *
     * @param requestDTO patient id and Details
     * @return MedicalReviewId
     */
    @PostMapping("/iccm-general/create")
    public SuccessResponse<Map<String, String>> createGeneralMedicalReview(@RequestBody GeneralMedicalReviewDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.createGeneralMedicalReview(requestDTO), HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param requestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/iccm-general/details")
    public SuccessResponse<GeneralMedicalReviewSummaryDetailsDTO> getMedicalReviewDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS,
                medicalReviewService.getGeneralMedicalReviewDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * Saves medical review summay details.
     *
     * @param requestDTO
     * @return
     */
    @PostMapping("/summary-create")
    public SuccessResponse<GeneralMedicalReviewSummaryDTO> createSummaryDetails(@RequestBody GeneralMedicalReviewSummaryDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.saveSummaryDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * create medical Review
     *
     * @param iccmdto patient id and Details
     * @return MedicalReviewId
     */
    @PostMapping("/iccm-under-2months/create")
    public SuccessResponse<Map<String, String>> createICCMUnder2months(@RequestBody UnderFiveIccmDTO iccmdto) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.createICCMUnder2months(iccmdto), HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param request patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/iccm-under-2months/details")
    public SuccessResponse<IccmResponseDTO> getIccmUnder2MSummary(@RequestBody MedicalReviewRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS, medicalReviewService.getIccmUnder2MSummary(request), HttpStatus.OK);
    }

    /**
     * create medical Review
     *
     * @param iccmdto patient id and Details
     * @return MedicalReviewId
     */
    @PostMapping("/iccm-under-5years/create")
    public SuccessResponse<Map<String, String>> createICCMUnder5Years(@RequestBody UnderFiveIccmDTO iccmdto) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.createICCMUnder5Years(iccmdto), HttpStatus.OK);
    }

    /**
     * Get History of Medical Review
     *
     * @param medicalReviewRequestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/history")
    public SuccessResponse<MedicalReviewHistoryDTO> getMedicalReviewHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS,
                medicalReviewService.getHistory(medicalReviewRequestDTO),
                HttpStatus.OK);
    }

    /**
     * Get PNC History of Medical Review
     *
     * @param medicalReviewRequestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/pnc-history")
    public SuccessResponse<MedicalReviewHistoryDTO> getPncMedicalReviewHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS,
                medicalReviewService.getPncHistory(medicalReviewRequestDTO),
                HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param requestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/pnc/create")
    public SuccessResponse<Map<String, String>> createPnc(@RequestBody PncMedicalReviewDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.savePncMedicalReview(requestDTO), HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param request patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/iccm-under-5years/details")
    public SuccessResponse<IccmResponseDTO> getIccmUnder2YSummary(@RequestBody MedicalReviewRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS, medicalReviewService.getIccmUnder5YSummary(request), HttpStatus.OK);
    }

    /**
     * Endpoint for retrieving details of a medical review related to ANC pregnancy.
     *
     * @param requestDTO The RequestDTO containing necessary information for fetching pregnancy medical review details.
     * @return A SuccessResponse containing the details of the pregnancy medical review and a success code.
     */
    @PostMapping("/anc-pregnancy/details")
    public SuccessResponse<MedicalReviewPregnancySummaryDetailsDTO> getPregnancyMedicalReviewDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS,
                medicalReviewService.getPregnancyMedicalReviewDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * Endpoint for creating a general medical review related to ANC pregnancy.
     *
     * @param requestDTO The MedicalReviewDTO containing information for the medical review to be created.
     * @return A SuccessResponse containing the newly created MedicalReviewDTO and a success code.
     */
    @PostMapping("/anc-pregnancy/create")
    public SuccessResponse<Map<String, String>> createGeneralMedicalReview(@RequestBody MedicalReviewPregnancyDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.createMedicalReview(requestDTO), HttpStatus.OK);
    }

    /**
     * @param requestDTO patient id and Details
     * @param requestDTO The request data containing information about labor, mother, and neonate.
     * @return MedicalReview Details
     * /**
     * Endpoint for creating a medical review for labor, mother, and neonate.
     * @return A SuccessResponse containing the created medical review for labor, mother, and neonate.
     */
    @PostMapping("/labour-mother-neonate/create")
    public SuccessResponse<Map<String, String>> createLabourMotherAndNeonateMedicalReview(@RequestBody MotherNeonateDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.createLabourMotherAndNeonate(requestDTO), HttpStatus.OK);
    }

    /**
     * Endpoint for retrieving details of labor, mother, and neonate.
     *
     * @param requestDTO The request data containing necessary information.
     * @return A SuccessResponse containing the details of labor, mother, and neonate.
     */
    @PostMapping("/labour-mother-neonate/details")
    public SuccessResponse<MotherDTO> getLabourMotherAndNeonateDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS,
                medicalReviewService.getLabourMotherAndNeonateDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * Endpoint for saving weight observations.
     * This method handles HTTP POST requests to save weight observations.
     * It receives an observation DTO containing weight data in the request body.
     *
     * @param requestDTO The observation DTO containing weight data to be saved.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    @PostMapping("/pnc/details")
    public SuccessResponse<PncMedicalReviewDTO> getPncDetails(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_MEDICAL_REVIEW_DETAILS,
                medicalReviewService.getPNCMedicalReviewDetails(requestDTO), HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param requestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/bp/create")
    public SuccessResponse<ObservationDTO> createBp(@RequestBody ObservationDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.CREATE_PATIENT_DETAILS, medicalReviewService.createBp(requestDTO),
                HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param requestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/weight/create")
    public SuccessResponse<ObservationDTO> createWeight(@RequestBody ObservationDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.CREATE_PATIENT_DETAILS,
                medicalReviewService.createWeight(requestDTO), HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param requestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/weight")
    public SuccessResponse<Map<String, Double>> getWeight(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT_DETAILS,
                medicalReviewService.getPatientWeight(requestDTO), HttpStatus.OK);
    }

    /**
     * Get medical Review Details
     *
     * @param requestDTO patient id and Details
     * @return MedicalReview Details
     */
    @PostMapping("/bp")
    public SuccessResponse<Map<String, Double>> getBp(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT_DETAILS,
                medicalReviewService.getPatientBp(requestDTO), HttpStatus.OK);
    }

    /**
     * Get Birth-History Details
     *
     * @param requestDTO the requestDTO
     * @return BirthHistoryDTO Details
     */
    @PostMapping("/birth-history")
    public SuccessResponse<BirthHistoryDTO> getBirthHistory(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED,
                medicalReviewService.getBirthHistory(requestDTO), HttpStatus.OK);
    }

    /**
     * Creates PNC child member details
     * <p>
     * This endpoint processes a POST request containing a {@link HouseholdMemberDTO} with the member details
     * </p>
     *
     * @param householdMemberDTO The {@link HouseholdMemberDTO} containing the member details
     * @return A {@link SuccessResponse} containing the success code and HTTP status
     */
    @PostMapping("/pnc-child/create")
    public SuccessResponse<String> createPncChild(@RequestBody HouseholdMemberDTO householdMemberDTO) {
        medicalReviewService.createPncChild(householdMemberDTO);
        return new SuccessResponse<>(SuccessCode.HOUSEHOLD_MEMBER_SAVE, HttpStatus.OK);
    }

    /**
     * <p>
     * Used to create or update the patient status
     * <p/>
     *
     * @param patientStatus The {@link PatientStatusDTO} - Contains the patient status details.
     * @return {@link PatientStatusDTO} The patient status details
     */
    @PostMapping("/patient-status/create")
    public SuccessResponse<PatientStatusDTO> createPatientStatus(@RequestBody PatientStatusDTO patientStatus) {
        return new SuccessResponse<>(SuccessCode.PATIENT_STATUS_UPDATE, medicalReviewService.createPatientStatus(patientStatus), HttpStatus.OK);
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
    public SuccessResponse<PatientStatusDTO> getPatientStatusDetails(@RequestBody PatientStatusDTO patientStatus) {
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT_STATUS_DETAILS,
                medicalReviewService.getPatientStatusDetails(patientStatus), HttpStatus.OK);
    }

    /**
     * Used to create or update the patient status
     * <p/>
     *
     * @param patientStatus The {@link PatientStatusDTO} - Contains the patient status details.
     */
    @PostMapping("/ncd/create")
    public SuccessResponse<Map<String, String>> createNcdMedicalReview(@RequestBody NCDMedicalReviewDTO request) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED, medicalReviewService.createNcdMedicalReview(request), HttpStatus.CREATED) ;
    }

    /**
     * <p>
     * Endpoint for creating a summary for the given NCD medical review.
     *</p>
     *
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     * @return A SuccessResponse indicating the result of the summary creation operation.
     */
    @PostMapping("/ncd/summary-create")
    public SuccessResponse<Map<String, String>> createSummary(@RequestBody NCDMedicalReviewDTO request) {
        medicalReviewService.createSummary(request);
        return new SuccessResponse<>(SuccessCode.CREATE_SUMMARY, HttpStatus.CREATED);
    }

    /**
     * Used to create or update the patient status
     * <p/>
     *
     * @param
     * patientStatus The {@link PatientStatusDTO} - Contains the patient status details.
     */
    @PostMapping("/ncd/details")
    public SuccessResponse<NcdMedicalReviewResponse> getNcdMedicalReviewDetails(@RequestBody MedicalReviewRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.MEDICAL_REVIEW_CREATED, medicalReviewService.ncdMedicalReviewSummary(request), HttpStatus.OK);
    }

    /**
     * <p>
     * This function updates a confirm diagnosis for the given patient.
     * </p>
     *
     * @param confirmDiagnosisDTO {@link ConfirmDiagnosisDTO} It contains the diagnosis details of the patient that
     *                            need to be created or updated
     * @return {@link SuccessResponse} Returns the success response with success message and http status.
     */
    @PostMapping("/confirm-diagnosis/update")
    public SuccessResponse<ConfirmDiagnosisDTO> updateConfirmDiagnosis(@RequestBody ConfirmDiagnosisDTO confirmDiagnosisDTO) {
        medicalReviewService.updateConfirmDiagnosis(confirmDiagnosisDTO);
        return new SuccessResponse<>(SuccessCode.PATIENT_DIAGNOSIS, HttpStatus.OK);
    }

    /**
     * Gets medical review count.
     *
     * @param request {@link RequestDTO} - request dto
     * @return SuccessResponse - response
     */
    @PostMapping("/count")
    public SuccessResponse<Map<String, Integer>> getMedicalReviewCount(@RequestBody RequestDTO request) {
        return new SuccessResponse<>(SuccessCode.GET_MEDICAL_REVIEW_COUNT,
                medicalReviewService.getMedicalReviewCount(request), HttpStatus.OK);
    }

    /**
     * The function retrieves NCD medical review history based on the provided
     * `MedicalReviewRequestDTO`.
     *
     * @param medicalReviewRequestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     *                                history, and any other relevant data needed to retrieve the NCD history
     * @return {@link SuccessResponse} The history based on given request is retrieved.
     */
    @PostMapping("/ncd/history-list")
    public SuccessResponse<NCDMedicalReviewHistoryDTO> getNCDMedicalReviewHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return new SuccessResponse<>(SuccessCode.GET_MEDICAL_REVIEW_LIST, medicalReviewService.getNCDMedicalReviewHistory(medicalReviewRequestDTO), HttpStatus.OK);
    }

    /**
     * The function  retrieves the NCD medical review summary history
     * based on the provided `MedicalReviewRequestDTO`.
     *
     * @param medicalReviewRequestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     *                                history, and any other relevant data needed to retrieve the NCD history
     * @return {@link SuccessResponse} The history based on given request is retrieved.
     */
    @PostMapping("/ncd/history-summary")
    public SuccessResponse<NCDMedicalReviewHistoryDTO> getNCDMedicalReviewSummaryHistory(@RequestBody MedicalReviewRequestDTO medicalReviewRequestDTO) {
        return new SuccessResponse<>(SuccessCode.GET_MEDICAL_REVIEW_SUMMARY, medicalReviewService.getNCDMedicalReviewSummaryHistory(medicalReviewRequestDTO), HttpStatus.OK);
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
    public SuccessResponse<Boolean> updateViewCount(@RequestBody RequestDTO request) {
        return new SuccessResponse<>(SuccessCode.NOTIFICATION_UPDATE_VIEW,
                medicalReviewService.updateViewCount(request), HttpStatus.OK);

    }

    /**
     * Gets patient lifestyle details
     *
     * @param request The {@link PsychologyDTO} - Contains the patient details.
     */
    @PostMapping("/patient-lifestyle-details")
    public SuccessResponse<LifestyleResponseDTO> getPatientLifestyleDetails(@RequestBody RequestDTO request) {
        return new SuccessResponse<>(SuccessCode.PATIENT_LIFESTYLE_DETAILS, medicalReviewService.getPatientLifestyleDetails(request), HttpStatus.OK) ;
    }


    /**
     * Used to get instructions
     * @return SuccessResponse - response
     */
    @GetMapping("/get-instructions")
    public SuccessResponse<Map<String, List<String>>> getInstruction() {
        Map<String, List<String>>  instructions = new HashMap<>();
        instructions.put(Constants.INSTRUCTIONS, medicalReviewService.getInstruction());
        return new SuccessResponse<>(SuccessCode.GET_INSTRUCTION_SUCCESS, instructions, HttpStatus.OK);
    }

    /**
     * <p>
     * Creates or updates an appointment of the given NCD medical review.
     * </p>
     *
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     */
    @PostMapping("/ncd/date/update")
    public SuccessResponse<Map<String, String>> updateAppointmentDate(@RequestBody NCDMedicalReviewDTO request) {
        medicalReviewService.updateNCDAppointment(request);
        return new SuccessResponse<>(SuccessCode.APPOINTMENT_UPDATED, HttpStatus.CREATED);
    }
}

