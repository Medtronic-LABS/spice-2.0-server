package com.mdtlabs.coreplatform.spiceservice.patient.controller;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.WgsDataDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.patient.service.PatientService;

/**
 * Controller for managing patient-related operations.
 * <p>
 * This controller handles various patient-related operations such as creating and searching for patients,
 * retrieving patient details, managing patient status and referrals, and handling patient diagnoses.
 * It interacts with the {@link PatientService} to perform these operations.
 * </p>
 */
@RestController
@RequestMapping(value = "/patient")
@Validated
public class PatientController {

    private final PatientService patientService;

    public PatientController(PatientService patientService) {
        this.patientService = patientService;
    }

    /**
     * Creates a new patient record.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing patient data and uses the {@link PatientService}
     * to create a new patient record. It returns the ID of the newly created patient resource.
     * </p>
     *
     * @param request The {@link RequestDTO} object containing the patient data.
     * @return A {@link ResponseEntity} containing the ID of the created patient resource.
     */
    @PostMapping("/create")
    public ResponseEntity<String> createPatientByPatientId(@RequestBody RequestDTO request) {
        return ResponseEntity.ok().body(patientService.createPatientByPatientId(request));
    }

    /**
     * <p>
     * Update a existing patient record.
     * This endpoint accepts a {@link EnrollmentRequestDTO} containing patient data and uses the {@link PatientService}
     * to create a new patient record. It returns the ID of the newly created patient resource.
     * </p>
     *
     * @param enrollmentRequestDto The {@link EnrollmentRequestDTO} object containing the patient data.
     * @return A {@link EnrollmentResponseDTO} containing the ID of the created patient resource.
     */
    @PostMapping("/update")
    public SuccessResponse<EnrollmentResponseDTO> updatePatient(@RequestBody EnrollmentRequestDTO enrollmentRequestDto) {
        EnrollmentResponseDTO enrollmentResponseDto = patientService.updatePatient(enrollmentRequestDto);
        return new SuccessResponse<>(SuccessCode.UPDATE_PATIENT_DETAILS, enrollmentResponseDto, HttpStatus.OK);
    }


    /**
     * Retrieves a list of patients based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link PatientRequestDTO} as criteria for searching and returns a list of patients
     * matching the criteria.
     * </p>
     *
     * @param patientRequestDTO The criteria for searching patients.
     * @return A {@link SuccessResponse} containing a list of patients matching the criteria.
     */
    @PostMapping("/list")
    public SuccessResponse<Map<String, Object>> getPatient(@RequestBody PatientRequestDTO patientRequestDTO)  {
        return new SuccessResponse<>(SuccessCode.GET_PATIENT_LIST,
                patientService.getPatientList(patientRequestDTO), HttpStatus.OK);
    }

    /**
     * Searches for patients based on the provided criteria.
     * <p>
     * Similar to the list endpoint, but may implement different search logic or return format.
     * </p>
     *
     * @param patientRequestDTO The criteria for searching patients.
     * @return A {@link SuccessResponse} containing the search results.
     */
    @PostMapping("/search")
    @ConfigureAppType
    public SuccessResponse<Map<String, Object>> searchPatient(@RequestBody PatientRequestDTO patientRequestDTO) {
        if (Constants.NON_COMMUNITY.equalsIgnoreCase(SelectedAppTypeContextHolder.get())){
            return new SuccessResponse<>(SuccessCode.PATIENT_SEARCH,
                    patientService.searchPatients(patientRequestDTO), HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.PATIENT_SEARCH, patientService.searchPatient(patientRequestDTO), HttpStatus.OK);
    }

    /**
     * Retrieves detailed information for a specific patient.
     * <p>
     * This endpoint accepts a {@link PatientDTO} identifying the patient and returns detailed information about the patient.
     * </p>
     *
     * @param patientDTO The identifier for the patient.
     * @return A {@link SuccessResponse} containing detailed patient information.
     */
    @PostMapping("/patientDetails")
    @ConfigureAppType
    public SuccessResponse<PatientDetailsDTO> getPatientDetails(@RequestBody PatientDTO patientDTO) {
        if (Constants.NON_COMMUNITY.equalsIgnoreCase(SelectedAppTypeContextHolder.get())) {
            return new SuccessResponse<>(SuccessCode.GET_PATIENT_DETAILS,
                    patientService.searchPatientDetails(patientDTO), HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT, patientService.getPatientDetails(patientDTO), HttpStatus.OK);
    }

    /**
     * Retrieves the current status of a patient by their ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient ID and returns the current status
     * of the patient as a map of status attributes.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient ID.
     * @return A {@link SuccessResponse} containing the patient's status as a map.
     */
    @PostMapping("/patient-status")
    public SuccessResponse<Map<String, Object>> getPatientStatus(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT_STATUS, patientService.getPatientStatus(requestDTO), HttpStatus.OK);
    }

    /**
     * Updates the status of a patient by their ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient ID and the new status details,
     * and updates the patient's status accordingly.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient ID and new status details.
     * @return A {@link SuccessResponse} indicating the success of the update operation.
     */
    @PostMapping("/update-patient-status")
    public SuccessResponse<Boolean> updatePatientStatus(@RequestBody RequestDTO requestDTO) {
        return new SuccessResponse<>(SuccessCode.UPDATED_PATIENT_STATUS,
                patientService.updateStatusOfServiceRequest(requestDTO), HttpStatus.OK);
    }

    /**
     * Retrieves referral tickets for a patient by their ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient ID and returns a list of referral
     * tickets associated with the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient ID.
     * @return A {@link SuccessResponse} containing a list of referral tickets.
     */
    @PostMapping("/referral-tickets")
    public SuccessResponse<ReferralTicketDTO> getReferralTicket(@RequestBody RequestDTO requestDTO) {
        List<ReferralTicketDTO> referralTicketDTOS = patientService.getReferralTickets(requestDTO);
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT_TICKETS, !referralTicketDTOS.isEmpty() ?
                referralTicketDTOS.getFirst() : null, HttpStatus.OK);
    }

    /**
     * Creates a new referral ticket for a patient by their ID.
     * <p>
     * This endpoint accepts a {@link ReferralDetailsDTO} containing the patient ID and referral details,
     * and creates a new referral ticket for the patient.
     * </p>
     *
     * @param referralTicketDTO The {@link ReferralDetailsDTO} containing the patient ID and referral details.
     * @return A {@link SuccessResponse} indicating the success of the referral ticket creation.
     */
    @PostMapping("/referral-tickets/create")
    public SuccessResponse<ReferralDetailsDTO> createReferralTicket(@RequestBody ReferralDetailsDTO referralTicketDTO) {
        return new SuccessResponse<>(SuccessCode.REFERRAL_TICKET_SAVE, patientService.createReferralTicket(referralTicketDTO)
                , HttpStatus.OK);
    }

    /**
     * Confirms and records a diagnosis for a patient.
     * <p>
     * This endpoint accepts a {@link DiagnosisDTO} containing the diagnosis details and records the diagnosis
     * for the patient.
     * </p>
     *
     * @param diagnosis The {@link DiagnosisDTO} containing the diagnosis details.
     * @return A {@link SuccessResponse} indicating the success of the diagnosis recording.
     */
    @PostMapping("/confirm-diagnosis")
    public SuccessResponse<Boolean> updatePatientDiagnosis(@RequestBody DiagnosisDTO diagnosis) {
        patientService.createDiagnosis(diagnosis);
        return new SuccessResponse<>(SuccessCode.PATIENT_DIAGNOSIS, HttpStatus.OK);
    }

    /**
     * Retrieves diagnosis details for a patient.
     * <p>
     * This endpoint accepts a {@link RequestDTO} identifying the patient and returns detailed information
     * about the patient's diagnosis.
     * </p>
     *
     * @param request The {@link RequestDTO} identifying the patient.
     * @return A {@link SuccessResponse} containing detailed diagnosis information.
     */
    @PostMapping("/diagnosis-details")
    public SuccessResponse<DiseaseDTO> getPatientDiagnosis(@RequestBody RequestDTO request) {
        return new SuccessResponse<>(SuccessCode.PATIENT_DIAGNOSIS, patientService.getPatientDiagnosis(request), HttpStatus.OK);
    }

    /**
     * Retrieves diagnosis details for a patient.
     * <p>
     * This endpoint accepts a {@link RequestDTO} identifying the patient and returns detailed information
     * about the patient's diagnosis.
     * </p>
     *
     * @param request The {@link RequestDTO} identifying the patient.
     * @return A {@link SuccessResponse} containing detailed diagnosis information.
     */
    @PostMapping("/get-diagnosis-details")
    public SuccessResponse<ConfirmDiagnosisDTO> getPatientDiagnosisDetails(@RequestBody RequestDTO request) {
        return new SuccessResponse<>(SuccessCode.PATIENT_DIAGNOSIS, patientService.getPatientDiagnosisDetails(request), HttpStatus.OK);
    }

    /**
     * Retrieves pregnancy information for patients by villages.
     * <p>
     * This endpoint is responsible for fetching pregnancy information for patients based on the villages they reside in.
     * It accepts a {@link RequestDTO} which contains criteria such as village names or IDs, and returns a list of
     * {@link PregnancyInfo} objects that match the criteria. This can be useful for health monitoring and planning
     * purposes in rural healthcare settings.
     * </p>
     *
     * @param request The {@link RequestDTO} containing the search criteria, typically village identifiers.
     * @return A list of {@link PregnancyInfo} objects containing pregnancy details of patients from specified villages.
     */
    @PostMapping("/pregnancy/info")
    public List<PregnancyInfo> getPregnancyInfoByVillages(@RequestBody RequestDTO request) {
        return patientService.getPregnancyInfoByVillages(request);
    }

    /**
     * Create pregnancy details to the patient.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} containing pregnancy details.
     * @return PatientPregnancyDetails Entity.
     */
    @PostMapping("/pregnancy/create")
    public SuccessResponse<PregnancyDetailsDTO> createPregnancyDetails(
            @RequestBody PregnancyDetailsDTO pregnancyDetailsDTO) {
        Logger.logInfo("In Patient controller, creating a patient's pregnancy details");
        return new SuccessResponse<>(SuccessCode.PATIENT_PREGNANCY_SAVE,
                patientService.createPregnancyDetails(pregnancyDetailsDTO), HttpStatus.CREATED);
    }

    /**
     * Gets Pregnancy details of a patient.
     *
     * @param requestData Request data with patientTrackId
     * @return PregnancyDetailsDTO object.
     */
    @PostMapping("/pregnancy/details")
    public SuccessResponse<PregnancyDetailsDTO> getPregnancyDetails(@RequestBody RequestDTO requestData) {
        Logger.logInfo("In Patient controller, getting a patient's pregnancy details");
        return new SuccessResponse<>(SuccessCode.GET_PATIENT_PREGNANCY, patientService.getPregnancyDetails(requestData),
                HttpStatus.CREATED);
    }

    /**
     * <p>
     * This method is used to update pregnancy ANC risk status to the patient.
     * </p>
     *
     * @param pregnancyDetailsDTO - {@link PregnancyDetailsDTO}
     * @return Boolean - boolean
     */
    @PutMapping("/pregnancy-anc-risk/update")
    public SuccessResponse<Boolean> updatePregnancyANCRisk(@RequestBody PregnancyDetailsDTO pregnancyDetailsDTO) {
        pregnancyDetailsDTO.setIsPregnant(Boolean.TRUE);
        return new SuccessResponse<>(SuccessCode.PREGNANCY_ANC_RISK_UPDATE,
                patientService.updatePregnancyANCRisk(pregnancyDetailsDTO), HttpStatus.OK);
    }

    /**
     * Used to delete the patient
     * @param request
     * @return
     */
    @PostMapping("/delete")
    public SuccessResponse<String> deletePatientByPatientId(@RequestBody RequestDTO request) {
        patientService.deletePatientByPatientId(request);
        return new SuccessResponse<>(SuccessCode.DELETE_SUCCESS, HttpStatus.OK);
    }

    /**
     * <p>
     * This function is to update referred site in patient.
     * </p>
     *
     * @param requestDTO {@link ScreeningLogRequestDTO} It contains the  necessary information to
     *                                                   update referred site.
     */
    @PostMapping("/referred-site-update")
    @TenantValidation
    public SuccessResponse<String> updateReferredSite(@RequestBody ScreeningLogRequestDTO requestDTO) {
        patientService.updateReferredSite(requestDTO);
        return new SuccessResponse<>(SuccessCode.REFERRED_SITE_UPDATE, HttpStatus.OK);
    }

    /**
     * <p>
     * Retrieves the calculated growth standards data for a patient based on the provided WgsDataDTO.
     * </p>
     *
     * @param wgsDataDTO - Contains the input params to get WGS data.
     * @return - Returns a SuccessResponse containing a map of calculated WGS data for the patient
     */
    @PostMapping("/calculate-wgs")
    public SuccessResponse<Map<String, Object>> getPatientWgsData(@RequestBody WgsDataDTO wgsDataDTO) {
        return new SuccessResponse<>(SuccessCode.GOT_PATIENT_WGS_DATA,
                patientService.getPatientWgsData(wgsDataDTO), HttpStatus.OK);
    }

    /**
     * Get patient details by village ids
     *
     * @param request - RequestDTO Entity
     * @return List of PatientDetailsDTO entity contains patient details
     */
    @PostMapping("/offline/list")
    public List<PatientDetailsDTO> listPatientDetails(@RequestBody RequestDTO request) {
        return patientService.getPatientDetailsByVillageIds(request);
    }
}
