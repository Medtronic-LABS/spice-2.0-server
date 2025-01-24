package com.mdtlabs.coreplatform.fhirmapper.patient.controller;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Patient;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.mapper.FhirMapper;
import com.mdtlabs.coreplatform.fhirmapper.patient.service.PatientService;

/**
 * Controller for managing patient-related operations.
 * <p>
 * This controller provides endpoints for creating and managing patient data, including
 * creating patients, retrieving patient details, updating patient status, and managing
 * referral tickets and diagnoses.
 * </p>
 *
 * @author Nanthinee sugumar created on Feb 28, 2024
 */
@RestController
@RequestMapping(value = "/patient")
@Validated
public class PatientController {

    private final PatientService patientService;

    private final FhirMapper fhirMapper;

    public PatientController(PatientService patientService, FhirMapper fhirMapper) {
        this.patientService = patientService;
        this.fhirMapper = fhirMapper;
    }

    /**
     * Creates a new patient resource in the FHIR server.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing patient information and uses it
     * to create a new patient resource in the FHIR server. The ID of the newly created patient
     * resource is returned.
     * </p>
     *
     * @param request The {@link RequestDTO} containing the patient information.
     * @return The ID of the newly created patient resource.
     */
    @PostMapping("/create")
    public String createPatientByPatientId(@RequestBody RequestDTO request) {
        return patientService.createPatientByPatientId(request);
    }

    /**
     * Creates a new patient resource in the FHIR server.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing patient information and uses it
     * to create a new patient resource in the FHIR server. The ID of the newly created patient
     * resource is returned.
     * </p>
     *
     * @param enrollmentRequestDto The {@link EnrollmentRequestDTO} containing the patient information.
     * @return EnrollmentResponseDTO object.
     */
    @PostMapping("/update")
    public EnrollmentResponseDTO updatePatient(@RequestBody EnrollmentRequestDTO enrollmentRequestDto) {
        return patientService.updatePatient(enrollmentRequestDto);
    }

    /**
     * Used to delete the patient
     * @param request
     * @return
     */
    @PostMapping("/delete")
    public void deletePatientByPatientId(@RequestBody RequestDTO request) {
        patientService.deletePatientByPatientId(request);
    }


    /**
     * Retrieves a list of patients based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link PatientRequestDTO} containing search criteria and returns
     * a list of patients that match the criteria.
     * </p>
     *
     * @param patientRequestDTO The search criteria for retrieving patients.
     * @return A {@link Map} containing the list of patients that match the criteria.
     */
    @PostMapping("/list")
    public Map<String, Object> getPatient(@RequestBody PatientRequestDTO patientRequestDTO) {
        return patientService.getPatientList(patientRequestDTO);
    }

    /**
     * Retrieves detailed information for a specific patient.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the ID of the patient and returns
     * detailed information about the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID.
     * @return A {@link PatientDetailsDTO} containing detailed information about the patient.
     */
    @PostMapping("/patientDetails")
    public PatientDetailsDTO getPatientDetails(@RequestBody RequestDTO requestDTO) {
        return patientService.getPatientDetails(requestDTO);
    }

    /**
     * Searches for patients based on the provided criteria.
     * <p>
     * This endpoint accepts a {@link PatientRequestDTO} containing search criteria and returns
     * a list of patients that match the criteria. If a filter is provided, it performs a more
     * specific search based on the filter.
     * </p>
     *
     * @param patientRequestDTO The search criteria for finding patients.
     * @return A {@link Map} containing the list of patients that match the criteria.
     */
    @PostMapping("/search")
    public Map<String, Object> searchPatient(@RequestBody PatientRequestDTO patientRequestDTO) {
        return Objects.isNull(patientRequestDTO.getFilter()) ? patientService.searchPatient(patientRequestDTO) :
                patientService.getPatientList(patientRequestDTO);
    }

    /**
     * Retrieves the status of a patient based on their ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient's ID and returns
     * the status of the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID.
     * @return A {@link Map} containing the status of the patient.
     */
    @PostMapping("/patient-status")
    public Map<String, Object> getPatientStatus(@RequestBody RequestDTO requestDTO) {
        requestDTO.setCategory(String.join(Constants.COMMA, List.of(Constants.ICCM, Constants.RMNCH, Constants.CHILDHOOD_VISIT)));
        requestDTO.setTicketType(requestDTO.getType());
        return patientService.getPatientStatus(requestDTO);
    }

    /**
     * Updates the status of a patient based on their ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient's ID and the new status
     * information, and updates the patient's status accordingly.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID and new status information.
     * @return A {@link ResponseEntity} containing a {@link FhirResponseDTO} with the result of the update operation.
     */
    @PostMapping("/update-patient-status")
    public ResponseEntity<FhirResponseDTO> updatePatientStatus(@RequestBody RequestDTO requestDTO) {
        return patientService.updateStatusOfServiceRequest(requestDTO);
    }

    /**
     * Creates a referral ticket for a patient based on their ID.
     * <p>
     * This endpoint accepts a {@link ReferralDetailsDTO} containing the patient's ID and creates
     * a referral ticket for medical review.
     * </p>
     *
     * @param referralTicketDTO The {@link ReferralDetailsDTO} containing the patient's ID.
     * @return A {@link ReferralDetailsDTO} containing the details of the created referral ticket.
     */
    @PostMapping("/referral-tickets/create")
    public ReferralDetailsDTO createReferralTicket(@RequestBody ReferralDetailsDTO referralTicketDTO) {
        return patientService.createReferralTicketForMedicalReview(referralTicketDTO,
                new Bundle().setType(Bundle.BundleType.TRANSACTION), Boolean.TRUE, Boolean.FALSE);
    }

    /**
     * Retrieves referral tickets for a patient based on their ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient's ID and returns a list
     * of referral tickets associated with the patient.
     * </p>
     *
     * @param requestDTO The {@link RequestDTO} containing the patient's ID.
     * @return A list of {@link ReferralTicketDTO} containing the referral tickets for the patient.
     */
    @PostMapping("/referral-tickets")
    public List<ReferralTicketDTO> getReferralTicket(@RequestBody RequestDTO requestDTO) {
        return patientService.getReferralTicketes(requestDTO.getPatientId(), requestDTO.getTicketId(),
                requestDTO.getType(), requestDTO.getTicketType());
    }

    /**
     * Updates the diagnosis for a patient.
     * <p>
     * This endpoint accepts a {@link DiagnosisDTO} containing the diagnosis information and updates
     * the patient's diagnosis accordingly.
     * </p>
     *
     * @param diagnosis The {@link DiagnosisDTO} containing the diagnosis information.
     */
    @PostMapping("/confirm-diagnosis")
    public void updatePatientDiagnosis(@RequestBody DiagnosisDTO diagnosis) {
        patientService.createDiagnosis(diagnosis);
    }

    /**
     * Retrieves the diagnosis details for a patient.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient's ID and returns a list
     * of diagnosis details for the patient.
     * </p>
     *
     * @param request The {@link RequestDTO} containing the patient's ID.
     * @return A list of {@link DiseaseDTO} containing the diagnosis details for the patient.
     */
    @PostMapping("/diagnosis-details")
    public List<DiseaseDTO> getPatientDiagnosis(@RequestBody RequestDTO request) {
        return patientService.getPatientDiagnosis(request, Boolean.FALSE, Boolean.TRUE);
    }

    /**
     * Retrieves pregnancy information for patients by village.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the village information and returns
     * a list of pregnancy information for patients in the specified village.
     * </p>
     *
     * @param request The {@link RequestDTO} containing the village information.
     * @return A list of {@link PregnancyInfo} containing the pregnancy information for patients.
     */
    @PostMapping("/pregnancy/info")
    public List<PregnancyInfo> getPregnancyInfoByVillages(@RequestBody RequestDTO request) {
        return patientService.getPregnancyInfoByVillages(request);
    }

    /**
     * This function update the pregnancy risk details based on the provided request data.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} It contains the  necessary information for the
     *                                                       pregnancy red risk details.
     * @return true if successfully updated, otherwise false
     */
    @PutMapping("/pregnancy-anc-risk/update")
    public Boolean updatePregnancyANCRisk(@RequestBody PregnancyDetailsDTO pregnancyDetailsDTO) {
        return patientService.updatePregnancyANCRisk(pregnancyDetailsDTO);
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
    public void updateReferredSite(@RequestBody ScreeningLogRequestDTO requestDTO) {
        patientService.updateReferredSite(requestDTO);
    }

    /**
     * Updates a referral ticket based on the member ID.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the member ID and updates the referral
     * ticket for the specified member.
     * </p>
     *
     * @param request The {@link RequestDTO} containing the member ID.
     */
    @PostMapping("/referral-tickets/update")
    public void updateReferralTicketByMemberId(@RequestBody RequestDTO request) {
        patientService.updateReferralTicketByMemberId(request, null);
    }

    /**
     * Get patient vitals information from member
     *
     * @param request - request data
     * @return Pregnancy Info map
     */
    @PostMapping("/get-patient-vitals")
    public PregnancyInfo getPatientVitals(@RequestBody RequestDTO request) {
        return patientService.getPatientVitals(request);
    }

    /**
     * Get patient details by national id
     *
     * @param request - PatientDetailsRequestDTO Entity
     * @return PatientDetailsResponseDTO entity contains patient details
     */
    @PostMapping("/ncd-patientDetails")
    public PatientDetailsDTO searchPatientDetails(@RequestBody RequestDTO request) {
        return patientService.searchPatientDetails(request);
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

    /**
     * Get patient details using search text
     *
     * @param request - SearchPatientsRequestDTO Entity
     * @return SearchPatientsResponseDTO entity contains searched patients details
     */
    @PostMapping("/ncd-search")
    public Map<String, Object> searchPatients(@RequestBody PatientRequestDTO request) {
        return patientService.searchPatients(request);
    }

    /**
     * Create pregnancy details to the patient.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} containing pregnancy details.
     * @return PatientPregnancyDetails Entity.
     */
    @PostMapping("/pregnancy/create")
    public PregnancyDetailsDTO createPregnancyDetails(@RequestBody PregnancyDetailsDTO pregnancyDetailsDTO) {
        Logger.logInfo("In Patient controller, creating a patient's pregnancy details");
        return patientService.createPregnancyDetails(pregnancyDetailsDTO);
    }

    /**
     * Retrieves a list of patients based on the provided request parameters.
     *
     * @param request the data transfer object containing the request parameters
     * @return a map containing the list of patients and a reference patient ID
     */
    @PostMapping("/ncd-list")
    public Map<String, Object> listPatient(@RequestBody PatientRequestDTO request) {
        return patientService.listPatients(request);
    }

    /**
     * Gets Pregnancy details of a patient.
     *
     * @param requestData Request data with patientTrackId
     * @return PregnancyDetailsDTO object.
     */
    @PostMapping("/pregnancy/details")
    public PregnancyDetailsDTO getPregnancyDetails(@RequestBody RequestDTO requestData) {
        Logger.logInfo("In Patient controller, getting a patient's pregnancy details");
        return patientService.getPregnancyDetails(requestData);
    }

    /**
     * Retrieves the diagnosis details for a patient.
     * <p>
     * This endpoint accepts a {@link RequestDTO} containing the patient's ID and returns a list
     * of diagnosis details for the patient.
     * </p>
     *
     * @param request The {@link RequestDTO} containing the patient's ID.
     * @return A list of {@link DiseaseDTO} containing the diagnosis details for the patient.
     */
    @PostMapping("/get-diagnosis-details")
    public ConfirmDiagnosisDTO getPatientDiagnosisDetails(@RequestBody RequestDTO request) {
        return patientService.getPatientDiagnosisDetails(request, Boolean.FALSE);
    }

    /**
     * <p>
     * Get patient data based on patient reference id.
     * </p>
     *
     * @param requestData Request data with patientTrackId
     * @return PatientDTO Response data with patient of given patient reference id
     */
    @PostMapping("/get-by-id")
    public PatientDTO getPatientById(@RequestBody RequestDTO requestData) {
        Logger.logInfo("In Patient controller, getting a patient's pregnancy details");
        PatientDTO patientDTO = new PatientDTO();
        Bundle bundle = patientService.getPatientDetailsByPatientReference(requestData.getPatientId());
        Patient patient = (Patient) bundle.getEntry().getFirst().getResource();
        return fhirMapper.toPatient(patient, patientDTO);
    }

    /**
     * Retrieves a map of patients based on the provided patient fhir ids.
     *
     * @param patientRequestDTO
     * @return map of patients.
     */
    @PostMapping("/ncd-search/list")
    public Map<String, PatientDetailsDTO> listPatients(@RequestBody PatientRequestDTO patientRequestDTO) {
        return patientService.listNcdPatients(patientRequestDTO);
    }
}
