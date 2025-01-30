package com.mdtlabs.coreplatform.spiceservice.patient.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.WgsDataDTO;

/**
 * <p>
 * This class is a service class to perform operation on Patient
 * operations.
 * </p>
 *
 * @author Nanthinee sugumar created on Feb 28, 2024
 */
public interface PatientService {

    /**
     * To get patient details
     *
     * @param patientRequestDTO A PatientDTO objects.
     * @return The method is returning a PatientRequestDTO object.
     */
    Map<String, Object> searchPatient(PatientRequestDTO patientRequestDTO);

    /**
     * To get patient details
     *
     * @param patientDTO A PatientDTO objects.
     * @return The method is returning a PatientDTO object.
     */
    PatientDetailsDTO getPatientDetails(PatientDTO patientDTO);

    /**
     * To get patient list
     *
     * @param patientRequestDTO A PatientDTO objects.
     * @return The method is returning a PatientRequestDTO object.
     */
    Map<String, Object> getPatientList(PatientRequestDTO patientRequestDTO);

    /**
     * Get Patient status by PatientId
     *
     * @param requestDTO patient Id
     * @return Status
     */
    Map<String, Object> getPatientStatus(RequestDTO requestDTO);

    /**
     * Update Patient Status by PatientId
     *
     * @param requestDTO patient id and Details
     * @return FHIR response object
     */
    Boolean updateStatusOfServiceRequest(RequestDTO requestDTO);

    /**
     * Get Patient Referral Ticket by PatientId
     *
     * @param requestDTO patient Id
     * @return List of Tickets
     */
    List<ReferralTicketDTO> getReferralTickets(RequestDTO requestDTO);


    /**
     * Create Patient Referral Ticket by PatientId
     *
     * @param referralTicketDTO patient Id
     * @return List of Tickets
     */
    ReferralDetailsDTO createReferralTicket(ReferralDetailsDTO referralTicketDTO);

    /**
     * Creates a Patient resource in a FHIR Bundle by using patient id.
     *
     * @param request The RequestDTO object containing the Patient data.
     * @return The ID of the created Patient resource.
     */
    String createPatientByPatientId(RequestDTO request);

    /**
     * Update a Patient resource in a FHIR Bundle by using patient id.
     *
     * @param enrollmentRequestDto The RequestDTO object containing the Patient data.
     * @return EnrollmentResponse object.
     */
    EnrollmentResponseDTO updatePatient(EnrollmentRequestDTO enrollmentRequestDto);

    /**
     * Creates patient diagnosis.
     *
     * @param diagnosis
     */
    void createDiagnosis(DiagnosisDTO diagnosis);

    /**
     * Gets a patient diagnosis.
     *
     * @param request
     * @return
     */
    List<DiseaseDTO> getPatientDiagnosis(RequestDTO request);

    /**
     * Retrieves diagnosis details for a patient.
     * <p>
     * This method accepts a {@link RequestDTO} identifying the patient and returns detailed information
     * about the patient's diagnosis.
     * </p>
     *
     * @param request The {@link RequestDTO} identifying the patient.
     * @return A {@link ConfirmDiagnosisDTO} containing detailed diagnosis information.
     */
    ConfirmDiagnosisDTO getPatientDiagnosisDetails(RequestDTO request);

    /**
     * Get patient pregnancy information by villages
     *
     * @param request - RequestDTO Entity
     * @return PregnancyInfo list
     */
    public List<PregnancyInfo> getPregnancyInfoByVillages(RequestDTO request);

    /**
     * Get patient details by national id
     *
     * @param requestDTO - PatientDetailsRequestDTO Entity
     * @return Map of search patient details
     */
    Map<String, Object> searchPatients(PatientRequestDTO requestDTO);

    /**
     * Get patient details using patient id
     *
     * @param patientDTO - PatientDTO Entity
     * @return PatientDetailsDTO entity contains searched patient details
     */
    PatientDetailsDTO searchPatientDetails(PatientDTO patientDTO);

    /**
     * Create pregnancy details to the patient.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} containing pregnancy details.
     * @return PatientPregnancyDetails Entity.
     */
    PregnancyDetailsDTO createPregnancyDetails(PregnancyDetailsDTO pregnancyDetailsDTO);

    /**
     * Gets Pregnancy details of a patient.
     *
     * @param requestData Request data with patientTrackId
     * @return PregnancyDetailsDTO object.
     */
    PregnancyDetailsDTO getPregnancyDetails(RequestDTO requestData);

    /**
     * Delete the patient based on patient fhir id.
     *
     * @param request The RequestDTO object containing the Patient id and provenance data.
     * @return The ID of the created Patient resource.
     */
    void deletePatientByPatientId(RequestDTO request);

    /**
     * Update the pregnancy anc risk
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} updating pregnancy anc details
     * @return true, if the pregnancy anc risk is updated otherwise returns false.
     */
    Boolean updatePregnancyANCRisk(PregnancyDetailsDTO pregnancyDetailsDTO);

    /**
     * <p>
     * This function is to update referred site in patient.
     * </p>
     *
     * @param requestDTO {@link ScreeningLogRequestDTO} It contains the  necessary information to
     *                                                   update referred site.
     */
    void updateReferredSite(ScreeningLogRequestDTO requestDTO);

    /**
     * <p>
     * Retrieves the calculated growth standards data for a patient based on the provided WgsDataDTO.
     * </p>
     *
     * @param wgsDataDTO - Contains the input params to get WGS data.
     * @return - Returns a SuccessResponse containing a map of calculated WGS data for the patient
     */
    Map<String, Object> getPatientWgsData(WgsDataDTO wgsDataDTO);

    /**
     * Get patient details by village ids
     *
     * @param requestDTO - RequestDTO Entity
     * @return List of PatientDetailsDTO entity contains patient details
     */
    List<PatientDetailsDTO> getPatientDetailsByVillageIds(RequestDTO requestDTO);
}
