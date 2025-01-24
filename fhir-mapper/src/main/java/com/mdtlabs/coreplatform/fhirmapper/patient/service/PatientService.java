package com.mdtlabs.coreplatform.fhirmapper.patient.service;

import java.util.List;
import java.util.Map;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Patient;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EncounterDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FhirResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;

public interface PatientService {


    /**
     * To get patient details
     *
     * @param patientRequestDTO A PatientDTO objects.
     * @return The method is returning a PatientRequestDTO object.
     */
    Map<String, Object> getPatientList(PatientRequestDTO patientRequestDTO);

    /**
     * To search patient details
     *
     * @param patientRequestDTO A PatientRequestDTO objects.
     * @return The method is returning a List of PatientDTO object.
     */
    Map<String, Object> searchPatient(PatientRequestDTO patientRequestDTO);

    /**
     * To get patient details
     *
     * @param requestDTO A PatientDTO objects.
     * @return The method is returning a PatientDTO object.
     */
    PatientDetailsDTO getPatientDetails(RequestDTO requestDTO);

    /**
     * Get Patient status by PatientId
     *
     * @param requestDTO patient Id
     * @return List of Status
     */
    public Map<String, Object> getPatientStatus(RequestDTO requestDTO);

    /**
     * Update Patient Status by PatientId
     *
     * @param requestDTO patient id and Details
     * @return FHIR response object
     */
    public ResponseEntity<FhirResponseDTO> updateStatusOfServiceRequest(RequestDTO requestDTO);

    /**
     * Create new Patient FHIR
     *
     * @param patientId member Details
     * @param bundle    Bundle Object
     * @return Patient reference
     */
    public Map<String, String> createPatient(String patientId, Bundle bundle, ProvenanceDTO provenanceDTO);

    /**
     * Update Patient FHIR
     *
     * @param enrollmentRequestDto EnrollmentRequestDTO
     * @return EnrollmentResponseDTO object.
     */
    public EnrollmentResponseDTO updatePatient(EnrollmentRequestDTO enrollmentRequestDto);

    /**
     * Get Patient Referral Ticket by PatientId
     *
     * @param patientId        patient Id
     * @param serviceRequestId service Request Id
     * @param type             type of the Ticket
     * @return List of Tickets
     */
    public List<ReferralTicketDTO> getReferralTicketes(String patientId, String serviceRequestId, String type,
                                                       String assessmentType);


    /**
     * Create Patient Referral Ticket by PatientId
     *
     * @param referralTicketDTO patient Id
     * @return List of Tickets
     */
    public ReferralDetailsDTO createReferralTicket(ReferralDetailsDTO referralTicketDTO, Bundle bundle,
                                                   boolean createRecord, boolean updatePrevious, Boolean isPregnant);


    /**
     * Create Patient Referral Ticket by PatientId
     *
     * @param referralTicketDTO patient Id
     * @return List of Tickets
     */
    public ReferralDetailsDTO createReferralTicketForMedicalReview(ReferralDetailsDTO referralTicketDTO, Bundle bundle,
                                                                   boolean createRecord, boolean updatePrevious);

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
    List<DiseaseDTO> getPatientDiagnosis(RequestDTO request, Boolean isMedicalReviewSummary, boolean includeAllCategories);

    /**
     * Get patient pregnancy information by villages
     *
     * @param request - RequestDTO Entity
     * @return PregnancyInfo list
     */
    List<PregnancyInfo> getPregnancyInfoByVillages(RequestDTO request);

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
    ConfirmDiagnosisDTO getPatientDiagnosisDetails(RequestDTO request, boolean isFromDetails);


    /**
     * Update patient overall status
     *
     * @param bundle           Bundle object
     * @param patientStatus    patientStatus
     * @param provenanceDTO    ProvenanceDTO
     * @param patientReference patientReference
     */
    void updatePatientOverallStatus(Bundle bundle, String patientStatus, ProvenanceDTO provenanceDTO,
                                    String patientReference);


    /**
     * Set patient reference in encounter details
     *
     * @param encounterDetails Encounter details DTO
     * @param bundle           Bundle object
     */
    void setPatientReferenceInEncounterDetails(EncounterDetailsDTO encounterDetails, Bundle bundle);

    /**
     * Create or update medical review encounter for patient
     *
     * @param encounterId
     * @param encounterDetailsDTO
     * @param encounterType
     * @param partOfEncounter
     * @param bundle
     * @return Encounter reference
     */
    String createOrUpdateMedicalReviewEncounter(String encounterId, EncounterDetailsDTO encounterDetailsDTO,
                                                String encounterType, String partOfEncounter, Bundle bundle);

    /**
     * Update referral ticket by member id
     *
     * @param request - RequestDTO Entity
     */
    void updateReferralTicketByMemberId(RequestDTO request, Bundle bundle);

    /**
     * Fetches a Patient resource from the FHIR server using the patient's ID.
     *
     * @param patientId The ID of the patient to be fetched.
     * @return The Patient resource fetched from the FHIR server.
     */
    Patient getPatientById(String patientId);

    /**
     * Creates a Patient resource in a FHIR Bundle by using patient id.
     *
     * @param request The RequestDTO object containing the Patient data.
     * @return The ID of the created Patient resource.
     */
    String createPatientByPatientId(RequestDTO request);

    /**
     * Delete the patient based on patient fhir id.
     *
     * @param request The RequestDTO object containing the Patient id and provenance data.
     * @return The ID of the created Patient resource.
     */
    void deletePatientByPatientId(RequestDTO request);

    /**
     * Updates the patient status of a Patient resource in a FHIR Bundle.
     * If the Patient resource is not found in the Bundle, it fetches the Patient from the FHIR server using the patient's ID.
     * It also updates the pregnancy status of a RelatedPerson resource associated with the patient.
     *
     * @param bundle          The FHIR Bundle in which the Patient resource's pregnancy status is to be updated.
     * @param pregnancyStatus The new pregnancy status to be set.
     * @param provenanceDTO   The Provenance data to be associated with the update operation.
     * @param patientId       The ID of the patient whose pregnancy status is to be updated.
     */
    void updatePatientStatus(Bundle bundle, Boolean pregnancyStatus, ProvenanceDTO provenanceDTO,
                             String patientId, Boolean isActive);

    /**
     * Get Patient details by Patient FHIR id
     *
     * @param id patientId value
     * @return Bundle Object
     */
    public Bundle getPatientDetailsByPatientReference(String id);

    /**
     * Get Patient Pregnancy Details
     *
     * @param memberIds
     * @return Map of PregnancyInfo
     */
    Map<String, PregnancyInfo> getPregnancyInfoFromPatientVitals(List<String> memberIds);

    /**
     * Get patient vitals information from member
     *
     * @param request - request data
     * @return Pregnancy Info map
     */
    PregnancyInfo getPatientVitals(RequestDTO request);

    /**
     * Close previous PNC details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    void closePncDetails(Bundle bundle, RequestDTO requestDTO);

    /**
     * Close previous PNC Neonate details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    void closePncNeonateDetails(Bundle bundle, RequestDTO requestDTO);

    /**
     * Close previous ANC details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    void closeAncDetails(Bundle bundle, RequestDTO requestDTO);

    /**
     * Close previous childhood details
     *
     * @param requestDTO Assessment Details
     * @param bundle     Bundle object
     */
    void closeChildhoodDetails(Bundle bundle, RequestDTO requestDTO);

    /**
     * Close All referral tickets
     *
     * @param bundle         Bundle Object
     * @param memberId       MemberId
     * @param encounterId    EncounterId
     * @param provenance     provenanceDetails
     * @param assessmentType assessmentType
     */
    void handlePatientDeath(String assessmentType, Bundle bundle, String memberId, String encounterId,
                            ProvenanceDTO provenance);

    /**
     * Get patient details by national id
     *
     * @param request - PatientDetailsRequestDTO Entity
     * @return PatientDetailsResponseDTO entity contains patient details
     */
    PatientDetailsDTO searchPatientDetails(RequestDTO request);

    /**
     * Get patient details by village ids
     *
     * @param requestDTO - RequestDTO Entity
     * @return List of PatientDetailsDTO entity contains patient details
     */
    List<PatientDetailsDTO> getPatientDetailsByVillageIds(RequestDTO requestDTO);

    /**
     * Get patient details using search text and if the type is enrollment returns not-enrolled patients
     *
     * @param request - SearchPatientsRequestDTO Entity
     * @return SearchPatientsResponseDTO entity contains searched patients details
     */
    Map<String, Object> searchPatients(PatientRequestDTO request);

    /**
     * Create pregnancy details to the patient.
     *
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} containing pregnancy details.
     * @return PatientPregnancyDetails Entity.
     */
    PregnancyDetailsDTO createPregnancyDetails(PregnancyDetailsDTO pregnancyDetailsDTO);

    /**
     * Retrieves a list of patients based on the provided request parameters.
     *
     * @param request the data transfer object containing the request parameters
     * @return a map containing the list of patients and a reference patient ID
     */
    Map<String, Object> listPatients(PatientRequestDTO request);

    /**
     * Retrieves a map of patients based on the provided patient fhir ids.
     *
     * @param patientRequestDTO .
     * @return map of patients.
     */
    Map<String, PatientDetailsDTO> listNcdPatients(PatientRequestDTO patientRequestDTO);

    /**
     * Fetch the patient pregnancy details
     *
     * @param requestData {@link RequestDTO} to fetch the patient pregnancy details
     * @return {@link PregnancyDetailsDTO} object
     */
    PregnancyDetailsDTO getPregnancyDetails(RequestDTO requestData);

    /**
     * <p>
     * This method is used to update pregnancy ANC risk status to the patient.
     * </p>
     *
     * @param pregnancyDetailsDTO - pregnancy details
     * @return Boolean - boolean
     */
    Boolean updatePregnancyANCRisk(PregnancyDetailsDTO pregnancyDetailsDTO);

    /**
     * <p>
     * This function is to update referred site in patient.
     * </p>
     *
     * @param requestDTO {@link ScreeningLogRequestDTO} It contains the  necessary information to
     *                   update referred site.
     */
    void updateReferredSite(ScreeningLogRequestDTO requestDTO);

    /**
     * Create patient by member reference
     *
     * @param memberReference - member reference
     * @param provenanceDTO   - provenance details
     * @param bundle          - bundle object
     * @return Patient - patient object
     */
    Patient createPatientByMemberReference(String memberReference, ProvenanceDTO provenanceDTO, Bundle bundle);

}
