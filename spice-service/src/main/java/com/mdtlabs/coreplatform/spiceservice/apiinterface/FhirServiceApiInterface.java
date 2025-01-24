package com.mdtlabs.coreplatform.spiceservice.apiinterface;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.mdtlabs.coreplatform.spiceservice.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanResponseDTO;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import feign.FeignException;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.FeignConfig;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiagnosisDTO.DiseaseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.EnrollmentResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FilterRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyle;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientNutritionLifestyleUpdateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientValidationResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PerformanceReport;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PncMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PregnancyInfo;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ReferralTicketDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanResponseDTO;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * This API interface used to interact with FHIR mapper
 *
 * @author Nandhakumar
 */
@FeignClient(name = "fhir-service", url = "${app.fhir-service}", configuration = FeignConfig.class)
public interface FhirServiceApiInterface {

    /**
     * Create new Household in FHIR DB
     *
     * @param token        auth token
     * @param client       auth client
     * @param householdDTO Request DTO
     * @return Household Object
     * @throws FeignException
     */
    @PostMapping("/household/create")
    public HouseholdDTO createHouseHold(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdDTO householdDTO) throws FeignException;

    /**
     * Create new Household Member in FHIR DB
     *
     * @param token              auth token
     * @param client             auth client
     * @param householdMemberDTO Request DTO
     * @return HouseholdMemberDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/create-member")
    public HouseholdMemberDTO createHouseHoldMember(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdMemberDTO householdMemberDTO) throws FeignException;

    /**
     * Update  Household in FHIR DB
     *
     * @param token        auth token
     * @param client       auth client
     * @param householdDTO Request DTO
     * @return HouseholdDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/update")
    public HouseholdDTO updateHouseHold(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdDTO householdDTO) throws FeignException;

    /**
     * Get Household Member by member id
     *
     * @param token   auth token
     * @param client  auth client
     * @param request Request DTO
     * @return HouseholdMemberDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/member/id")
    public HouseholdMemberDTO getHouseholdMemberById(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request) throws FeignException;

    /**
     * Get Household Member by patient id
     *
     * @param token   auth token
     * @param client  auth client
     * @param request Request DTO
     * @return HouseholdMemberDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/member/patient-id")
    public HouseholdMemberDTO getHouseholdMemberByPatientId(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request) throws FeignException;

    /**
     * Update  Household Member in FHIR DB
     *
     * @param token              auth token
     * @param client             auth client
     * @param householdMemberDTO Request DTO
     * @return HouseholdMemberDTO Object
     * @throws FeignException
     */
    @PostMapping("/household/update-member")
    public HouseholdMemberDTO updateHouseHoldMember(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody HouseholdMemberDTO householdMemberDTO) throws FeignException;

    /**
     * Get household from FHIR Db
     *
     * @param token       auth tokeb
     * @param client      auth client
     * @param householdId id
     * @return HouseholdDTO object
     * @throws FeignException
     */
    @GetMapping("/household/${householdId}")
    public HouseholdDTO getHouseHold(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @PathVariable(value = "householdId") String householdId) throws FeignException;

    /**
     * To create Assessment
     *
     * @param authToken     auth token
     * @param client        auth client
     * @param assessmentDTO assessment Details
     * @return Assessment Details
     */
    @PostMapping("/assessment/create")
    public AssessmentDTO assessmentCreate(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody AssessmentDTO assessmentDTO) throws FeignException;

    /**
     * To create screening log
     *
     * @param authToken     auth token
     * @param client        auth client
     * @param screeningDTO  screening log details
     * @return created screening log details
     */
    @PostMapping("/screening/create")
    BioDataDTO screeningCreate(@RequestHeader("Authorization") String authToken,
                               @RequestHeader("client") String client,
                               @RequestBody ScreeningLogRequestDTO screeningDTO) throws FeignException;

    @PostMapping("/patient/ncd-patientDetails")
    public PatientDetailsDTO searchPatientDetails(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody PatientDTO requestDTO) throws FeignException;


    @PostMapping("/patient/ncd-search")
    public Map<String, Object> searchPatients(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody PatientRequestDTO requestDTO) throws FeignException;

    /**
     * Search  Patient in FHIR DB
     *
     * @param token             auth token
     * @param client            auth client
     * @param patientRequestDTO Request DTO
     * @return PatientDTO Object
     */
    @PostMapping("/patient/search")
    public Map<String, Object> searchPatient(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody PatientRequestDTO patientRequestDTO) throws FeignException;

    /**
     * Creates a Patient resource in a FHIR Bundle by using patient id.
     *
     * @param token   auth token
     * @param client  auth client
     * @param request Request DTO
     * @return The ID of the created Patient resource.
     */
    @PostMapping("/patient/create")
    public String createPatientByPatientId(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request) throws FeignException;

    /**
     * Update a Patient resource in a FHIR Bundle by using id.
     *
     * @param token      The authorization token provided in the request header.
     * @param client     The client identifier provided in the request header.
     * @param enrollmentRequestDto The EnrollmentRequestDTO containing patient information for update.
     * @return enrollmentResponseDTO An EnrollmentResponseDTO containing updated patient information.
     */
    @PostMapping("/patient/update")
    public EnrollmentResponseDTO updatePatient(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody EnrollmentRequestDTO enrollmentRequestDto) throws FeignException;

    /**
     * To Get referral Tickets
     *
     * @param authToken  auth token
     * @param client     auth client
     * @param requestDTO assessment Details
     * @return Ticket Details
     */
    @PostMapping("/patient/referral-tickets")
    public List<ReferralTicketDTO> getReferralTickets(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * To Get referral Tickets
     *
     * @param authToken  auth token
     * @param client     auth client
     * @param requestDTO assessment Details
     * @return Ticket Details
     */
    @PostMapping("/patient/referral-tickets/create")
    public ReferralDetailsDTO createReferralTicket(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody
    ReferralDetailsDTO requestDTO) throws FeignException;

    /**
     * Update referral ticket by member id
     *
     * @param authToken  auth token
     * @param client     auth client
     * @param requestDTO request Details
     */
    @PostMapping("/patient/referral-tickets/update")
    public void updateReferralTicketByMemberId(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody
    RequestDTO requestDTO) throws FeignException;

    /**
     * Search  Patient in FHIR DB
     *
     * @param token      auth token
     * @param client     auth client
     * @param patientDTO Request DTO
     * @return PatientDTO Object
     * @throws FeignException
     */
    @PostMapping("/patient/patientDetails")
    public PatientDetailsDTO getPatientDetails(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody PatientDTO patientDTO) throws FeignException;

    /**
     * To Get list of patients
     *
     * @param token             auth token
     * @param client            auth client
     * @param patientRequestDTO Request DTO
     * @return PatientDTO Object
     * @throws FeignException
     */
    @PostMapping("/patient/list")
    public Map<String, Object> getPatientList(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody PatientRequestDTO patientRequestDTO) throws FeignException;

    /**
     * To Get list of patients
     *
     * @param token             auth token
     * @param client            auth client
     * @param patientRequestDTO Request DTO
     * @return PatientDTO Object
     * @throws FeignException
     */
    @PostMapping("/patient/ncd-list")
    Map<String, Object> getPatientNcdList(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody PatientRequestDTO patientRequestDTO) throws FeignException;

    /**
     * Get patient vitals information from member
     *
     * @param token   auth token
     * @param client  auth client
     * @param request Request Data
     * @return PregnancyInfo Object
     * @throws FeignException
     */
    @PostMapping("/patient/get-patient-vitals")
    public PregnancyInfo getPatientVitals(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request) throws FeignException;

    /**
     * To Get patient status
     *
     * @param authToken  auth token
     * @param client     auth client
     * @param requestDTO request Details
     * @return Ticket Details
     */
    @PostMapping("/patient/patient-status")
    public Map<String, Object> getPatientStatus(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * To update patient status
     *
     * @param authToken  auth token
     * @param client     auth client
     * @param requestDTO request Details
     * @return Ticket Details
     */
    @PostMapping("/patient/update-patient-status")
    public ResponseEntity<Boolean> updatePatientStatus(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Retrieve household list from FHIR DB
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return Household List
     * @throws FeignException
     */
    @PostMapping("/household/list")
    public List<HouseholdDTO> getHouseholdList(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Retrieve household member list from FHIR DB
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return Household member List
     * @throws FeignException
     */
    @PostMapping("/household/member/list")
    public List<HouseholdMemberDTO> getHouseholdMemberList(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Update Household member signature
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return HouseholdMemberDTO
     * @throws FeignException
     */
    @PostMapping("/household/member/update-signature")
    public HouseholdMemberDTO updateSignature(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;


    /**
     * Create Medical Review ICCM general
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Medical Review Details
     * @return GeneralMedicalReviewDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/iccm-general/create")
    public Map<String, String> createIccmGeneralMedialReview(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody GeneralMedicalReviewDTO requestDTO) throws FeignException;

    /**
     * Get Medical Review ICCM general
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Medical Review Id
     * @return GeneralMedicalReviewSummaryDetailsDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/iccm-general/details")
    public GeneralMedicalReviewSummaryDetailsDTO getIccmGeneralMedialReview(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Create Medical Review ICCM general Summary
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Medical Review Id
     * @return GeneralMedicalReviewSummaryDetailsDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/summary-create")
    public GeneralMedicalReviewSummaryDTO createMedicalReviewSummary(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody GeneralMedicalReviewSummaryDTO requestDTO) throws FeignException;

    /**
     * Get Medical Review ICCM general Summary
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO MedicalReviewRequestDTO
     * @return MedicalReviewHistoryDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/history")
    MedicalReviewHistoryDTO getMedicalReviewHistory(@RequestHeader("Authorization") String token,
                                                    @RequestHeader("client") String client,
                                                    @RequestBody MedicalReviewRequestDTO requestDTO) throws FeignException;

    /**
     * Get All PNC Medical Review of Child
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO MedicalReviewRequestDTO
     * @return MedicalReviewHistoryDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/pnc-history")
    MedicalReviewHistoryDTO getPncMedicalReviewHistory(@RequestHeader("Authorization") String token,
                                                       @RequestHeader("client") String client,
                                                       @RequestBody MedicalReviewRequestDTO requestDTO) throws FeignException;

    /**
     * Creates iccm medical review for under two months
     *
     * @param iccmdto
     * @return Map<String, String> response
     */
    @PostMapping("/medical-review/iccm-under-2months/create")
    public Map<String, String> createICCMUnder2months(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody UnderFiveIccmDTO iccmdto) throws FeignException;

    /**
     * Endpoint for creating a value observation related to ANC pregnancy, specifically for weight and blood pressure.
     *
     * @param token      The authorization token provided in the request header.
     * @param client     The client identifier provided in the request header.
     * @param requestDTO The ObservationDTO containing information for the value observation to be created.
     * @return A ResponseEntity containing a FhirResponseDTO with the result of the operation.
     * @throws FeignException in case of errors during communication with the backend service.
     */
    @PostMapping("/medical-review/bp/create")
    public ObservationDTO createBp(@RequestHeader("Authorization") String token,
                                   @RequestHeader("client") String client, @RequestBody ObservationDTO requestDTO) throws FeignException;

    /**
     * Endpoint for creating a value observation related to ANC pregnancy, specifically for weight and blood pressure.
     *
     * @param token      The authorization token provided in the request header.
     * @param client     The client identifier provided in the request header.
     * @param requestDTO The ObservationDTO containing information for the value observation to be created.
     * @return A ResponseEntity containing a FhirResponseDTO with the result of the operation.
     * @throws FeignException in case of errors during communication with the backend service.
     */
    @PostMapping("/medical-review/weight/create")
    public ObservationDTO createWeight(@RequestHeader("Authorization") String token,
                                       @RequestHeader("client") String client, @RequestBody ObservationDTO requestDTO) throws FeignException;

    /**
     * Endpoint for creating a general medical review related to ANC pregnancy.
     *
     * @param token      The authorization token provided in the request header.
     * @param client     The client identifier provided in the request header.
     * @param requestDTO The MedicalReviewDTO containing information for the medical review to be created.
     * @return The created MedicalReviewDTO.
     * @throws FeignException in case of errors during communication with the backend service.
     */
    @PostMapping("/medical-review/anc-pregnancy/create")
    public Map<String, String> createMedicalReview(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody MedicalReviewPregnancyDTO requestDTO) throws FeignException;

    /**
     * Endpoint for creating a medical review for labor, mother, and neonate based on the provided data.
     *
     * @param token      The authorization token for authentication.
     * @param client     The client identifier for the request.
     * @param requestDTO The data containing information about labor, mother, and neonate.
     * @return A MotherNeonateDTO representing the created medical review.
     * @throws FeignException if an error occurs during communication with the service.
     */
    @PostMapping("/medical-review/labour-mother-neonate/create")
    public Map<String, String> createLabourMotherAndNeonateMedicalReview(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody MotherNeonateDTO requestDTO) throws FeignException;

    /**
     * Endpoint for retrieving details of a medical review related to ANC pregnancy.
     *
     * @param token      The authorization token provided in the request header.
     * @param client     The client identifier provided in the request header.
     * @param requestDTO The RequestDTO containing necessary information for fetching pregnancy medical review details.
     * @return The details of the pregnancy medical review.
     * @throws FeignException in case of errors during communication with the backend service.
     */
    @PostMapping("/medical-review/anc-pregnancy/details")
    public MedicalReviewPregnancySummaryDetailsDTO getPregnancyMedicalReviewDetails(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Endpoint for retrieving details of labor, mother, and neonate based on the provided request data.
     *
     * @param token      The authorization token for authentication.
     * @param client     The client identifier for the request.
     * @param requestDTO The data containing necessary information to retrieve the details.
     * @return A MotherNeonateDTO containing the details of labor, mother, and neonate.
     * @throws FeignException if an error occurs during communication with the service.
     */
    @PostMapping("/medical-review/labour-mother-neonate/details")
    public MotherNeonateDTO getLabourMotherAndNeonateDetails(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Gets a medical review details of under two months.
     *
     * @param request
     * @return Map<String, Object>
     */
    @PostMapping("/medical-review/iccm-under-2months/details")
    public IccmResponseDTO getIccmUnder2MSummary(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody MedicalReviewRequestDTO request) throws FeignException;

    /**
     * Creates iccm medical review for two months to five years.
     *
     * @param iccmdto
     * @return Map<String, String> response
     */
    @PostMapping("/medical-review/iccm-under-5years/create")
    public Map<String, String> createICCMUnder5Years(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody UnderFiveIccmDTO iccmdto) throws FeignException;

    /**
     * Gets a medical review details of two months to five years.
     *
     * @param request
     * @return IccmResponseDTO
     */
    @PostMapping("/medical-review/iccm-under-5years/details")
    public IccmResponseDTO getIccmUnder5YSummary(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody MedicalReviewRequestDTO request) throws FeignException;

    /**
     * Endpoint for listing prescription
     * This method handles HTTP POST requests to listing MedicationRequest.
     *
     * @param requestDTO The Request DTO containing Prescription details to be updated.
     */
    @PostMapping("/prescription-request/list")
    public List<PrescriptionDTO> getPrescriptions(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Endpoint for listing prescription
     * This method handles HTTP POST requests to listing MedicationRequest.
     *
     * @param requestDTO The Request DTO containing Prescription details to be updated.
     */
    @PostMapping("/prescription-request/ncd-list")
    public List<PrescriptionDTO> getNcdPrescriptions(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Create Medical Review PNC
     *
     * @param token                  auth Token
     * @param client                 auth Client
     * @param prescriptionRequestDTO PrescriptionRequestDTO
     * @throws FeignException
     */
    @PostMapping("/prescription-request/remove")
    public void removePrescription(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestHeader("tenantId") Long tenantId, @RequestBody PrescriptionRequestDTO prescriptionRequestDTO) throws FeignException;

    /**
     * Create Medical Review PNC
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Pnc MedicalReviewDetails
     * @return PncMedicalReviewDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/pnc/create")
    Map<String, String> createPncMedicalReview(@RequestHeader("Authorization") String token,
                                               @RequestHeader("client") String client,
                                               @RequestBody PncMedicalReviewDTO requestDTO) throws FeignException;

    /**
     * Create Medical Review PNC child
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param householdMemberDTO household member details
     * @return HouseholdMemberDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/pnc-child-create")
    HouseholdMemberDTO createChild(@RequestHeader("Authorization") String token,
                                               @RequestHeader("client") String client,
                                               @RequestBody HouseholdMemberDTO householdMemberDTO) throws FeignException;

    /**
     * Get PNC Medical Review Details
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Medical Review Id
     * @return GeneralMedicalReviewSummaryDetailsDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/pnc/details")
    PncMedicalReviewDTO getPncDetails(@RequestHeader("Authorization") String token,
                                      @RequestHeader("client") String client,
                                      @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get weight from Medical Review Details
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Medical Review Id
     * @return GeneralMedicalReviewSummaryDetailsDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/weight")
    Map<String, Double> getWeight(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                                  @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get Bp value from Medical Review Details
     *
     * @param token      auth Token
     * @param client     auth Client
     * @param requestDTO Medical Review Id
     * @return GeneralMedicalReviewSummaryDetailsDTO
     * @throws FeignException
     */
    @PostMapping("/medical-review/bp")
    Map<String, Double> getBp(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                              @RequestBody RequestDTO requestDTO) throws FeignException;

    /* *
     * Creates patient Diagnosis.
     *
     * @param diagnosis
     * @return
     */
    @PostMapping("/patient/confirm-diagnosis")
    public void updatePatientDiagnosis(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody DiagnosisDTO diagnosis);

    /**
     * Gets patient Diagnosis.
     *
     * @param request
     * @return List<DiseaseDTO>
     */
    @PostMapping("/patient/diagnosis-details")
    public List<DiseaseDTO> getPatientDiagnosis(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request);

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
    @PostMapping("/patient/get-diagnosis-details")
    public ConfirmDiagnosisDTO getPatientDiagnosisDetails(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request);


    /**
     * Endpoint for listing Medication request
     * This method handles HTTP POST requests to listing MedicationRequest.
     *
     * @param prescriptionRequestDTO The prescriptionRequestDTO containing Prescription details to be updated.
     */
    @PostMapping("/prescription-request/update")
    Map<String, String> updatePrescriptionRequest(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody PrescriptionRequestDTO prescriptionRequestDTO) throws FeignException;

    /**
     * Endpoint for listing Medication request
     * This method handles HTTP POST requests to listing MedicationRequest.
     *
     * @param prescriptionRequestDTO The prescriptionRequestDTO containing Prescription details to be updated.
     */
    @PostMapping("/prescription-request/ncd-update")
    Map<String, String> updateNcdPrescriptionRequest(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody PrescriptionRequestDTO prescriptionRequestDTO) throws FeignException;

    /**
     * Get patient pregnancy information by villages
     *
     * @param token      auth token
     * @param client     auth client
     * @param requestDTO Request DTO
     * @return PregnancyInfo List
     * @throws FeignException
     */
    @PostMapping("/patient/pregnancy/info")
    public List<PregnancyInfo> getPregnancyInfoByVillages(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Gets prescription Details.
     *
     * @param token
     * @param client
     * @param requestDTO
     * @return
     * @throws FeignException
     */
    @PostMapping("/prescription-request/prescribed-details")
    public PrescriptionHistoryDTO getPrescribedDetails(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestHeader("tenantId") Long tenantId, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get prescription history list.
     *
     * @param token
     * @param client
     * @param requestDTO
     * @return
     * @throws FeignException
     */
    @PostMapping("/prescription-request/history-list")
    public List<PrescriptionDTO> getPrescriptionHistoryList(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestHeader("tenantId") Long tenantId, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * Get Birth-History Details
     *
     * @param requestDTO the requestDTO
     * @return BirthHistoryDTO Details
     */
    @PostMapping("/medical-review/birth-history")
    BirthHistoryDTO getBirthHistory(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO);

    /**
     * <p>
     *  To create or update an investigation details.
     * </p>
     *
     * @param token authorization token
     * @param client client
     * @param requestDTO investigation details
     * @return response
     */
    @PostMapping("/investigation/create")
    Map<String, String> createOrUpdateInvestigation(@RequestHeader("Authorization") String token,
                                                    @RequestHeader("client") String client, @RequestHeader("tenantId") Long tenantId, @RequestBody LabTestRequestDTO requestDTO);

    /**
     * <p>
     *  To retrieve or get the investigation details.
     * </p>
     *
     * @param token authorization token
     * @param client client
     * @param requestDTO investigation details
     * @return response
     */
    @PostMapping("investigation/details")
    LabTestDTO getInvestigatedDetails(@RequestHeader("Authorization") String token,
                                      @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO);

    /**
     * <p>
     *  To retrieve the investigation details by encounter.
     * </p>
     *
     * @param token authorization token
     * @param client client
     * @param requestDTO investigation details
     * @return response
     */
    @PostMapping("/investigation/list")
    List<LabTestDTO> getInvestigationsByEncounter(@RequestHeader("Authorization") String token,
                             @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO);

    /**
     *  <p>
     *      To remove the investigation details by using investigation id.
     *  <p/>
     * @param token authentication token
     * @param client client
     * @param requestDTO investigation details
     * @return response
     */
    @PostMapping("investigation/remove")
    Map<String,String> removeInvestigation(@RequestHeader("Authorization") String token,
                             @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO);
                             
    /* 
     * Gets performance monitoring report data for the given request.
     *
     * @param token   - Token required for communication.
     * @param client  - Client type in the header.
     * @param request - Filter request that contains the actual request for fetching the user response.
     * @return Map      - Performance monitoring report data for the given filter.
     */
    @PostMapping("/report/chw-performance")
    Map<String, Map<String, PerformanceReport>> getPerformanceMonitoringReport(
            @RequestHeader("Authorization") String token, @RequestHeader("client") String client,
            @RequestBody FilterRequestDTO request);

    /**
     * <p>
     *     To get the history details of investigation based on the patient information.
     * </p>
     * @param token authentication token
     * @param client client
     * @param requestDTO investigation details
     * @return response details of investigation
     */
    @PostMapping("investigation/history-list")
    LabTestHistoryDTO getHistoryInvestigatedDetails(@RequestHeader("Authorization") String token,
                             @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO,
                                                    @RequestHeader("tenantId") Long tenantId);

    /**
     * <p>
     *     To create pnc medical review child member details.
     * </p>
     * @param token authentication token
     * @param client client
     * @param householdMemberDTO member details
     */
    @PostMapping("/medical-review/pnc-child/create")
    public void createPncChild(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                        @RequestBody HouseholdMemberDTO householdMemberDTO) throws FeignException;
                        
    @PostMapping("/medical-review/birth-history/{encounterId}")
    BirthHistoryDTO getBirthHistory(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @PathVariable("encounterId") String encounterId);

    /** 
     * Registers patient and member.
     * 
     * @param request
     * @return EnrollmentResponseDTO
     */
    @PostMapping("/patient/register")
    public EnrollmentResponseDTO registerPatientAndMember(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody EnrollmentRequestDTO request);

    /**
     * Validates patient using national-id.
     *
     * @param request
     * @return PatientValidationResponseDTO
     */
    @PostMapping("/patient/validate")
    public PatientValidationResponseDTO validatePatient(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody BioDataDTO request);

    /**
     * Registers patient and member.
     * 
     * @param request
     * @return EnrollmentResponseDTO
     */
    @PostMapping("/patient-treatment-plan/details")
    public TreatmentPlanResponseDTO getPatientTreatmentPlan(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO request);

    /** 
     * Updates patient treatment plan.
     * 
     * @param request
     * @return EnrollmentResponseDTO
     */
    @PostMapping("/patient-treatment-plan/update")
    public void updateTreatmentPlanData(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody TreatmentPlanDTO treatmentPlanDT);

    /**
     * <p>
     * This method is used to create the patient assessment in fhir server
     * <p/>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given
     * @param screeningDTO {@link ScreeningLogRequestDTO} screening dto is given
     * @return {@link ScreeningLog} Screening details for the patient
     * @throws FeignException Throws the error if something went wrong
     */
    @PostMapping("/screening/get-by-person-id")
    ScreeningLog getScreeningLog(@RequestHeader("Authorization") String authToken,
                                 @RequestHeader("client") String client,
                                 @RequestBody ScreeningLogRequestDTO screeningDTO) throws FeignException;

    /**
     * <p>
     * Used to create or update the patient status
     * <p/>
     *
     * @param token - {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param request - Patient status details.
     */
    @PostMapping("/medical-review/patient-status/create")
    public PatientStatusDTO createPatientStatus(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                                                @RequestBody PatientStatusDTO request);

    /**
     * <p>
     * Used to get the patient status details based on the patient id.
     * <p/>
     *
     * @param token - {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param request - Patient id to get the patient status.
     * @return {@link PatientStatusDTO} - Patient status details
     */
    @PostMapping("/medical-review/patient-status/details")
    PatientStatusDTO getPatientStatusDetails(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                                    @RequestBody PatientStatusDTO request);

    @PostMapping("/patientvisit/create")
    Map<String, Object> createPatientVisit(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client,
                                        @RequestBody PatientVisitDTO patientVisit) throws FeignException;

    /**
     * <p>
     * This method is used to create the patient bp logs in fhir server
     * <p/>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given
     * @param requestDTO {@link RequestDTO} screening dto is given
     * @return {@link PatientBpLogsDTO} Patient bp log details
     * @throws FeignException Throws the error if something went wrong
     */
    @PostMapping("/bplog/list")
    PatientBpLogsDTO getBpLogList(@RequestHeader("Authorization") String authToken,
                                  @RequestHeader("client") String client,
                                  @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * <p>
     * This method is used to create the patient glucose logs in fhir server
     * <p/>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given
     * @param client - client to authenticate.
     * @param requestDTO {@link RequestDTO} request dto is given
     * @return {@link PatientGlucoseLogDTO} Patient glucose log details
     * @throws FeignException Throws the error if something went wrong
     */
    @PostMapping("/glucoselog/list")
    PatientGlucoseLogDTO getGlucoseList(@RequestHeader("Authorization") String authToken,
                                        @RequestHeader("client") String client,
                                        @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * <p>
     * Saves a Patient's mental health related questions and its related answers.
     * </p>
     *
     * @param token   {@link String} - The "Authorization" header is typically used to send a token or credentials to
     *                authenticate the user making the request is given.
     * @param client  {@link String} - client to authenticate.
     * @param request {@link AssessmentDTO} - Request object with mental health details.
     * @return A {@link ResponseEntity} of String type with mental health creation message.
     */
    @PostMapping("mentalhealth/create")
    public ResponseEntity<String> createMentalHealth(@RequestHeader("Authorization") String token,
            @RequestHeader("client") String client, @RequestBody AssessmentDTO request);

    /**
     * <p>
     * This method is used to create the patient pregnancy details in fhir server
     * <p/>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} pregnancy details is given to save.
     * @return {@link PregnancyDetailsDTO} Patient pregnancy details
     * @throws FeignException Throws the error if something went wrong
     */
    @PostMapping("/patient/pregnancy/create")
    PregnancyDetailsDTO createPregnancyDetails(@RequestHeader("Authorization") String authToken,
                                               @RequestHeader("client") String client,
                                               @RequestBody PregnancyDetailsDTO pregnancyDetailsDTO);

	/**
	 * Creats NCD medical review
	 *
	 * @param authToken
	 * @param client
	 * @param request
	 * @return
	 * @throws FeignException
	 */
	@PostMapping("/medical-review/ncd/create")
	public Map<String, String> createNcdMedicalReview(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client,
															@RequestBody NCDMedicalReviewDTO request) throws FeignException;


    /**
     * <p>
     * Creates a summary for the given NCD medical review.
     *</p>
     *
     * @param authToken The authorization token provided in the request header.
     * @param client The client identifier provided in the request header.
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     * @throws FeignException in case of errors during communication with the backend service.
     */
    @PostMapping("/medical-review/ncd/summary-create")
    void createSummary(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client,
                                               @RequestBody NCDMedicalReviewDTO request) throws FeignException;

	/**
	 * Gets NCD medical review details.
	 *
	 * @param authToken
	 * @param client
	 * @param request
	 * @return
	 * @throws FeignException
	 */
	@PostMapping("/medical-review/ncd/details")
	public NcdMedicalReviewResponse getNcdMedicalReviewDetails(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client,
															@RequestBody MedicalReviewRequestDTO request) throws FeignException;

    /**
     * Gets Pregnancy details of a patient.
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patientTrackId
     * @return PregnancyDetailsDTO object.
     */
    @PostMapping("/patient/pregnancy/details")
    PregnancyDetailsDTO getPregnancyDetails(@RequestHeader("Authorization") String authToken,
                                            @RequestHeader("client") String client,
                                            @RequestBody RequestDTO requestData);

    /**
     * <p>
     *     This method used to create the lab test result.
     * <p/>
     *
     * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param labTestRequestDTO - Result create DTO with the lab test information.
     */
    @PostMapping("/investigation/result/create")
    public void updateInvestigationResult(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                                                    @RequestBody LabTestRequestDTO labTestRequestDTO);

    /**
     * <p>
     *     Review the lab test result entered by the lab technician.
     * <p/>
     *
     * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestDTO - Review comments and lab test id.
     */
    @PostMapping("/investigation/review")
    public void reviewInvestigation(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                                                    @RequestBody RequestDTO requestDTO);

    /**
     * <p>
     * This function updates the confirmed diagnosis in a medical review with the provided data.
     * </p>
     *
     * @param token               {@link String} This authorization token from the request header.
     * @param client              {@link String} It is from the request header.
     * @param confirmDiagnosisDTO {@link ConfirmDiagnosisDTO} It represents the diagnosis values of the patient.
     */
    @PostMapping("/medical-review/confirm-diagnosis/update")
    void updateConfirmDiagnosis(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
                                @RequestBody ConfirmDiagnosisDTO confirmDiagnosisDTO);

    /**
     *     Get patient lab test intensification details..
     * <p/>
     *
     * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestDTO - Review comments and lab test id.
     */
    @PostMapping("/investigation/prediction")
    public Map<String, List<LabTestDTO>> getIntensificationDetails(@RequestHeader("Authorization") String token,
                                                                  @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO);

    /**
     * Saves clinical notes of a patient for psychology.
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param request Request data with patientTrackId, patientVisitId and clinical notes
     * @return PregnancyDetailsDTO object.
     */
    @PostMapping("/medical-review/patient-psychology/create")
    PsychologyDTO savePatientPsychology(@RequestHeader("Authorization") String authToken,
                                        @RequestHeader("client") String client,
                                        @RequestBody PsychologyDTO request);

    /**
     * Gets Psychology note details of a patient.
     * @param request Request data with patientTrackId
     * @return PregnancyDetailsDTO object.
     */
    @PostMapping("/medical-review/patient-psychology/list")
    List<PsychologyDTO> getPatientPsychology(@RequestHeader("Authorization") String authToken,
                                        @RequestHeader("client") String client,
                                        @RequestBody PsychologyDTO request);

    /**
     * Delete patient based on patient fhir id..
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patientTrackId
     * @return PregnancyDetailsDTO object.
     */
    @PostMapping("/patient/delete")
    void deletePatientByPatientId(@RequestHeader("Authorization") String authToken,
                                    @RequestHeader("client") String client,
                                    @RequestBody RequestDTO requestData);

    /**
     * Gets Prescription dispense list of a patient.
     *
     * @param requestData Request data with patient id
     * @return list of PrescriptionDTO object.
     */
    @PostMapping("/prescription-request/dispense-prescription/list")
    List<PrescriptionDTO> getPrecriptionDispenseList(@RequestHeader("Authorization") String authToken,
                                            @RequestHeader("client") String client,
                                            @RequestBody RequestDTO requestData);

    /**
     * Updates Prescription dispense details of a patient.
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patient id
     */
    @PostMapping("/prescription-request/dispense-prescription/update")
    Map<String, String> updatePrescriptionDispense(@RequestHeader("Authorization") String authToken,
                                                     @RequestHeader("client") String client,
                                                     @RequestBody PrescriptionRequestDTO requestData);

    /**
     * Gets Prescription history details of a patient.
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patient id
     * @return list of PrescriptionDTO object.
     */
    @PostMapping("/prescription-request/dispense-prescription/history")
    List<PrescriptionDTO> getPrescriptionDispenseHistory(@RequestHeader("Authorization") String authToken,
                                                     @RequestHeader("client") String client,
                                                     @RequestBody RequestDTO requestData);

    /**
     * Gets Prescription prediction details of a patient.
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patient id
     * @return list of PrescriptionPredictionDTO object.
     */
    @PostMapping("/prescription-request/prediction")
    PrescriptionPredictionDTO getPrescriptionPrediction(@RequestHeader("Authorization") String authToken,
                                                        @RequestHeader("client") String client,
                                                        @RequestBody RequestDTO requestData);

    /**
     * <p>
     * Endpoint to create a new Patient Nutrition Lifestyle entry.
     * </p>
     *
     * @param patientNutritionLifestyle the Patient Nutrition Lifestyle object to be created
     * @return the created Patient Nutrition Lifestyle object
     */
    @PostMapping("/patient-nutrition-lifestyle/create")
    PatientNutritionLifestyle addPatientNutritionLifestyle(@RequestHeader("Authorization") String token,
                                                                  @RequestHeader("client") String client,
                                                                  @RequestBody PatientNutritionLifestyle patientNutritionLifestyle);

    /**
     * <p>
     * Endpoint to retrieve a list of Patient Nutrition Lifestyle entries.
     *</p>
     *
     * @param request the RequestDTO containing the request parameters
     * @return a list of Patient Nutrition Lifestyle entries
     */
    @PostMapping("/patient-nutrition-lifestyle/list")
    List<PatientNutritionLifestyle> getPatientNutritionLifeStyleList(@RequestHeader("Authorization") String token,
                                                                            @RequestHeader("client") String client,
                                                                            @RequestBody RequestDTO request);

    /**
     * Endpoint to update an existing Patient Nutrition Lifestyle entry.
     *
     * @param patientNutritionLifestyles the Patient Nutrition Lifestyle list to be updated
     * @return the updated Patient Nutrition Lifestyle object
     */
    @PutMapping("/patient-nutrition-lifestyle/update")
    PatientNutritionLifestyleUpdateDTO updatePatientNutritionLifestyle(@RequestHeader("Authorization") String token,
                                                                       @RequestHeader("client") String client,
                                                                       @RequestBody PatientNutritionLifestyleUpdateDTO patientNutritionLifestyles);

    /**
     * Endpoint to delete an existing Patient Nutrition Lifestyle entry.
     *
     * @param patientNutritionLifestyle the Patient Nutrition Lifestyle object to be deleted
     * @return the removed PatientNutritionLifestyle object
     */
    @PostMapping("/patient-nutrition-lifestyle/remove")
    PatientNutritionLifestyle deletePatientNutritionLifestyle(@RequestHeader("Authorization") String token,
                                                @RequestHeader("client") String client,
                                                @RequestBody PatientNutritionLifestyle patientNutritionLifestyle);

    /**
     * <p>
     * Gets medical review count.
     * </p>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param request   - request dto with Patient ID.
     * @return Map with Prescription and Medical Review count.
     */
    @PostMapping("/medical-review/count")
    Map<String, Integer> getMedicalReviewCount(@RequestHeader(Constants.AUTHORIZATION_HEADER) String authToken,
            @RequestHeader(Constants.CLIENT) String client, @RequestBody RequestDTO request);

    /**
     * This function retrieves NCD medical review history based on the provided request data and
     * authorization token.
     *
     * @param token      {@link String} This authorization token from the request header.
     * @param client     {@link String} It is from the request header.
     * @param requestDTO {@link MedicalReviewRequestDTO} It contains the  necessary information for the medical review summary history request.
     * @return A {@link NCDMedicalReviewHistoryDTO} object is being returned
     */
    @PostMapping("/medical-review/ncd/history-list")
    NCDMedicalReviewHistoryDTO getNCDMedicalReviewHistory(@RequestHeader("Authorization") String token,
                                                        @RequestHeader("client") String client, @RequestBody MedicalReviewRequestDTO requestDTO);

    /**
     * This function retrieves the summary history of NCD medical reviews based on the provided request
     * data and authorization token.
     *
     * @param token      {@link String} This authorization token from the request header.
     * @param client     {@link String} It is from the request header.
     * @param requestDTO {@link MedicalReviewRequestDTO} It contains the  necessary information for the medical review summary history request.
     * @return A {@link NCDMedicalReviewHistoryDTO} object is being returned
     */
    @PostMapping("/medical-review/ncd/history-summary")
    NCDMedicalReviewHistoryDTO getNCDMedicalReviewSummaryHistory(@RequestHeader("Authorization") String token,
                                                                 @RequestHeader("client") String client, @RequestBody MedicalReviewRequestDTO requestDTO);

    /**
     * <p>
     * Validate patient based on patient reference id.
     * </p>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patient reference
     * @return Map<String, String>
     */
    @PostMapping("/patient-transfer/validate")
    Map<String, String> validatePatientTransfer(@RequestHeader("Authorization") String authToken,
                                                @RequestHeader("client") String client,
                                                @RequestBody RequestDTO requestData);

    /**
     * <p>
     * Get patient data based on patient reference id.
     * </p>
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestData Request data with patientTrackId
     * @return PatientDTO Response data with patient of given patient reference id
     */
    @PostMapping("/patient/get-by-id")
    PatientDTO getPatientById(@RequestHeader("Authorization") String authToken,
                              @RequestHeader("client") String client,
                              @RequestBody RequestDTO requestData);

    /**
     * <p>
     * Update patient organization for given patient reference.
     * </p>
     *
     * @param authToken  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param requestDTO Request data with patient reference and organization data
     */
    @PostMapping("/patient-transfer/update/patient-records")
    void updatePatientRecords(@RequestHeader("Authorization") String authToken,
                              @RequestHeader("client") String client,
                              @RequestBody RequestDTO requestDTO);


    /**
     * <p>
     * Updates the PatientNutritionLifestyle view status.
     * </p>
     *
     * @param authToken - {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client    - client to authenticate.
     * @param request the {@link RequestDTO} request object contains Patient reference and Menu name.
     * @return {@link Boolean} view count update status.
     */
    @PutMapping("/medical-review/update-view-status")
    boolean updateViewCount(@RequestHeader(Constants.AUTHORIZATION_HEADER) String authToken,
            @RequestHeader(Constants.CLIENT) String client, @RequestBody RequestDTO request);

    /*
     * removes Psychology note details of a patient.
     * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param request Request data with patientTrackId, patientVisitId and clinical notes
     */
    @PostMapping("/medical-review/patient-psychology/remove")
    PsychologyDTO removePsychologyData(@RequestHeader("Authorization") String token,
                              @RequestHeader("client") String client, @RequestBody PsychologyDTO request);

    /**
     * updates Psychology note details of a patient.
     * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param request Request data with patientTrackId, patientVisitId and clinical notes
     */
    @PutMapping("/medical-review/patient-psychology/update")
    PsychologyDTO updatePsychology(@RequestHeader("Authorization") String token,
                                   @RequestHeader("client") String client, PsychologyDTO request);

    /**
     * This method is used to fetch patients count of a user in a given date from fhir server
     * </p>
     * @param request {@link DashboardDetailsRequestDTO} patient count details request dto is given
     * @return {@link DashboardDetails} Patients count details for the user
     * @throws FeignException Throws the error if something went wrong
     */
    @PostMapping("screening/dashboard-count")
    DashboardDetails getPatientCount(@RequestHeader("Authorization") String authToken,
                                     @RequestHeader("client") String client,
                                     @RequestBody DashboardDetailsRequestDTO request) throws FeignException;

    /**
     * Gets patient lifestyle details.
     *
     * @param token
     * @param client
     * @param request
     * @return List<LifestyleResponseDTO>
     */
    @PostMapping("/medical-review/patient-lifestyle-details")
    List<LifestyleResponseDTO> getPatientLifestyleDetails(@RequestHeader("Authorization") String token,
                                   @RequestHeader("client") String client, @RequestBody RequestDTO request);

    /**
     * This function update the pregnancy risk details based on the provided request data and authorization token.
     *
     * @param token      {@link String} This authorization token from the request header.
     * @param client     {@link String} It is from the request header.
     * @param pregnancyDetailsDTO {@link PregnancyDetailsDTO} It contains the  necessary information for the
     *                                                       pregnancy red risk details.
     * @return A {@link RequestDTO} object is being returned
     */
    @PutMapping("/patient/pregnancy-anc-risk/update")
    Boolean updatePregnancyANCRisk(@RequestHeader("Authorization") String token,
                                   @RequestHeader("client") String client,
                                   @RequestBody PregnancyDetailsDTO pregnancyDetailsDTO);

    /**
     * <p>
     * This function retrieves whether the signature is updated or not.
     * </p>
     *
     * @param token      {@link String} This authorization token from the request header.
     * @param client     {@link String} It is from the request header.
     * @param requestDTO {@link RequestDTO} It contains the  patient details and provenance details.
     * @return A boolean value is being returned
     */
    @PostMapping("/patient/update-signature")
    boolean updateMemberSignature(@RequestHeader("Authorization") String token, @RequestHeader("client") String client, @RequestBody RequestDTO requestDTO) throws FeignException;

    /**
     * <p>
     * This function is to update referred site in patient.
     * </p>
     * @param token      {@link String} This authorization token from the request header.
     * @param client     {@link String} It is from the request header.
     * @param requestDTO {@link ScreeningLogRequestDTO} It contains the  necessary information to
     *                                                   update referred site.
     */
    @PostMapping("/patient/referred-site-update")
    void updateReferredSite(@RequestHeader("Authorization") String token,
                                   @RequestHeader("client") String client,
                                   @RequestBody ScreeningLogRequestDTO requestDTO);
    
    /**
     * Creates a summary for the given NCD medical review.
     *</p>
     *
     * @param authToken The authorization token provided in the request header.
     * @param client The client identifier provided in the request header.
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     * @throws FeignException in case of errors during communication with the backend service.
     */
    @PostMapping("/medical-review/ncd/date/update")
    void updateNCDAppointment(@RequestHeader("Authorization") String authToken, @RequestHeader("client") String client,
                       @RequestBody NCDMedicalReviewDTO request) throws FeignException;

    /**
     * To Get list of patients based on search criteria
     *
     * @param token             auth token
     * @param client            auth client
     * @param patientRequestDTO Request DTO
     * @return PatientDTO Object
     * @throws FeignException
     */
    @PostMapping("/patient/ncd-search/list")
    Map<String, PatientDetailsDTO> getPatientNcdListBySearchText(@RequestHeader("Authorization") String token,
                                                                 @RequestHeader("client") String client,
                                                                 @RequestBody PatientRequestDTO patientRequestDTO) throws FeignException;

    /**
     * To Get list of patients based on search criteria
     *
     * @param token             auth token
     * @param client            auth client
     * @param request Request DTO
     * @return PatientDetailsDTO Object
     * @throws FeignException
     */
    @PostMapping("/patient/offline/list")
    List<PatientDetailsDTO> listPatientDetails(@RequestHeader("Authorization") String token,
                                               @RequestHeader("client") String client,
                                               @RequestBody RequestDTO request) throws FeignException;

    /**
    * <p>
    * This method is used to get a latest BpLog Assessment.
    * </p>
    *
    * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
    *                  authenticate the user making the request is given.
    * @param client - client to authenticate.
    * @param assessmentDTO {@link AssessmentDTO} entity is given
    * @return {@link BpLogDTO} Latest bp log details
    */
    @PostMapping("/assessment/bp-log/latest")
    BpLogDTO getExistingBpLog(@RequestHeader("Authorization") String token,
                            @RequestHeader("client") String client, AssessmentDTO assessmentDTO);

    /**
     * <p>
     * This method is used to get a latest Glucose Log Assessment.
     * </p>
     *
     * @param token  {@link String} The "Authorization" header is typically used to send a token or credentials to
     *                  authenticate the user making the request is given.
     * @param client - client to authenticate.
     * @param assessmentDTO {@link AssessmentDTO} entity is given
     * @return {@link GlucoseLogDTO} Latest glucose log details
     */
    @PostMapping("/assessment/glucose-log/latest")
    GlucoseLogDTO getExistingGlucoseLog(@RequestHeader("Authorization") String token,
                                        @RequestHeader("client") String client, AssessmentDTO assessmentDTO);

    /**
     * <p>
     * Saves a Patient's mental health related questions and its related answers.
     * </p>
     *
     * @param token   {@link String} - The "Authorization" header is typically used to send a token or credentials to
     *                authenticate the user making the request is given.
     * @param client  {@link String} - client to authenticate.
     * @param request {@link AssessmentDTO} - Request object with mental health details.
     * @return A {@link MentalHealthDTO} The constructed response is returned
     */
    @PostMapping("mentalhealth/details")
    MentalHealthDTO getMentalHealthDetails(@RequestHeader("Authorization") String token,
                                           @RequestHeader("client") String client, RequestDTO request);

    /**
     * <p>
     * Saves a Patient's mental health related questions and its related answers.
     * </p>
     *
     * @param token   {@link String} - The "Authorization" header is typically used to send a token or credentials to
     *                authenticate the user making the request is given.
     * @param client  {@link String} - client to authenticate.
     * @param request {@link AssessmentDTO} - Request object with mental health details.
     * @return A {@link ResponseEntity} of String type with mental health creation message.
     */
    @PostMapping("mentalhealth/condition-create")
    ResponseEntity<String> createMentalHealthCondition(@RequestHeader("Authorization") String token,
                                                     @RequestHeader("client") String client, @RequestBody AssessmentDTO request);

    /**
     * <p>
     * Saves a Patient's mental health related questions and its related answers.
     * </p>
     *
     * @param token   {@link String} - The "Authorization" header is typically used to send a token or credentials to
     *                authenticate the user making the request is given.
     * @param client  {@link String} - client to authenticate.
     * @param request {@link AssessmentDTO} - Request object with mental health details.
     * @return A {@link MentalHealthDTO} The constructed response is returned
     */
    @PostMapping("mentalhealth/condition-details")
    AssessmentDTO getMentalHealthCondition(@RequestHeader("Authorization") String token,
                                           @RequestHeader("client") String client, AssessmentDTO request);
}
