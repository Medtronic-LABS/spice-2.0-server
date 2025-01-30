package com.mdtlabs.coreplatform.spiceservice.medicalreview.service;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.dto.BirthHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ConfirmDiagnosisDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GeneralMedicalReviewSummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.IccmResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LifestyleResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MotherNeonateDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NCDMedicalReviewHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.NcdMedicalReviewResponse;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientStatusDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.UnderFiveIccmDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancyDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewPregnancySummaryDetailsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ObservationDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PncMedicalReviewDTO;

import org.springframework.web.bind.annotation.RequestBody;

/**
 * <p>
 * This class is a service class to perform operation on MedicalReviewService
 * operations.
 * </p>
 *
 * @author Nandhakumar created on Mar 28, 2024
 */
public interface MedicalReviewService {

    /**
     * Create General Medical Review
     *
     * @param generalMedicalReviewDTO Medical Review Details
     * @return GeneralMedicalReviewDTO
     */
    Map<String, String> createGeneralMedicalReview(GeneralMedicalReviewDTO generalMedicalReviewDTO);

    /**
     * Get Medical Review Summary
     *
     * @param requestDTO EncounterId
     * @return Summary Details
     */
    GeneralMedicalReviewSummaryDetailsDTO getGeneralMedicalReviewDetails(RequestDTO requestDTO);

    /**
     * Create General Medical Review summary
     *
     * @param summaryDTO Medical Review Details
     * @return GeneralMedicalReviewDTO
     */
    GeneralMedicalReviewSummaryDTO saveSummaryDetails(GeneralMedicalReviewSummaryDTO summaryDTO);

    /**
     * Creates iccm medical review for under two months
     *
     * @param iccmdto
     * @return Map<String, String> response
     */
    Map<String, String> createICCMUnder2months(UnderFiveIccmDTO iccmdto);

    /**
     * Gets a medical review details of under two months.
     *
     * @param requestDTO
     * @return IccmResponseDTO
     */
    IccmResponseDTO getIccmUnder2MSummary(MedicalReviewRequestDTO requestDTO);

    /**
     * Creates iccm medical review for two months to five years.
     *
     * @param iccmdto
     * @return Map<String, String> response
     */
    Map<String, String> createICCMUnder5Years(UnderFiveIccmDTO iccmdto);

    /**
     * Gets a medical review details of two months to five years.
     *
     * @param requestDTO
     * @return IccmResponseDTO
     */
    IccmResponseDTO getIccmUnder5YSummary(MedicalReviewRequestDTO requestDTO);

    /**
     * Creates a new value observation in the FHIR database.
     * This method constructs an observation resource with the provided observation DTO
     * and saves it to the FHIR server using a bundle transaction.
     *
     * @param observationDTO The observation DTO containing details of the observation to be created.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    ObservationDTO createBp(@RequestBody ObservationDTO observationDTO);

    /**
     * Sets medical observations based on medical review data.
     * This method constructs observation resources for various medical review data
     * and saves them to the FHIR server using a bundle transaction.
     *
     * @param medicalReviewDTO The medical review DTO containing the data for setting observations.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    Map<String, String> createMedicalReview(@RequestBody MedicalReviewPregnancyDTO medicalReviewDTO);

    /**
     * Retrieves pregnancy summary details for a medical review based on the given identifier.
     * <p>
     * This method fetches and returns pregnancy summary details related to a specific medical review.
     * It takes an identifier as input to locate the desired medical review.
     *
     * @param requestDTO The unique identifier of the medical review.
     * @return MedicalReviewPregnancySummaryDetailsDTO containing pregnancy summary details.
     */
    MedicalReviewPregnancySummaryDetailsDTO getPregnancyMedicalReviewDetails(RequestDTO requestDTO);

    /**
     * Save Pnc Medical Review Details
     *
     * @param pncMedicalReviewDTO Pnc Medical Review Details
     * @return Dto
     */
    Map<String, String> savePncMedicalReview(PncMedicalReviewDTO pncMedicalReviewDTO);

    /**
     * Get Medical Review Details
     *
     * @param requestDTO MotherId
     * @return PncMedicalReviewDTO
     */
    PncMedicalReviewDTO getPNCMedicalReviewDetails(RequestDTO requestDTO);

    /**
     * get Patient Latest Weight
     *
     * @param requestDTO Request Details
     * @return weight
     */
    Map<String, Double> getPatientWeight(RequestDTO requestDTO);

    /**
     * get Patient Latest Bp values
     *
     * @param requestDTO Request Details
     * @return Bp values
     */
    Map<String, Double> getPatientBp(RequestDTO requestDTO);

    /**
     * Creates a new value observation in the FHIR database.
     * This method constructs an observation resource with the provided observation DTO
     * and saves it to the FHIR server using a bundle transaction.
     *
     * @param observationDTO The observation DTO containing details of the observation to be created.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     */
    ObservationDTO createWeight(@RequestBody ObservationDTO observationDTO);

    /**
     * Retrieves the medical review history for a given patient or encounter.
     * Implementing classes should provide the logic to retrieve the medical review history based on the provided request.
     *
     * @param requestDTO The request data transfer object containing the patientId and/or encounterId.
     * @return A MedicalReviewHistoryDTO object containing the medical review history of the patient or encounter.
     */
    MedicalReviewHistoryDTO getHistory(MedicalReviewRequestDTO requestDTO);

    /**
     * Retrieves the PNC medical review history for a given patient or encounter.
     * Implementing classes should provide the logic to retrieve the PNC medical review history based on the provided request.
     *
     * @param requestDTO The request data transfer object containing the patientId and/or encounterId.
     * @return A MedicalReviewHistoryDTO object containing the PNC medical review history of the patient or encounter.
     */
    MedicalReviewHistoryDTO getPncHistory(MedicalReviewRequestDTO requestDTO);

    /**
     * Get Birth-History Details
     *
     * @param requestDTO RequestDTO
     * @return BirthHistoryDTO Details
     */
    BirthHistoryDTO getBirthHistory(RequestDTO requestDTO);

    /**
     * Creates a new record for labor, mother, and neonate based on the provided data.
     *
     * @param requestDTO The data containing information about labor, mother, and neonate.
     * @return A MotherNeonateDTO representing the created record.
     */
    Map<String, String> createLabourMotherAndNeonate(@RequestBody MotherNeonateDTO requestDTO);

    /**
     * Retrieves details of labor, mother, and neonate based on the provided request data.
     *
     * @param requestDTO The data containing necessary information to retrieve the details.
     * @return A MotherNeonateDTO containing the details of labor, mother, and neonate.
     */
    MotherNeonateDTO getLabourMotherAndNeonateDetails(RequestDTO requestDTO);

    /**
     * Save Pnc Child Details.
     *
     * @param householdMemberDTO Pnc Medical Review child Details.
     */
    void createPncChild(HouseholdMemberDTO householdMemberDTO);

    /**
     * <p>
     * Used to create or update the patient status
     * <p/>
     *
     * @param patientStatusDto The {@link PatientStatusDTO} - Contains the patient status details.
     */
    PatientStatusDTO createPatientStatus(PatientStatusDTO patientStatusDto);

    /**
     * <p>
     * Used to get the patient status details based on the patient id.
     * <p/>
     *
     * @param patientStatusDto - Patient id to get the patient status.
     * @return {@link PatientStatusDTO} - Patient status details
     */
    PatientStatusDTO getPatientStatusDetails(PatientStatusDTO patientStatusDto);

    /**
     * Creates Ncd medical review.
     * 
     * @param request
     * @return Map<String, String>
     */
    Map<String, String> createNcdMedicalReview(NCDMedicalReviewDTO request);


    /**
     * <p>
     * Creates a summary for the given NCD medical review.
     *</p>
     *
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     */
    void createSummary(NCDMedicalReviewDTO request);

    /**
     * Get medical review details.
     * 
     * @param request
     * @return
     */
    NcdMedicalReviewResponse ncdMedicalReviewSummary(MedicalReviewRequestDTO request);

    /**
     * <p>
     * This function creates and updates confirm diagnosis information in a FHIR server.
     * </p>
     *
     * @param confirmDiagnosisDTO {@link ConfirmDiagnosisDTO} It contains information related to confirming a diagnosis for a patient.
     */
    void updateConfirmDiagnosis(ConfirmDiagnosisDTO confirmDiagnosisDTO);


    /**
     * Gets prescription and lab test count.
     *
     * @param request - request DTO
     * @return Map - count map
     */
    Map<String, Integer> getMedicalReviewCount(RequestDTO request);

    /**
     * The function retrieves NCD medical review history based on the provided
     * `MedicalReviewRequestDTO`.
     *
     * @param medicalReviewRequestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     *                                history, and any other relevant data needed to retrieve the NCD history
     * @return {@link NCDMedicalReviewHistoryDTO} The history based on given request is retrieved.
     */
    NCDMedicalReviewHistoryDTO getNCDMedicalReviewHistory(MedicalReviewRequestDTO medicalReviewRequestDTO);

    /**
     * The function  retrieves the NCD medical review summary history
     * based on the provided `MedicalReviewRequestDTO`.
     *
     * @param medicalReviewRequestDTO {@link MedicalReviewRequestDTO} It contains details such as patient information, medical
     *                                history, and any other relevant data needed to retrieve the NCD history
     * @return {@link NCDMedicalReviewHistoryDTO} The history based on given request is retrieved.
     */
    NCDMedicalReviewHistoryDTO getNCDMedicalReviewSummaryHistory(MedicalReviewRequestDTO medicalReviewRequestDTO);

    /**
     * <p>
     * Updates the PatientNutritionLifestyle view status.
     * </p>
     *
     * @param request the {@link RequestDTO} request object contains Patient reference and Menu name.
     * @return A {@link Boolean} based on the result of view count update.
     */
    boolean updateViewCount(RequestDTO request);

    /**
     * Gets patient lifetyle details.
     * 
     * @param request
     * @return List<LifestyleResponseDTO>
     */
    List<LifestyleResponseDTO> getPatientLifestyleDetails(RequestDTO request);

    /**
     * Used to get instruction
     * @return List<String>
     */
    List<String> getInstruction();

    /**
     * <p>
     * Creates or updates an appointment of the given NCD medical review.
     *</p>
     *
     * @param request The NCDMedicalReviewDTO object containing the details of the medical review to be summarized.
     */
    void updateNCDAppointment(NCDMedicalReviewDTO request);
}
