package com.mdtlabs.coreplatform.spiceservice.followup.service;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDto;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpCriteria;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FollowUpDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import org.springframework.web.bind.annotation.PostMapping;

import java.util.List;

/**
 * Interface for follow-up service operations.
 * <p>
 * Defines the contract for services handling follow-up operations for household members. It includes creating and updating follow-up records,
 * retrieving a list of follow-ups based on criteria, and fetching follow-up criteria based on a request.
 * </p>
 *
 * @author Maria Antony
 * Created on April 29, 2024.
 */
public interface FollowUpService {

    /**
     * Add the call register
     *
     * @param callRegister call register
     * @param updateOldCallRegister updateOldCallRegister as a boolean value
     * @return FollowUpDTO object
     */
    FollowUpDTO addCallRegister(CallRegister callRegister, boolean updateOldCallRegister);

    /**
     * Creates a follow-up record for a household member.
     * <p>
     * This method is responsible for creating a new follow-up record based on the provided {@link FollowUpDTO} details.
     * It returns the created follow-up record.
     * </p>
     *
     * @param followUp The follow-up details as a {@link FollowUpDTO} object.
     * @return The created {@link FollowUpDTO} object.
     */
    @PostMapping("/create")
    FollowUpDTO createFollowUp(FollowUpDTO followUp);

    /**
     * Updates an existing follow-up record for a household member.
     * <p>
     * This method updates an existing follow-up record with the details provided in the {@link FollowUpDTO} object.
     * It returns the updated follow-up record.
     * </p>
     *
     * @param followUp The follow-up details to be updated as a {@link FollowUpDTO} object.
     * @return The updated {@link FollowUpDTO} object.
     */
    @PostMapping("/update")
    FollowUpDTO updateFollowUp(FollowUpDTO followUp);

    /**
     * Updates an existing follow-up record for a household member.
     * <p>
     * This method updates an existing follow-up record with the details provided in the {@link FollowUpDTO} object.
     * It returns the updated follow-up record.
     * </p>
     *
     * @param followUp The follow-up details to be updated as a {@link FollowUpDTO} object.
     * @return The updated {@link FollowUpDTO} object.
     */
    FollowUpDTO updateNcdFollowUp(FollowUpDTO followUp);

    /**
     * Retrieves a list of follow-up records based on the provided criteria.
     * <p>
     * This method fetches a list of follow-up records that match the criteria specified in the {@link RequestDTO} object.
     * It returns a list of {@link FollowUpDTO} objects that match the criteria.
     * </p>
     *
     * @param request The criteria for retrieving follow-up records as a {@link RequestDTO} object.
     * @return A list of {@link FollowUpDTO} objects.
     */
    List<FollowUpDTO> getFollowUpList(RequestDTO request);

    /**
     * Retrieves the follow-up criteria based on the provided request.
     * <p>
     * This method fetches the follow-up criteria based on the details specified in the {@link RequestDTO} object.
     * It returns a {@link FollowUpCriteria} object containing the criteria for follow-up operations.
     * </p>
     *
     * @param request The request details as a {@link RequestDTO} object.
     * @return The {@link FollowUpCriteria} object.
     */
    FollowUpCriteria getFollowUpCriteria(RequestDTO request);

    /**
     * Add the call register
     *
     * @param followUpDTO follow up details
     * @param updateOldCallRegister updateOldCallRegister as a boolean value
     * @return FollowUpDTO object
     */
    FollowUpDTO createNcdFollowUp(FollowUpDTO followUpDTO, boolean updateOldCallRegister);

    /**
     * This method is used to get patient list for followup.
     *
     * @param patientRequestDTO - patient request dto
     * @return List - list of FollowUpListDTO
     */
    ResponseListDTO<FollowUpDTO> getFollowUpPatients(PatientRequestDTO patientRequestDTO);

    /**
     * This method is used to get patient list for followup.
     *
     * @param patientRequestDTO - patient request dto
     * @return List - list of FollowUpListDTO
     */
    List<FollowUpDTO> getAllCallRegistersByVillages(PatientRequestDTO patientRequestDTO, String type);

    /**
     * This method is used to get patient list for followup.
     *
     * @param patientRequestDTO - patient request dto
     * @return List - list of FollowUpListDTO
     */
    ResponseListDTO<FollowUpDTO> getScreeningFollowUpPatients(PatientRequestDTO patientRequestDTO);

    /**
     * This method is used to get patient list for followup.
     *
     * @param patientRequestDTO - patient request dto
     * @return List - list of FollowUpListDTO
     */
    ResponseListDTO<FollowUpDTO> getAssessmentFollowUpPatients(PatientRequestDTO patientRequestDTO);

    /**
     * <p>
     * This method is used for getting the Lost to follow up Patients.
     * </p>
     *
     * @param patientRequestDTO - Request object with filter options
     * @return List of Lost follow up patients.
     */
    ResponseListDTO<FollowUpDTO> getLostToFollowUpPatients(PatientRequestDTO patientRequestDTO);

    /**
     * <p>
     * This method is used for getting the medical review follow up Patients.
     * </p>
     *
     * @param patientRequestDTO - Request object with filter options
     * @return List of medical review follow up patients.
     */
    ResponseListDTO<FollowUpDTO> getMedicalReviewFollowUpPatients(PatientRequestDTO patientRequestDTO);

    /**
     * <p>
     * This function delete the call register for given type
     * </p>
     *
     * @param followUpDTO {@link FollowUpDTO} request that contains the call register details of the patient
     */
    void deleteNcdCallRegister(FollowUpDTO followUpDTO);

    /**
     * <p> This method is responsible for retrieving pending call register based on logged in user </p>
     *
     * @return call register details.
     */
    CallRegisterDto getPendingCallRegister();

    /**
     * <p>
     * This function updates the call register for given type
     * </p>
     *
     * @param patientReference patient id for get the call register
     * @param siteReference site id for change the call register site
     */
    void transferCallRegisters(String patientReference, String siteReference);
}
