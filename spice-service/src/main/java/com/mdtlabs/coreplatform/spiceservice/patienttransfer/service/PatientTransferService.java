package com.mdtlabs.coreplatform.spiceservice.patienttransfer.service;


import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientTransferUpdateRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;

/**
 * <p>
 * This class is a service class to perform operation on PatientTransfer
 * operations.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Oct 07, 2024
 */
public interface PatientTransferService {
    /**
     * <p>
     * Creates patient transfer
     * </p>
     *
     * @param patientTransferDto
     * @return Success response
     */
    void createPatientTransfer(PatientTransferRequestDTO patientTransferDto);

    /**
     * <p>
     * Validates patient transfer.
     * </p>
     *
     * @param requestDTO
     * @return Success response
     */
    Map<String, String> validatePatientTransfer(RequestDTO requestDTO);

    /**
     * <p>
     * This method is used to retrieve a list of districts based on a search request.
     * </p>
     *
     * @param patientTransferDto {@link PatientTransferUpdateRequestDTO} The RequestDTO  contains necessary information
     *                         to update the patient transfer
     * @return {@link PatientTransfer} The response PatientTransfer containing updated
     * patient tranfer object
     */
    void updatePatientTransfer(PatientTransferUpdateRequestDTO patientTransferDto);

    /**
     * <p>
     * Get transferred patient count for notification
     * </p>
     *
     * @param requestDTO {@link RequestDTO} The RequestDTO  contains necessary information
     *                         to get the count of the transferred patients
     * @return {@link Map<String, Long>} The response PatientTransfer containing updated
     * patient tranfer object
     */
    Map<String, Long> getPatientTransferCount(RequestDTO requestDTO);

    /**
     * <p>
     * Get transferred patients list
     * </p>
     *
     * @param requestDTO {@link RequestDTO} The RequestDTO  contains necessary information
     *                         to get the count of the transferred patients
     * @return {@link Map<String, Object>} The response containing the list
     * of transferred patients
     */
    Map<String, Object> getPatientTransferList(RequestDTO requestDTO);
}
