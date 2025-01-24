package com.mdtlabs.coreplatform.fhirmapper.patienttransfer.service;

import java.util.Map;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
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
     * Validates patient transfer.
     * </p>
     *
     * @param requestDTO .
     * @return Success response
     */
    Map<String, String> validatePatientTransfer(RequestDTO requestDTO);

    /**
     * <p>
     * This method is used to update patient organization for given patient reference.
     * </p>
     *
     * @param requestDTO {@link RequestDTO} The requestDTO contains necessary information
     *                         with patient reference and organization data
     */
    void updatePatientRecords(RequestDTO requestDTO);
}
