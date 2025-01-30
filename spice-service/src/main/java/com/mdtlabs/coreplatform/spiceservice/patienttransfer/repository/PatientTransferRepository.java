package com.mdtlabs.coreplatform.spiceservice.patienttransfer.repository;

import java.util.List;

import feign.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.PatientTransfer;
import com.mdtlabs.coreplatform.commonservice.common.model.enumeration.PatientTransferStatus;

/**
 * <p>
 * PatientTransferRepository is a Java interface defining a repository for the PatientTransfer entity.
 * It extends two other repository interfaces, JpaRepository
 * which provide basic CRUD operations and pagination/sorting functionality respectively.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created at Oct 09 2024
 */
@Repository
public interface PatientTransferRepository extends JpaRepository<PatientTransfer, Long> {

    PatientTransfer findByIdAndIsDeleted(long id, boolean isDeleted);

    String GET_PATIENT_TRANSFER = "select patientTransfer from PatientTransfer as patientTransfer ";

    String GET_PATIENT_TRANSFER_BY_PATIENT_TRACKER_AND_STATUS = GET_PATIENT_TRANSFER
            + "where patientTransfer.isDeleted = false and patientTransfer.transferStatus = :transferStatus and patientTransfer.patientFhirId = :patientFhirId";

    String GET_PATIENT_TRANSFER_COUNT = "select count(patientTransfer.id) from PatientTransfer as patientTransfer "
            + "where patientTransfer.isDeleted = false and patientTransfer.isShow = true and ((patientTransfer.oldSite.id = :healthFacilityId and patientTransfer.transferBy.id = :userId ) or (patientTransfer.transferTo.id = :userId and patientTransfer.transferStatus = 'PENDING' and patientTransfer.transferSite.id = :healthFacilityId))";

    String GET_INCOMING_LIST = GET_PATIENT_TRANSFER
            + "where patientTransfer.isDeleted = false and patientTransfer.transferSite.id = :healthFacilityId and patientTransfer.transferTo.id = :transferTo and patientTransfer.transferStatus = :transferStatus order by patientTransfer.updatedAt desc";

    String GET_OUTGOING_LIST = GET_PATIENT_TRANSFER
            + "where patientTransfer.isDeleted = false and patientTransfer.oldSite.id = :healthFacilityId and patientTransfer.transferBy.id = :transferBy and patientTransfer.isShow = true order by patientTransfer.updatedAt desc";

    /**
     * <p>
     * Get PatientTransfer by patient.
     * </p>
     *
     * @param patientFhirId - Patient reference
     * @return PatientTransfer entity
     */
    @Query(value = GET_PATIENT_TRANSFER_BY_PATIENT_TRACKER_AND_STATUS)
    PatientTransfer findByPatientTrackIdAndTransferStatus(@Param("patientFhirId") String patientFhirId,
                                                          @Param("transferStatus") PatientTransferStatus transferStatus);

    /**
     * <p>
     * Get transferred patient count for notification
     * </p>
     *
     * @param healthFacilityId - health facility id to get the notification count
     * @param userId           - user id to get the notification count for specific user
     * @return Long The response containing the count of the notification
     * regarding the transferred patients
     */
    @Query(value = GET_PATIENT_TRANSFER_COUNT)
    Long getPatientTransferCount(@Param("healthFacilityId") Long healthFacilityId,
                                 @Param("userId") Long userId);

    /**
     * <p>
     * Retrieves a list of incoming patient transfers to a specific health facility.
     * </p>
     *
     * @param healthFacilityId The ID of the health facility where patients are being transferred.
     * @param transferTo       The ID of the destination within the health facility for the transfer.
     * @param transferStatus   The status of the patient transfer, filtering the results based on transfer status.
     * @return {@link List<PatientTransfer>} A list of incoming patient transfers matching the specified criteria.
     */
    @Query(value = GET_INCOMING_LIST)
    List<PatientTransfer> getIncomingList(@Param("healthFacilityId") Long healthFacilityId,
                                          @Param("transferTo") Long transferTo, @Param("transferStatus") PatientTransferStatus transferStatus);

    /**
     * <p>
     * Retrieves a list of outgoing patient transfers from a specified health facility.
     * </p>
     *
     * @param healthFacilityId The ID of the health facility initiating the patient transfers.
     * @param transferBy       The ID of the entity or department responsible for initiating the transfers.
     * @return {@link List<PatientTransfer>} A list of outgoing patient transfers matching the specified criteria.
     */

    @Query(value = GET_OUTGOING_LIST)
    List<PatientTransfer> getOutgoingList(@Param("healthFacilityId") Long healthFacilityId,
                                          @Param("transferBy") Long transferBy);

    /**
     * <p>
     * Fetch the  call registers of given patient id
     * </p>
     *
     * @param patientId The id of the patient
     * @return Paitent transfer of given patient id is returned.
     */
    List<PatientTransfer> findByPatientFhirId(String patientId);
}
