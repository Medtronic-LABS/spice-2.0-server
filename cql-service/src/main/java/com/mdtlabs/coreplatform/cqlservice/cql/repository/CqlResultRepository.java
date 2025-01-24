package com.mdtlabs.coreplatform.cqlservice.cql.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.cqlservice.model.entity.CqlResult;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the CqlResult module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024.
 */
@Repository
public interface CqlResultRepository extends JpaRepository<CqlResult, Long> {

    /**
     * <p>
     * This method retrieves the CqlResult for the specified patient's resource id.
     * </p>
     *
     * @param resourceId the resource id of the patient.
     * @return the CqlResult for the specified patient.
     */
    CqlResult findByResourceIdAndIsLatestTrue(String resourceId);

    /**
     * <p>
     * This method retrieves the CqlResult for the specified patient id.
     * </p>
     *
     * @param patientId the id of the patient.
     * @return the CqlResult for the specified patient.
     */
    CqlResult findByPatientIdAndIsLatestTrue(String patientId);

    /**
     * This method retrieves the list of CqlResult for the specified village ids with last sync and current sync time
     *
     * @param villageIds      the list of village ids.
     * @param lastSyncTime    last sync time.
     * @param currentSyncTime current sync time.
     * @return the list of CqlResult for the specified conditions.
     */
    List<CqlResult> findByVillageIdInAndUpdatedAtGreaterThanAndUpdatedAtLessThanEqualAndIsLatestTrue(List<String> villageIds,
                                                                                                     Date lastSyncTime, Date currentSyncTime);

    /**
     * This method retrieves the list of CqlResult for the specified village ids with current sync time
     *
     * @param villageIds      the list of village ids.
     * @param currentSyncTime current sync time.
     * @return the list of CqlResult for the specified conditions.
     */
    List<CqlResult> findByVillageIdInAndUpdatedAtLessThanEqualAndIsLatestTrue(List<String> villageIds, Date currentSyncTime);

}
