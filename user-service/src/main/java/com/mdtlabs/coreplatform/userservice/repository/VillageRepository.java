package com.mdtlabs.coreplatform.userservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Village module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Jan 11, 2024
 */
@Repository
public interface VillageRepository extends JpaRepository<Village, Long> {

    /**
     * <p>
     * Retrieves a list of villages based on the provided criteria.
     * This method filters villages by their IDs, deletion status, and activation status.
     * It is particularly useful for fetching villages that are active, not deleted, and match a set of specific IDs.
     * </p>
     *
     * @param linkedVillageIds A list of village IDs to filter the villages by.
     * @param isDeleted        A boolean flag indicating if the villages to be found should be marked as deleted.
     * @param isActive         A boolean flag indicating if the villages to be found should be marked as active.
     * @return List<Village> A list of villages that match the given criteria.
     */
    List<Village> findByIdInAndIsDeletedAndIsActive(List<Long> linkedVillageIds, boolean isDeleted, boolean isActive);

}
