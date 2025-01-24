package com.mdtlabs.coreplatform.offlineservice.offlinesync.repository;

import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.offlineservice.common.model.OfflineSync;

import java.util.List;

/**
 * <p>
 * This class is a repository class to perform operation on offline sync repositories
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 */
@Repository
public interface OfflineSyncRepository extends JpaRepository<OfflineSync, Long> {

    public static final String GET_OFFLINE_SYNC_BY_IDS = "select offlineSync from OfflineSync offlineSync "
            + "where offlineSync.id in (:ids) and offlineSync.isDeleted = false";

    public static final String GET_OFFLINE_SYNC = "select offlineSync from OfflineSync offlineSync"
            + " WHERE (COALESCE(:requestId) is null OR (offlineSync.requestId = :requestId))"
            + " AND (COALESCE(:userId) is null OR (offlineSync.createdBy = :userId))"
            + " AND (COALESCE(:statuses) is null OR (offlineSync.status in (:statuses)))"
            + " AND (COALESCE(:types) is null OR (offlineSync.type in (:types)))"
            + " AND offlineSync.isDeleted = false";

    /**
     * Get offline sync requests based on id.
     *
     * @param id - ID of the offline sync request.
     * @return - Offline sync requests
     */
    public OfflineSync findByIdAndIsDeletedFalse(Long id);

    /**
     * Get offline sync requests based on ids.
     *
     * @param ids - Offline Sync IDS.
     * @return - Offline sync requests
     */
    @Query(value = GET_OFFLINE_SYNC_BY_IDS)
    public List<OfflineSync> findByIds(@Param(Constants.IDS) List<Long> ids);

    /**
     * Get offline sync requests based on id.
     *
     * @param id - ID of the offline sync request.
     * @return - Offline sync requests
     */
    public OfflineSync findByRequestIdAndReferenceIdAndType(String id, String referenceId, String type);

    /**
     * Get offline sync requests based on request.
     *
     * @param requestId - Request id.
     * @param userId - User id.
     * @param types - Types.
     * @param statuses - Statuses.
     * @return - List of Offline sync requests
     */
    @Query(value = GET_OFFLINE_SYNC)
    public List<OfflineSync> getOfflineSync(@Param(Constants.REQUEST_ID) String requestId,
                                             @Param(Constants.USER_ID) Long userId,
                                             @Param(Constants.TYPES) List<String> types,
                                             @Param(Constants.STATUSES) List<String> statuses);
}
