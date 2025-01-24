package com.mdtlabs.coreplatform.spiceservice.householdmemberlink.respository;

import com.mdtlabs.coreplatform.spiceservice.common.model.HouseholdMemberLink;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the disease category module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Denisha J Created on Oct 17, 2024.
 */
@Repository
public interface HouseholdMemberLinkRepository extends JpaRepository<HouseholdMemberLink, Long> {

    String GET_UNASSIGNED_MEMBERLIST_BY_VILLAGE_IDS = "FROM HouseholdMemberLink as householdMemberLink where" +
            " (COALESCE(:villageIds) is null OR (householdMemberLink.villageId in (:villageIds)))" +
            " and (cast(:lastSyncTime as Date) is null OR (householdMemberLink.updatedAt > :lastSyncTime))" +
            " and (cast(:currentSyncTime as Date) is null OR (householdMemberLink.updatedAt <= :currentSyncTime))" +
            " and (cast(:lastSyncTime as Date) is not null OR householdMemberLink.status = :Unassigned)" +
            " and householdMemberLink.isActive=true" +
            " and householdMemberLink.isDeleted=false";

    /**
     * API to get all Member List on villageid.
     *
     * @return List</HouseholdMemberLink>
     */
    @Query(value = GET_UNASSIGNED_MEMBERLIST_BY_VILLAGE_IDS)
    List<HouseholdMemberLink> findByVillageIds(@Param(Constants.VILLAGE_IDS) List<String> villageIds,
                                               @Param(Constants.UNASSIGNED) String unassigned,
                                               @Param(Constants.LAST_SYNC_TIME) Date lastSyncTime,
                                               @Param(Constants.CURRENT_SYNC_TIME) Date currentSyncTime);

    /**
     * API to get all Member List on memberId.
     *
     * @return List</ HouseholdMemberLink>
     */
    List<HouseholdMemberLink> findByMemberIdInAndIsDeletedFalseAndIsActiveTrue(List<String> memberIds);
}
