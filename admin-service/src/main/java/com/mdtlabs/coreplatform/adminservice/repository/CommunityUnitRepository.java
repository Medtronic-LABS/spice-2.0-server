package com.mdtlabs.coreplatform.adminservice.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.CommunityUnit;

@Repository
public interface CommunityUnitRepository extends JpaRepository<CommunityUnit, Long> {

    String GET_COMMUNITY_UNITS = "select cu from CommunityUnit cu where (:countryId is null OR cu.countryId=:countryId) "
            + "and (:parentRegionId is null OR cu.parentRegionId=:parentRegionId) "
            + "AND (COALESCE(:searchTerm) is null or "
            + "lower(cu.name) LIKE CONCAT('%',lower(CAST(:searchTerm AS text)),'%')) "
            + "and cu.isActive = true and cu.isDeleted = false";

    @Query(value = GET_COMMUNITY_UNITS)
    Page<CommunityUnit> getCommunityUnits(@Param("countryId") Long countryId,
                                          @Param("parentRegionId") Long parentRegionId,
                                          @Param("searchTerm") String searchTerm,
                                          Pageable pageable);

}


