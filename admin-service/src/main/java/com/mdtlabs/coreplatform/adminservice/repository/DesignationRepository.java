package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the user module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Divya created on Oct 18, 2024
 */
@Repository
public interface DesignationRepository extends JpaRepository<Designation, Long> {

    String GET_ALL_DESIGNATIONS = "select designation from Designation as designation join designation.role as role where role.level >=:level and designation.countryId =:countryId";

    /**
     * <p>
     * To Fetch all Designation for given countryId and IsActive True.
     * </p>
     *
     * @param countryId ID of the country to which the designation list to be retrieved is given
     * @return {@link List} the list of designation is retrieved
     */
    @Query(value = GET_ALL_DESIGNATIONS)
    List<Designation> getAllDesignations(@Param(Constants.COUNTRY_ID) Long countryId, @Param(FieldConstants.LEVEL) Long level);
}
