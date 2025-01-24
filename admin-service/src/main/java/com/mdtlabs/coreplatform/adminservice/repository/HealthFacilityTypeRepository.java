package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.HealthFacilityTypes;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the HealthFacilityTypes module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 * @since Dec 26, 2023
 */
@Repository
public interface HealthFacilityTypeRepository extends JpaRepository<HealthFacilityTypes, Long> {

    List<HealthFacilityTypes> findAllByIsActiveTrueAndIsDeletedFalse();

}
