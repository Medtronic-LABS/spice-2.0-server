package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.CurrentMedication;


/**
 * This interface is responsible for performing database operations between
 * server and CurrentMedication entity.
 *
 * @author Niraimathi S created on Jun 30, 2022
 *
 */
@Repository
public interface CurrentMedicationRepository extends JpaRepository<CurrentMedication, Long> {
    
	/**
	 * Gets CurrentMedication entity list based on isActive And isDeleted fields.
	 *
	 * @return List of CurrentMedication entities
	 */
	List<CurrentMedication> findByIsDeletedFalseAndIsActiveTrue();
	
	/**
	 * <p>
     * Gets CurrentMedication entity list based on isActive, isDeleted, and id's.
     * </p>
     *
	 * @param ids
	 * @return List of CurrentMedication entities
	 */
	List<CurrentMedication> findByIdInAndIsDeletedFalseAndIsActiveTrue(Set<Long> ids);

}
