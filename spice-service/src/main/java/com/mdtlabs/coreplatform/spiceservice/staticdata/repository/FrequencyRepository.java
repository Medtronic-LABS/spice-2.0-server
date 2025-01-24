package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Frequency;


/**
 * This repository maintains connection between database and frequency entity.
 *
 * @since Jun 20, 2022
 * @author Niraimathi S
 */
@Repository
public interface FrequencyRepository extends JpaRepository<Frequency, Long> {
	
	/**
	 * Find list of Frequency by its deleted and active status.
	 *
	 * @return List of Frequency
	 */
	List<Frequency> findByIsDeletedFalseAndIsActiveTrue();

	/**
	 * Finds Frequency by risk level and is deleted.
	 * 
	 * @param cvdRiskLevel
	 * @param isDeleted
	 * @return List<Frequency>
	 */
    List<Frequency> findByRiskLevelAndIsDeleted(String cvdRiskLevel, boolean isDeleted);

	/**
	 * Finds Frequency by name, type and is deleted.
	 * 
	 * @param hba1cDefaultFrequency
	 * @param frequencyHba1cCheck
	 * @return
	 */
    Frequency findByNameAndTypeAndIsDeleted(String name, String type, boolean isDeleted);

}
