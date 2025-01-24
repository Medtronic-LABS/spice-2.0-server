package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.FrequencyType;

/**
 * This repository maintains connection between database and frequency type entity.
 *
 * @since May 05, 2023
 * @author Jeyaharini T A
 */
@Repository
public interface FrequencyTypeRepository extends JpaRepository<FrequencyType, Long> {
	
	/**
	 * Gets list of Lifestyle entities based on isActive and isDeleted fields.
	 *
	 * @return Lifestyle entities List
	 */
	List<FrequencyType> findByIsDeletedFalseAndIsActiveTrue();
}
