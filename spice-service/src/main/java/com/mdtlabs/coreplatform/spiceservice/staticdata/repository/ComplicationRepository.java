package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Complication;

/**
 * This interface is responsible for performing database operations between
 * server and Complication entity.
 *
 * @author Niraimathi S created on 30 Jun, 2022
 *
 */
@Repository
public interface ComplicationRepository extends JpaRepository<Complication, Long> {
	
	/**
	 * Gets Complication entity list by isDeleted and isActive fields.
	 *
	 * @return List of Complication entities.
	 */
	List<Complication> findByIsDeletedFalseAndIsActiveTrue();

}
