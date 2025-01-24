package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Comorbidity;

/**
 * This interface is responsible for performing database operations between
 * server and Comorbidity entity.
 *
 * @author Niraimathi S
 *
 */
@Repository
public interface ComorbidityRepository extends JpaRepository<Comorbidity, Long> {
	/**
	 * Gets list of comorbidities by isDeleted and isActive fields.
	 *
	 * @return List of Comorbidity entity.
	 */
	List<Comorbidity> findByIsDeletedFalseAndIsActiveTrue();
}
