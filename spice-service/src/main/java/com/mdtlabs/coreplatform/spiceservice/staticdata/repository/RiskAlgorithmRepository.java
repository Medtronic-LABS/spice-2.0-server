package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.RiskAlgorithm;

/**
 * This interface is responsible for performing database operations between
 * server and RiksAlgorithm entity.
 *
 * @author Niraimathi S
 *
 */
@Repository
public interface RiskAlgorithmRepository extends JpaRepository<RiskAlgorithm, Long> {

	/**
	 * Gets lsit of RiksAlgorithm by country id.
	 *
	 * @return List of RiskAlgorithm entities
	 */
	List<RiskAlgorithm> findByIsDeletedFalseAndIsActiveTrue();
}
