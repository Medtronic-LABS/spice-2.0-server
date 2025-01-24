package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Lifestyle;

/**
 * This interface is responsible for performing database operations between
 * server and Lifestyle entity.
 *
 * @author Niraimathi S created on Jun 30, 2022
 *
 */
@Repository
public interface LifestyleRepository extends JpaRepository<Lifestyle, Long> {
	
	/**
	 * Gets list of Lifestyle entities based on isActive and isDeleted fields.
	 *
	 * @return Lifestyle entities List
	 */
	List<Lifestyle> findByIsDeletedFalseAndIsActiveTrue();

}
