package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Unit;

/**
 * This interface is responsible for performing database operations between
 * server and Unit entity.
 *
 * @author Niraimathi S created on Jun 30, 2022
 *
 */
@Repository
public interface UnitRepository extends JpaRepository<Unit, Long> {

	/**
	 * Gets list of Units using Name.
	 *
	 * @param   name  unit name
	 * @return  List      List of Unit entities
	 */
	List<Unit> findByNameNotLike(String name);
	
	/**
	 * Gets list of Units using Type.
	 *
	 * @param   name  unit name
	 * @return  List      List of Unit entities
	 */
	List<Unit> findByType(String type);

}
