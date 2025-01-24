package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.DosageForm;


/**
 * This interface is responsible for performing database operations between
 * server and DosageForm entity.
 *
 * @author Niraimathi S created on Jun 30, 2022
 *
 */
@Repository
public interface DosageFormRepository extends JpaRepository<DosageForm, Long> {

	/**
	 * Gets list of Dosage form entities except "Other" dosageform.
	 *
	 * @param name dosage form name
	 * @return List of DosageForm entities
	 */
	List<DosageForm> findByNameNotLike(String name);


	/**
	 * Gets list of Dosage form entities except "Other" dosageform.
	 *
	 * @return List of DosageForm entities
	 */
	List<DosageForm> findByIsDeletedFalseAndIsActiveTrue();

	/**
	 * To get Dosage form.
	 *
	 * @param name
	 * @return Dosage Form
	 */
	DosageForm findByNameAndIsDeletedFalse(String name);

}
