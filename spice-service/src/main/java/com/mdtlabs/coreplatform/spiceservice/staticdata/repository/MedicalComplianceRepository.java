package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.MedicalCompliance;



/**
 * This interface is responsible for performing database operations between
 * server and MedicalCompliance entity.
 *
 * @author Niraimathi S
 *
 */
@Repository
public interface MedicalComplianceRepository extends JpaRepository<MedicalCompliance, Long> {

	/**
	 * <p>
	 * Gets list of MedicalCompliance entities.
	 * </p>
	 *
	 * @return MedicalCompliance entities List
	 */
    public List<MedicalCompliance> findByIsDeletedFalseAndIsActiveTrue();

}
