package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Diagnosis;


/**
 * <p>
 * This interface is responsible for performing database operations between
 * server and Diagnosis entity.
 * </p>
 *
 * @author Niraimathi S
 *
 */
@Repository
public interface DiagnosisRepository extends JpaRepository<Diagnosis, Long> {

    /**
	 * <p>
	 * This method is used to get list of Diagnosis entites.
	 * </p>
	 * 
	 * @return List of Diagnosis
	 */
    public List<Diagnosis> findByIsDeletedFalseAndIsActiveTrue();

}
