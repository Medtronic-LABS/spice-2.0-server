package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.DosageFrequency;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Dosage frequency based action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Yogeshwaran Mohan Created on May 8, 2024.
 */
@Repository
public interface DosageFrequencyRepository extends JpaRepository<DosageFrequency, Long> {

    /**
	 * Gets Complication entity list by isDeleted and isActive fields.
	 *
	 * @return List of Complication entities.
	 */
	List<DosageFrequency> findByIsDeletedFalseAndIsActiveTrue();


    /**
     * Finds all dosage frequency by isActive and isDeleted.
     *
     * @return List of dosage frequency
     */
    List<DosageFrequency> findAllByIsActiveTrueAndIsDeletedFalse();

}
