package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.ObstetricExamination;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the obstetric examination module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Nanthinee Sugumar Created on Mar 20, 2024.
 */
public interface ObstetricExaminationRepository extends JpaRepository<ObstetricExamination, Long> {

    /**
     * API to get all active records.
     *
     * @return List
     */
    List<ObstetricExamination> findAllByIsActiveTrueAndIsDeletedFalse();
}
