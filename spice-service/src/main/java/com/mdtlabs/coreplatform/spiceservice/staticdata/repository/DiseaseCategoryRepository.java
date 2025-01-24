package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.DiseaseCategory;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the disease category module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Repository
public interface DiseaseCategoryRepository extends JpaRepository<DiseaseCategory, Long> {

    /**
     * API to get all active records.
     *
     * @return List
     */
    List<DiseaseCategory> findAllByIsActiveTrueAndIsDeletedFalse();

}
