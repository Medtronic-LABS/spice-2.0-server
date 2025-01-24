package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Meta;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the role module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 */
public interface MetaRepository extends JpaRepository<Meta, Long> {

    /**
     * Finds meta by category.
     *
     * @param category type
     * @return List of Meta
     */
    List<Meta> findByCategoryAndIsDeletedFalseAndIsActiveTrue(String category);


    /**
     * Finds all meta by isActive and isDeleted.
     *
     * @return List of Meta
     */
    List<Meta> findAllByIsActiveTrueAndIsDeletedFalse();

}
