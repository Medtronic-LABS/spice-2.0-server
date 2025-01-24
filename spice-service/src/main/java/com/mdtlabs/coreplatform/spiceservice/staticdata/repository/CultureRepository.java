package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the user module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author VigneshKumar created on Jun 20, 2022
 */
@Repository
public interface CultureRepository extends JpaRepository<Culture, Long> {
    
    /**
     * <p>
     * To get the active cultures.
     * </p>
     * 
     * @return List of culture entities.
     */
    public List<Culture> findByIsDeletedFalseAndIsActiveTrue();
    
    /**
     * <p>
     * To get culture by name.
     * <p>
     *
     * @param name
     * @return Culture entity.
     */
    public Culture findByNameIgnoreCase(String name);

}
