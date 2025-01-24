package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Culture module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 */
@Repository
public interface CultureRespository extends JpaRepository<Culture, Long> {

    List<Culture> findAllByIsActiveTrueAndIsDeletedFalse();

}
