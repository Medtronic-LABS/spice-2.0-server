package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.DosageForm;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the DosageForm module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 */
@Repository
public interface DosageFormRepository extends JpaRepository<DosageForm, Long> {

    List<DosageForm> findAllByIsActiveTrueAndIsDeletedFalse();
}
