package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Reason;

/**
 * This interface is responsible for performing database operations between
 * server and reason entity.
 *
 * @author Niraimathi S
 *
 */
@Repository
public interface ReasonRepository extends JpaRepository<Reason, Long> {

    /**
     * <p>
	 * Gets list of Reason entities based on isDefault and isDeleted.
     * </p>
	 *
	 * @return List of Reason entities
	 */
    public List<Reason> findByIsDeletedFalseAndIsActiveTrue();

}
