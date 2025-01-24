package com.mdtlabs.coreplatform.fhirmapper.common.utils.respository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;

/**
 * Repository interface for {@link MetaCodeDetails} entity.
 * <p>
 * Extends {@link JpaRepository} to provide standard methods for CRUD operations and
 * query execution for {@link MetaCodeDetails} entities. It interacts with the database
 * to perform operations related to MetaCodeDetails entities.
 * </p>
 *
 * @author Karthick M created On 05 Mar 2024
 */
@Repository
public interface MetaCodeDetailsRepository extends JpaRepository<MetaCodeDetails, Long> {

    List<MetaCodeDetails> findAllByIsActiveTrueAndIsDeletedFalse();
}
