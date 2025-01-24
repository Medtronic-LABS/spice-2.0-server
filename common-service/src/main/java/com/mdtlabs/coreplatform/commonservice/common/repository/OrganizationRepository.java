package com.mdtlabs.coreplatform.commonservice.common.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;

/**
 * <p>
 * This Repository interface maintains connection between Organization entity and
 * database.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
@Repository(value = "organizationRepo")
public interface OrganizationRepository extends JpaRepository<Organization, Long> {

    /**
     * <p>
     * Gets the organization based on is active and is deleted.
     * </p>
     *
     * @return {@link List<Organization>}  list of entities
     */
    List<Organization> findByIsDeletedFalseAndIsActiveTrue();

    /**
     * Finds organization by form data id and form name.
     * 
     * @param id
     * @param string
     * @return Organization
     */
    Organization findByFormDataIdAndFormName(Long id, String string);
    
}
