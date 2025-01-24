package com.mdtlabs.coreplatform.adminservice.repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 *  Interface that communicates between database and server side that is Category entity.
 * </p>
 *
 *  @author Gokul created on Oct 17, 2024
 */
@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

    /**
     * Retrieves list of categories with isDeleted false and isActive true
     */
    List<Category> findByIsDeletedFalseAndIsActiveTrueOrderByDisplayOrder();
}