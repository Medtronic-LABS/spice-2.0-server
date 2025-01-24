package com.mdtlabs.coreplatform.commonservice.common.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.ApiRolePermission;

/**
 * <p>
 * This interface defines the repository for ApiRolePermission entities.
 * It extends JpaRepository to leverage Spring Data JPA's methods for database operations.
 * It includes custom methods to fetch ApiRolePermission entities based on service name and type.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
@Repository
public interface ApiRolePermissionRepository extends JpaRepository<ApiRolePermission, Long> {

    /**
     * Fetches a list of ApiRolePermission entities based on the provided service name.
     *
     * @param serviceName The name of the service for which ApiRolePermission entities are to be fetched.
     * @return List<ApiRolePermission> - A list of ApiRolePermission entities matching the provided service name.
     */
    List<ApiRolePermission> findByServiceNameAndIsActiveTrueAndIsDeletedFalse(String serviceName);

    /**
     * Fetches a list of ApiRolePermission entities based on the provided service name and type.
     *
     * @param serviceName The name of the service for which ApiRolePermission entities are to be fetched.
     * @param type The type of the ApiRolePermission entities to be fetched.
     * @return List<ApiRolePermission> - A list of ApiRolePermission entities matching the provided service name and type.
     */
    List<ApiRolePermission> findByServiceNameAndTypeAndIsActiveTrueAndIsDeletedFalse(String serviceName, String type);
}
