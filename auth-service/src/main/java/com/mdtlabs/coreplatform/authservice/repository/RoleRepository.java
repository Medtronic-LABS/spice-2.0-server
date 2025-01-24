package com.mdtlabs.coreplatform.authservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the role module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Prabu
 * @since Sep 16, 2022
 */
@Repository
public interface RoleRepository extends JpaRepository<Role, Long> {

    String GET_ALL_ROLES = "select role from Role as role where role.isActive=true and role.isDeleted=false";

    /**
     * Retrieves all active roles from the database.
     * This method uses a JPA query to fetch all Role entities where the isActive field is true.
     * The result is returned as a list of Role entities.
     *
     * @return A list of active Role entities.
     */
    @Query(value = GET_ALL_ROLES)
    List<Role> getActiveRoles();

} 