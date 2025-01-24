package com.mdtlabs.coreplatform.userservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserSupervisor;


/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the UserSupervisor module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick Murugesan
 * @since Feb 01, 2024
 */
@Repository
public interface UserSupervisorRepository extends JpaRepository<UserSupervisor, Long> {

    /**
     * <p>
     * Retrieves a list of UserSupervisor entities where the user ID is within the specified list,
     * and both 'isDeleted' and 'isActive' flags are false and true respectively. This method is useful
     * for fetching active, non-deleted supervisors based on a list of user IDs.
     * </p>
     *
     * @param ids A list of user IDs to filter the UserSupervisors by.
     * @return List<UserSupervisor> A list of UserSupervisor entities matching the criteria.
     */
    List<UserSupervisor> findByUserIdInAndIsDeletedFalseAndIsActiveTrue(List<Long> ids);

    /**
     * <p>
     * Retrieves a single UserSupervisor entity based on the provided user ID, where 'isDeleted' is false
     * and 'isActive' is true. This method is ideal for fetching an active, non-deleted supervisor for a specific user.
     * </p>
     *
     * @param id The user ID to filter the UserSupervisor by.
     * @return UserSupervisor The UserSupervisor entity matching the criteria.
     */
    UserSupervisor findByUserIdAndIsDeletedFalseAndIsActiveTrue(Long id);

    /**
     * <p>
     * Retrieves a list of UserSupervisor entities based on the supervisor ID, while also considering
     * the 'isDeleted' and 'isActive' flags. This method allows for filtering supervisors by their ID
     * and status, which is useful for managing supervisor records in various states.
     * </p>
     *
     * @param supervisorId The supervisor ID to filter the UserSupervisors by.
     * @param isDeleted    The deletion status to filter the UserSupervisors by.
     * @param isActive     The active status to filter the UserSupervisors by.
     * @return List<UserSupervisor> A list of UserSupervisor entities matching the criteria.
     */
    List<UserSupervisor> findBySupervisorIdAndIsDeletedAndIsActive(Long supervisorId, boolean isDeleted, boolean isActive);

    /**
     * <p>
     * Retrieves a list of UserSupervisor entities based on the provided user ID, considering both deletion and activation status.
     * This method is useful for filtering UserSupervisor entities to find those that are associated with a specific user,
     * while also taking into account whether the entities are marked as deleted or active.
     * </p>
     *
     * @param userId    The user ID to filter the UserSupervisors by.
     * @param isDeleted The deletion status to filter the UserSupervisors by. If true, only deleted UserSupervisors are retrieved.
     * @param isActive  The active status to filter the UserSupervisors by. If true, only active UserSupervisors are retrieved.
     * @return List<UserSupervisor> A list of UserSupervisor entities matching the criteria.
     */
    List<UserSupervisor> findByUserIdAndIsDeletedAndIsActive(Long userId, boolean isDeleted, boolean isActive);

    /**
     * <p>
     * Retrieves a list of UserSupervisor entities based on the supervisor IDs provided, considering both deletion and activation status.
     * </p>
     *
     * @param ids       A list of supervisor IDs to filter the UserSupervisors by.
     * @param isDeleted The deletion status to filter the UserSupervisors by. If true, only UserSupervisors marked as deleted are retrieved.
     * @param isActive  The active status to filter the UserSupervisors by. If true, only UserSupervisors marked as active are retrieved.
     * @return List<UserSupervisor> A list of UserSupervisor entities matching the criteria.
     */
    List<UserSupervisor> findBySupervisorIdInAndIsDeletedAndIsActive(List<Long> ids, boolean isDeleted, boolean isActive);
}
