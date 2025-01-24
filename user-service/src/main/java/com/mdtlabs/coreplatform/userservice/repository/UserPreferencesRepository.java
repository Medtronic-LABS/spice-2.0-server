package com.mdtlabs.coreplatform.userservice.repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserPreferences;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * This is the repository class for user preferences operations.
 * In query annotation (nativeQuery = true), the query performs like SQL.
 * Otherwise it performs like HQL default value for (nativeQuery = false).
 * </p>
 *
 * @author Nandhakumar karthikeyan.
 * @since July 09, 2024
 */
@Repository
public interface UserPreferencesRepository extends JpaRepository<UserPreferences, Long> {

    String GET_USER_PREFERENCES_BY_ID = "From UserPreferences where type = :type and userId = :userId and isActive = true and isDeleted = false";

    /**
     * Gets preferences detail for the given user.
     *
     * @param userId - User id to get the detail.
     * @param type   - Type of user preference.
     * @return - User preference retrieved for the given user.
     */
    @Query(value = GET_USER_PREFERENCES_BY_ID)
    UserPreferences getPreferencesByType(@Param("userId") Long userId, @Param("type") String type);

}
