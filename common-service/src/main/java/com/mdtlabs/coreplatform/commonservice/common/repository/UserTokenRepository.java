package com.mdtlabs.coreplatform.commonservice.common.repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserToken;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the user module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
@Repository
public interface UserTokenRepository extends JpaRepository<UserToken, Long> {

    /**
     * <p>
     * Retrieves a UserToken entity by authentication token.
     * This method uses a JPA query to fetch a UserToken entity where the authentication token matches the provided parameter.
     * The result is returned as a UserToken entity.
     * </p>
     *
     * @param token The authentication token of the user.
     * @return A UserToken entity that matches the provided authentication token.
     */
    UserToken findByAuthToken(String token);

    /**
     * <p>
     * Retrieves a list of UserToken entities by user ID where the isActive field is true.
     * This method uses a JPA query to fetch UserToken entities where the user ID matches the provided parameter and the isActive field is true.
     * The result is returned as a list of UserToken entities.
     * </p>
     *
     * @param userId The ID of the user.
     * @return A list of UserToken entities that match the provided user ID and where isActive is true.
     */
    List<UserToken> findByUserIdAndIsActiveTrue(Long userId);

    /**
     * <p>
     * Retrieves a list of UserToken entities by user IDs where the isActive field is true.
     * This method uses a JPA query to fetch UserToken entities where the user ID is in the provided set of IDs and the isActive field is true.
     * The result is returned as a list of UserToken entities.
     * </p>
     *
     * @param userIds The set of user IDs.
     * @return A list of UserToken entities that match the provided user IDs and where isActive is true.
     */
    List<UserToken> findByUserIdInAndIsActiveTrue(Set<Long> userIds);

}
