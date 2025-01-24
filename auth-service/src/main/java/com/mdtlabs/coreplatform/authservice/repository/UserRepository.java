package com.mdtlabs.coreplatform.authservice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the user module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Prabu created on Sep 16, 2022
 */
@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    public static String GET_USER_BY_USERNAME = "FROM User as user where user.username=:username and user.isActive=true and user.isDeleted=false";

    public static final String GET_USER_BY_USERNAME_ISACTIVE = "select user from User as user "
            + "where user.username =:username and user.isActive =:status and user.isDeleted=false";

    public static String GET_USER_BY_PHONENUMBER = "FROM User as user where user.phoneNumber=:phoneNumber and user.isActive=true and user.isDeleted=false";

    /**
     * <p>
     * Retrieves a User entity by username.
     * This method uses a JPA query to fetch a User entity where the username matches the provided parameter.
     * The result is returned as a User entity.
     * </p>
     *
     * @param username The username of the user to be retrieved.
     * @return A User entity that matches the provided username.
     */
    @Query(value = GET_USER_BY_USERNAME)
    public User getUserByUsername(@Param("username") String username);

    /**
     * <p>
     * Retrieves a User entity by phone number.
     * This method uses a JPA query to fetch a User entity where the phone number matches the provided parameter.
     * The result is returned as a User entity.
     * </p>
     *
     * @param phoneNumber The phone number of the user to be retrieved.
     * @return A User entity that matches the provided phone number.
     */
    @Query(value = GET_USER_BY_PHONENUMBER)
    public User getUserByPhonenumber(@Param("phoneNumber") String phoneNumber);

    /**
     * <p>
     * This method is used to get user data by passing username.
     * </p>
     *
     * @param username username of the user is given
     * @param status   active status of the user is given
     * @return {@link User}   user information which is stored
     */
    @Query(value = GET_USER_BY_USERNAME_ISACTIVE)
    public User getUserByUsername(@Param(FieldConstants.USERNAME) String username,
                                  @Param(FieldConstants.STATUS) Boolean status);
} 