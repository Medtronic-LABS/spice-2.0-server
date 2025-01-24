package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;

import com.mdtlabs.coreplatform.authservice.ErrorConstants;
import com.mdtlabs.coreplatform.authservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.authservice.repository.UserRepository;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

/**
 * <p>
 * The class "AuthenticationProvider" implements the Spring Security interface for authentication
 * providers.
 * </p>
 *
 * @author shajini varghese Created on Feb 01, 2023
 */
public class AuthenticationProvider implements org.springframework.security.authentication.AuthenticationProvider {

    private final UserRepository userRepository;
    private final RoleRepository roleRepository;

    @Value("${app.login-time-limit-in-minutes}")
    private int loginTimeLimitInMinutes;

    @Value("${app.login-count-limit}")
    private int loginCountLimit;

    public AuthenticationProvider(UserRepository userRepository, RoleRepository roleRepository) {
        this.userRepository = userRepository;
        this.roleRepository = roleRepository;
    }


    /**
     * <p>
     * Authenticates a user.
     * This method takes an Authentication object, retrieves the username and password from it, and checks if the user is valid.
     * It also checks if the user has active roles and if the password matches the one stored in the database.
     * If the user is authenticated, a new UsernamePasswordAuthenticationToken is returned with the user, password, and authorities.
     * If the user is not authenticated, a BadCredentialsException is thrown.
     * </p>
     *
     * @param authentication The Authentication object containing the principal and credentials.
     * @return A fully authenticated object including credentials.
     * @throws AuthenticationException if authentication fails.
     */
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        String username = String.valueOf(authentication.getPrincipal()).toLowerCase();
        String password = String.valueOf(authentication.getCredentials());
        User user = authenticationCheck(username, password);
        Set<Role> roles = user.getRoles();
        Map<String, Role> rolesAsMap = getRoleAsMap(roles);
        boolean isActiveRole = Boolean.FALSE;
        List<Role> rolesList = roleRepository.getActiveRoles();
        for (Role role : rolesList) {
            if (null != rolesAsMap.get(role.getName())) {
                isActiveRole = Boolean.TRUE;
            }
        }
        if (!isActiveRole) {
            throw new BadCredentialsException(ErrorConstants.ERROR_INVALID_USER);
        }

        boolean isAuthenticated = Boolean.FALSE;
        if (password.equals(user.getPassword())) {
            isAuthenticated = Boolean.TRUE;
        }
        if (isAuthenticated) {
            Set<GrantedAuthority> authorityList = user.getAuthorities();
            user.setRoles(roles);
            return new UsernamePasswordAuthenticationToken(user, password, authorityList);
        }
        Logger.logError(StringUtil.constructString(ErrorConstants.INFO_USER_PASSWORD_NOT_MATCH, username));
        throw new BadCredentialsException(ErrorConstants.ERROR_INVALID_USER);
    }

    /**
     * <p>
     * Checks the authentication of a user.
     * This method takes a username and password, and performs several checks to validate the user.
     * It checks if the username and password are not blank, if the user exists, if the password matches the one stored in the database,
     * and if the user is within the login time limit.
     * If all checks pass, the user is considered authenticated and the user object is returned.
     * If any check fails, a BadCredentialsException is thrown.
     * </p>
     *
     * @param username The username of the user to be authenticated.
     * @param password The password of the user to be authenticated.
     * @return The User object if the user is authenticated.
     * @throws BadCredentialsException if any authentication check fails.
     */
    private User authenticationCheck(String username, String password) {
        if ((isBlank(username)) || (isBlank(password))) {
            throw new BadCredentialsException(ErrorConstants.ERROR_USERNAME_PASSWORD_BLANK);
        }
        User user = null;
        if (username.matches(Constants.NUMBER_REGEX)) {
            user = userRepository.getUserByPhonenumber(username);
        } else {
            user = userRepository.getUserByUsername(username);
        }
        if (null == user) {
            Logger.logError(StringUtil.constructString(ErrorConstants.INFO_USER_NOT_EXIST, username));
            throw new BadCredentialsException(ErrorConstants.ERROR_INVALID_USER);
        }
        if (null == user.getPassword()) {
            Logger.logError(StringUtil.constructString(ErrorConstants.PASSWORD_NOT_EXIST, username));
            throw new BadCredentialsException(ErrorConstants.ERROR_INVALID_USER);
        }

        boolean isLoginWithinTimeLimit = isLoginWithinTimeLimit(user);
        if (isLoginWithinTimeLimit) {
            Logger.logError(StringUtil.constructString(ErrorConstants.ERROR_ACCOUNT_DISABLED));
            throw new BadCredentialsException(ErrorConstants.ERROR_ACCOUNT_DISABLED);
        } else {
            if (user.getPassword().equals(password)) {
                setUserValues(user, Constants.ZERO, Boolean.FALSE, null, null);
                userRepository.save(user);
            } else if (Boolean.TRUE.equals(user.getIsBlocked())) {
                setUserValues(user, Constants.ONE, Boolean.FALSE, null, DateUtil.formatDate(new Date()));
                userRepository.save(user);
            } else if (updateAccBlockStatus(user)) {
                Logger.logError(StringUtil.constructString(ErrorConstants.ERROR_INVALID_ATTEMPTS));
                throw new BadCredentialsException(ErrorConstants.ERROR_INVALID_ATTEMPTS);
            }
        }
        Logger.logInfo(StringUtil.constructString(Constants.INFO_USER_EXIST, String.valueOf(user.isEnabled())));
        return user;
    }

    /**
     * <p>
     * Checks if a user's login is within the time limit.
     * This method takes a User object and checks if the user is blocked.
     * If the user is blocked, it calculates the difference in minutes between the current time and the time of the user's invalid login.
     * If the difference is less than or equal to the login time limit, it sets the isLoginWithinTimeLimit flag to true.
     * </p>
     *
     * @param user The User object to check the login time limit for.
     * @return A boolean indicating whether the user's login is within the time limit.
     */
    public boolean isLoginWithinTimeLimit(User user) {
        boolean isLoginWithinTimeLimit = false;
        if (Boolean.TRUE.equals(user.getIsBlocked())) {
            int invalidLoginAttempts = user.getInvalidLoginAttempts();
            Logger.logInfo("invalidLoginAttempts== isLoginLimitExceed " + invalidLoginAttempts);
            Date invalidLoginTime = DateUtil.formatDate(user.getInvalidLoginTime());
            Date currentDate = DateUtil.formatDate(new Date());
            long differenceInMinutes = DateUtil.getDateDiffInMinutes(invalidLoginTime, currentDate);
            if (differenceInMinutes <= loginTimeLimitInMinutes) {
                isLoginWithinTimeLimit = true;
            }
        }
        return isLoginWithinTimeLimit;
    }

    /**
     * <p>
     * Updates the account block status of a user.
     * This method takes a User object and checks the number of invalid login attempts made by the user.
     * If the number of invalid login attempts is greater than or equal to the login count limit, a BadCredentialsException is thrown.
     * If the number of invalid login attempts is greater than or equal to zero, the number of invalid login attempts is incremented.
     * If the incremented number of invalid login attempts equals the login count limit, the user is blocked and the block date is set to the current date.
     * If the incremented number of invalid login attempts does not equal the login count limit, the user is not blocked and the invalid login time is set to the current date.
     * The updated user is then saved in the UserRepository.
     * The method returns a boolean indicating whether the user is blocked.
     * </p>
     *
     * @param user The User object to update the account block status for.
     * @return A boolean indicating whether the user is blocked.
     */
    private boolean updateAccBlockStatus(User user) {
        int invalidLoginAttempts = user.getInvalidLoginAttempts();
        boolean isBlocked = Boolean.FALSE;
        if (invalidLoginAttempts >= loginCountLimit) {
            Logger.logError(StringUtil.constructString(ErrorConstants.ERROR_INVALID_ATTEMPTS));
            throw new BadCredentialsException(ErrorConstants.ERROR_INVALID_ATTEMPTS);
        }
        if (invalidLoginAttempts >= Constants.ZERO) {
            invalidLoginAttempts++;
            if (invalidLoginAttempts == loginCountLimit) {
                setUserValues(user, invalidLoginAttempts, Boolean.TRUE, DateUtil.formatDate(new Date()), DateUtil.formatDate(new Date()));
                isBlocked = Boolean.TRUE;
            } else {
                setUserValues(user, invalidLoginAttempts, Boolean.FALSE, null, DateUtil.formatDate(new Date()));
            }
            userRepository.save(user);
        }
        return isBlocked;

    }

    /**
     * <p>
     * This method is used to convert a set of roles into a map with role names as keys and roles as values.
     * </p>
     *
     * @param roles The "roles" parameter is a Set of objects of type "Role" is given
     */
    private Map<String, Role> getRoleAsMap(Set<Role> roles) {
        Map<String, Role> roleAsMap = new HashMap<>();
        roles.forEach(role -> roleAsMap.put(role.getName(), role));
        return roleAsMap;
    }

    /**
     * <p>
     * This method is used to check if the provided authentication class is assignable from the
     * UsernamePasswordAuthenticationToken class and returns a boolean value accordingly.
     * </p>
     *
     * @param authentication The "authentication" parameter is a Class object that represents the type
     *                       of authentication object that is being checked for support is given
     * @return {@link  boolean} A boolean value indicating whether the given authentication class is supported or
     * not is given
     */
    public boolean supports(Class<?> authentication) {
        return UsernamePasswordAuthenticationToken.class.isAssignableFrom(authentication);
    }

    /**
     * <p>
     * Checks if a CharSequence is blank.
     * This method takes a CharSequence and checks if it is null or has a length of zero, in which case it returns true.
     * If the CharSequence is not null and has a length greater than zero, it checks each character to see if it is a whitespace character.
     * If a non-whitespace character is found, it returns false. If no non-whitespace characters are found, it returns true.
     * </p>
     *
     * @param charSequence The CharSequence to check for blankness.
     * @return A boolean indicating whether the CharSequence is blank.
     */
    private boolean isBlank(CharSequence charSequence) {
        int stringLength;
        if ((charSequence == null) || ((stringLength = charSequence.length()) == Constants.ZERO)) {
            return Boolean.TRUE;
        }
        for (int character = Constants.ZERO; character < stringLength; character++) {
            if (!Character.isWhitespace(charSequence.charAt(character))) {
                return Boolean.FALSE;
            }
        }
        return Boolean.TRUE;
    }

    /**
     * <p>
     * Checks if a user's login attempts have exceeded the limit.
     * This method takes a User object and checks if the user is null.
     * If the user is null, it returns false.
     * If the user is not null, it retrieves the number of invalid login attempts and the time of the last invalid login.
     * It then calculates the difference in hours between the current time and the time of the last invalid login.
     * If the difference in hours is greater than or equal to the login time limit in minutes, it sets the invalid login time to the current date and resets the number of invalid login attempts to one.
     * If the difference in hours is less than the login time limit in minutes, it checks the number of invalid login attempts.
     * If the number of invalid login attempts is greater than or equal to the login count limit, it sets the blocked date to the current date, resets the number of invalid login attempts to zero, and blocks the user.
     * If the number of invalid login attempts is less than the login count limit, it increments the number of invalid login attempts.
     * The updated user is then saved in the UserRepository.
     * The method returns a boolean indicating whether the user's login attempts have exceeded the limit.
     * </p>
     *
     * @param user The User object to check the login limit for.
     * @return A boolean indicating whether the user's login attempts have exceeded the limit.
     */
    public boolean isLoginLimitExceed(User user) {
        if (null == user) {
            return Boolean.FALSE;
        }
        int invalidLoginAttempts = user.getInvalidLoginAttempts();
        Date invalidLoginTime = DateUtil.formatDate(user.getInvalidLoginTime());
        Date currentDate = DateUtil.formatDate(new Date());
        long differenceInHours = DateUtil.getDiffInHours(currentDate, invalidLoginTime);
        if (differenceInHours >= loginTimeLimitInMinutes) {
            user.setInvalidLoginTime(currentDate);
            setUserValues(user, Constants.ONE, Boolean.FALSE, Boolean.TRUE);
        } else {
            if (invalidLoginAttempts >= loginCountLimit) {
                user.setBlockedDate(currentDate);
                setUserValues(user, Constants.ZERO, Boolean.TRUE, Boolean.FALSE);
                userRepository.save(user);
                return Boolean.TRUE;
            } else if (invalidLoginAttempts >= Constants.ZERO) {
                user.setInvalidLoginAttempts(++invalidLoginAttempts);
            }
        }
        userRepository.save(user);
        return Boolean.FALSE;
    }

    /**
     * <p>
     * Sets the user values related to login attempts and blocking.
     * This method takes a User object, the number of invalid login attempts, a boolean indicating whether the user is blocked,
     * the date when the user was blocked, and the date of the last invalid login.
     * It sets these values in the User object and logs the blocked date.
     * </p>
     *
     * @param user                 The User object to set the values for.
     * @param invalidLoginAttempts The number of invalid login attempts.
     * @param isBlocked            A boolean indicating whether the user is blocked.
     * @param blockedDate          The date when the user was blocked.
     * @param invalidLoginTime     The date of the last invalid login.
     */
    private void setUserValues(User user, int invalidLoginAttempts, boolean isBlocked, Date blockedDate, Date invalidLoginTime) {
        user.setInvalidLoginAttempts(invalidLoginAttempts);
        user.setIsBlocked(isBlocked);
        user.setBlockedDate(blockedDate);
        user.setInvalidLoginTime(invalidLoginTime);
        Logger.logInfo("blocked date " + user.getBlockedDate());
    }

    /**
     * <p>
     * Sets the user values related to login attempts, blocking status, and active status.
     * This method takes a User object, the number of invalid login attempts, a boolean indicating whether the user is blocked,
     * and a boolean indicating whether the user is active.
     * It sets these values in the User object.
     * </p>
     *
     * @param user                 The User object to set the values for.
     * @param invalidLoginAttempts The number of invalid login attempts.
     * @param isBlocked            A boolean indicating whether the user is blocked.
     * @param isActive             A boolean indicating whether the user is active.
     */
    private void setUserValues(User user, int invalidLoginAttempts, boolean isBlocked, boolean isActive) {
        user.setInvalidLoginAttempts(invalidLoginAttempts);
        user.setIsBlocked(isBlocked);
        user.setActive(isActive);
    }
}
