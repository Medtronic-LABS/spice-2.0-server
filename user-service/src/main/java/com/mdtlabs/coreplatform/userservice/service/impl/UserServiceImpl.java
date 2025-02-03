package com.mdtlabs.coreplatform.userservice.service.impl;

import java.security.Key;
import java.security.SecureRandom;
import java.util.*;
import java.util.stream.Collectors;

import javax.crypto.SecretKey;
import javax.xml.bind.DatatypeConverter;

import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.JwtBuilder;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.Jwts.SIG;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.security.MacAlgorithm;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.OrganizationUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserTenantsContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.ForbiddenException;
import com.mdtlabs.coreplatform.commonservice.common.exception.InvalidPathException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.EmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDetailsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OutBoundEmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserOrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserPreferencesDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserSuperAdminDto;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserVillageResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplate;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserPreferences;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserSupervisor;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserToken;
import com.mdtlabs.coreplatform.commonservice.common.repository.UserTokenRepository;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.userservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.userservice.apiinterface.NotificationInterface;
import com.mdtlabs.coreplatform.userservice.apiinterface.ShortUrlInterface;
import com.mdtlabs.coreplatform.userservice.mapper.UserMapper;
import com.mdtlabs.coreplatform.userservice.model.RequestDTO;
import com.mdtlabs.coreplatform.userservice.repository.UserPreferencesRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserRepository;
import com.mdtlabs.coreplatform.userservice.repository.UserSupervisorRepository;
import com.mdtlabs.coreplatform.userservice.service.OrganizationService;
import com.mdtlabs.coreplatform.userservice.service.RoleService;
import com.mdtlabs.coreplatform.userservice.service.UserService;

/**
 * <p>
 * This service class contain all the business logic for user module and perform
 * all the user operation here.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
@Service
public class UserServiceImpl implements UserService {

    @Value("${app.email-app-url}")
    private String appUrl;

    @Value("${app.forget-password-count-limit}")
    private int forgotPasswordCountLimit;

    @Value("${app.forgot-password-time-limit-in-minutes}")
    private int forgotPasswordtimeLimitInMinutes;

    @Value("${app.login-count-limit}")
    private int loginCountLimit;

    @Value("${app.login-time-limit-in-hour}")
    private int loginTimeLimitInHour;

    @Value("${app.mail-user}")
    private String mailUser;

    @Value("${app.reset-password-count-limit}")
    private int resetPasswordCountLimit;

    @Value("${app.reset-password-time-limit-in-minutes}")
    private int resetPasswordtimeLimitInMinutes;

    @Value("${app.sms-reset-url}")
    private String smsResetUrl;

    @Value("${app.shorten-app}")
    private String shortenApp;

    @Value("${app.environment:}")
    private String environment;

    private final ModelMapper modelMapper = new ModelMapper();
    private final UserRepository userRepository;
    private final NotificationInterface notificationInterface;
    private final FhirServiceApiInterface fhirServiceApiInterface;
    private final UserSupervisorRepository userSupervisorRepository;
    private final UserMapper userMapper;
    private final RoleService roleService;
    private final OrganizationUtil organizationUtil;
    private final RedisTemplate<String, String> redisTemplate;
    private final OrganizationService organizationService;
    private final UserPreferencesRepository userPreferencesRepository;
    private final UserTokenRepository userTokenRepository;
    private final ShortUrlInterface shortUrlInterface;

    private static final int URL_LENGTH = 10;

    public UserServiceImpl(UserRepository userRepository, NotificationInterface notificationInterface,
                           FhirServiceApiInterface fhirServiceApiInterface,
                           UserSupervisorRepository userSupervisorRepository, UserMapper userMapper, RoleService roleService,
                           OrganizationUtil organizationUtil, RedisTemplate<String, String> redisTemplate,
                           @Lazy OrganizationService organizationService, UserPreferencesRepository userPreferencesRepository,
                           UserTokenRepository userTokenRepository, ShortUrlInterface shortUrlInterface) {
        this.userRepository = userRepository;
        this.notificationInterface = notificationInterface;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.userSupervisorRepository = userSupervisorRepository;
        this.userMapper = userMapper;
        this.roleService = roleService;
        this.organizationUtil = organizationUtil;
        this.redisTemplate = redisTemplate;
        this.organizationService = organizationService;
        this.userPreferencesRepository = userPreferencesRepository;
        this.userTokenRepository = userTokenRepository;
        this.shortUrlInterface = shortUrlInterface;
    }

    /**
     * {@inheritDoc}
     */
    public User createUser(User user, boolean needToValidate) {
        if (needToValidate) {
            validateUser(user);
            userRepository.save(user);
            forgotPassword(user, Boolean.TRUE, UserContextHolder.getUserDto().getClient(), null);
        } else {
            userRepository.save(user);
        }
        return user;
    }

    /**
     * <p>
     * Validates the provided {@link User} object for necessary fields and uniqueness in the system.
     * This method checks if the user object is not null, has a username, has at least one role, has a valid phone number
     * according to the defined regex, and ensures the username or phone number does not already exist in the system.
     * If any validation fails, an appropriate exception is thrown.
     * </p>
     *
     * @param user The {@link User} object to validate.
     * @throws BadRequestException if the user object fails validation checks for nullability, roles, or phone number format.
     * @throws SpiceValidation     if the username or phone number already exists in the system.
     */
    private void validateUser(User user) {
        if (Objects.isNull(user)
                || Objects.isNull(user.getUsername())
                || (Objects.isNull(user.getRoles()) || user.getRoles().isEmpty())
                || StringUtils.isEmpty(user.getPhoneNumber())
                || !user.getPhoneNumber().matches(Constants.NUMBER_REGEX)) {
            throw new BadRequestException(2001);
        }
        User existingUser = userRepository.getUserByUsernameOrPhoneNumber(user.getUsername().toLowerCase(), user.getPhoneNumber(), user.getCountryCode());
        if (!Objects.isNull(existingUser)) {
            throw new SpiceValidation(2002);
        }

    }

    /**
     * <p>
     * Attempts to reset a user's password by generating a new JWT token.
     * This method checks if the user has exceeded the limit for password reset attempts within a given timeframe.
     * If the limit is not exceeded, it generates a new JWT token for password reset, saves it to the user's profile,
     * and sends an email with the token. If any part of this process fails, it logs the error and returns false.
     * If the user is null, it immediately returns true, assuming no action is needed.
     * </p>
     *
     * @param user           The {@link User} object for which the password reset is being attempted.
     * @param isFromCreation A boolean flag indicating if the password reset request is part of the user creation process.
     * @param appType        The type of application requesting the password reset, used to customize the email content.
     * @return Boolean indicating the success of the password reset attempt. Returns true if the process is successful
     * or if the user object is null (indicating no action needed), and false if the password reset limit is exceeded
     * or if an error occurs during the process.
     */
    private Boolean forgotPassword(User user, boolean isFromCreation, String appType, String client) {
        if (null != user) {

            boolean forgotPasswordLimitExceed = checkForgotPasswordLimitExceed(user, isFromCreation);
            if (forgotPasswordLimitExceed) {
                Logger.logError(StringUtil.constructString(ErrorConstants.PASSWORD_RESET_ERROR_MESSAGE));
                throw new ForbiddenException(20007);
            } else {
                String jwtToken = null;
                String shortToken = null;
                try {
                    jwtToken = forgotPasswordTokenCreation(user);
                    shortToken = getShortToken();
                    user.setForgetPasswordToken(jwtToken);
                    user.setForgetPasswordShortToken(shortToken);
                    user.setShortenUrl(getShortenUrl(user.getForgetPasswordShortToken()));
                    if (Constants.CLIENT_SPICE_MOBILE.equals(client)) {
                        sendForgetPasswordSms(user);
                    }
                    sendEmail(user, isFromCreation, appType);
                    userRepository.save(user);
                    return Boolean.TRUE;
                } catch (Exception exception) {
                    Logger.logError(String.valueOf(exception));
                }
            }
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    /**
     * <p>
     * Checks if the user has exceeded the limit for password reset attempts within a specified timeframe.
     * This method increments the user's password reset attempt count and compares it against a predefined limit.
     * If the count exceeds the limit within the specified timeframe, the user's attempt count is reset, and the method returns true,
     * indicating the limit has been exceeded. Otherwise, it updates the user's attempt count and time of the last attempt.
     * </p>
     *
     * @param user           The {@link User} object whose password reset attempts are being checked.
     * @param isFromCreation A boolean flag indicating if the check is part of the user creation process.
     * @return Boolean True if the password reset attempt limit has been exceeded, false otherwise.
     */
    private Boolean checkForgotPasswordLimitExceed(User user, boolean isFromCreation) {
        int forgotPasswordCount = user.getForgetPasswordCount(); //5
        forgotPasswordCount++;
        Date forgotPasswordTime = DateUtil.formatDate(user.getForgetPasswordTime());
        Date currentDate = DateUtil.formatDate(new Date());
        long getDateDiffInMinutes = DateUtil.getDateDiffInMinutes(forgotPasswordTime, currentDate);
        if (getDateDiffInMinutes >= forgotPasswordtimeLimitInMinutes) { //60
            user.setForgetPasswordTime(currentDate);
            user.setForgetPasswordCount(Constants.ONE);
        } else {
            if (forgotPasswordCount < forgotPasswordCountLimit && forgotPasswordCount >= 0 && !isFromCreation) {
                user.setForgetPasswordTime(currentDate);
                user.setForgetPasswordCount(forgotPasswordCount);
            }
            if (forgotPasswordCount >= forgotPasswordCountLimit) {
                user.setForgetPasswordCount(forgotPasswordCountLimit);
                userRepository.save(user);
                return Boolean.TRUE;
            }
        }
        user.setForgetPasswordTime(currentDate);
        userRepository.save(user);
        return Boolean.FALSE;
    }

    /**
     * <p>
     * Generates a JWT token for password reset purposes.
     * This method creates a JWT token with the user's username as a claim, signs it with a predefined secret key,
     * and sets an expiration time. The token is intended for use in password reset operations, allowing the system
     * to verify the user's identity and authorize a password change.
     * </p>
     *
     * @param user The {@link User} entity for which the password reset token is being generated.
     * @return String The generated JWT token for password reset.
     */
    private String forgotPasswordTokenCreation(User user) {
        MacAlgorithm signatureAlgorithm = SIG.HS256;
        byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(Constants.AES_KEY_TOKEN);
        Map<String, Object> userInfo = new HashMap<>();
        userInfo.put(Constants.USERNAME, user.getUsername());
        JwtBuilder jwt = Jwts.builder().claims(userInfo).
                signWith(Keys.hmacShaKeyFor(apiKeySecretBytes), signatureAlgorithm).id(user.getId().toString())
                .expiration(DateUtil.addMinutesToCurrentDate(forgotPasswordtimeLimitInMinutes)).issuedAt(new Date()).issuer(Constants.ISSUER);
        return jwt.compact();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean forgetPassword(String username, String appType, String type, String client) {
        User user = username.matches(Constants.NUMBER_REGEX) ? userRepository.findByPhoneNumberAndIsDeletedFalse(username)
                : userRepository.getUserByUsername(username);
        if (Objects.isNull(user)) {
            throw new DataNotFoundException(2003);
        }
        return forgotPassword(user, Boolean.FALSE, appType, client);
    }

    /**
     * {@inheritDoc}
     */
    public void sendEmail(User user, boolean isFromCreation, String appType) {
        ResponseEntity<EmailTemplate> emailTemplateResponse = null;
        emailTemplateResponse = notificationInterface
                .getEmailTemplate((isFromCreation ? Constants.NEW_USER_CREATION : Constants.FORGOT_PASSWORD_USER),
                        appType);
        if (Objects.isNull(emailTemplateResponse) || Objects.isNull(emailTemplateResponse.getBody())) {
            throw new SpiceValidation(2004);
        }
        EmailDTO emailDto = new EmailDTO();
        Map<String, String> data = new HashMap<>();
        try {
            if (isFromCreation && null != emailTemplateResponse.getBody()) {
                emailDto = constructUserCreationEmail(user, data, emailDto, emailTemplateResponse.getBody());
            } else {
                emailDto = constructForgotEmail(user, data, emailDto, emailTemplateResponse.getBody());
            }
        } catch (Exception e) {
            e.getMessage();
        }
        createOutBoundEmail(emailDto);
    }

    /**
     * <p>
     * Constructs an email for user creation.
     * This method prepares the email content for new user creation notifications. It dynamically generates the email
     * content based on the provided user details, JWT token, and application type. The method updates the email data map
     * with specific values such as the application URL and the user's email address. It leverages an {@link EmailTemplate}
     * to format the email content appropriately.
     * </p>
     *
     * @param user          The {@link User} entity for whom the email is being constructed.
     * @param data          A map holding key-value pairs for dynamic content replacement in the email template.
     * @param emailDto      The {@link EmailDTO} object that will be populated with the email content.
     * @param emailTemplate The {@link EmailTemplate} defining the structure and content of the email.
     * @return The populated {@link EmailDTO} with the constructed email content.
     */
    private EmailDTO constructUserCreationEmail(User user, Map<String, String> data, EmailDTO emailDto,
                                                EmailTemplate emailTemplate) {
        data.put(Constants.APP_URL_EMAIL, user.getShortenUrl());
        data.put(Constants.NAME, StringUtil.concatString(user.getFirstName(), Constants.SPACE, user.getLastName()));
        data.put(Constants.EMAIL, user.getUsername());
        return userMapper.setUserCreationEmailTemplate(user, emailTemplate, emailDto, data);
    }

    /**
     * <p>
     * Constructs an email for the password reset process.
     * This method prepares the email content for password reset notifications by dynamically generating the email content
     * based on the provided user details, JWT token, and application type. It updates the email data map with specific values
     * such as the application URL with the token and the expiration time. It leverages an {@link EmailTemplate} to format
     * the email content appropriately.
     * </p>
     *
     * @param user          The {@link User} entity for whom the email is being constructed.
     * @param data          A map holding key-value pairs for dynamic content replacement in the email template.
     * @param emailDto      The {@link EmailDTO} object that will be populated with the email content.
     * @param emailTemplate The {@link EmailTemplate} defining the structure and content of the email.
     * @return The populated {@link EmailDTO} with the constructed email content.
     */
    private EmailDTO constructForgotEmail(User user, Map<String, String> data, EmailDTO emailDto,
                                          EmailTemplate emailTemplate) {
        data.put(Constants.APP_URL_EMAIL, user.getShortenUrl());
        data.put(Constants.NAME, StringUtil.concatString(user.getFirstName(), Constants.SPACE, user.getLastName()));
        data.put(Constants.EMAIL, user.getUsername());
        return userMapper.setForgotPasswordEmailTemplate(emailTemplate, mailUser, user, emailDto, data);
    }

    /**
     * <p>
     * Sends an outbound email.
     * This method is responsible for sending the constructed email to the intended recipient. It maps the {@link EmailDTO}
     * to an {@link OutBoundEmailDTO} and makes a call to an external service to send the email. If the external service
     * indicates failure, a {@link SpiceValidation} exception is thrown.
     * </p>
     *
     * @param emailDto The {@link EmailDTO} containing the email content and recipient information.
     */
    private void createOutBoundEmail(EmailDTO emailDto) {
        OutBoundEmailDTO outBoundEmailDto = modelMapper.map(emailDto, OutBoundEmailDTO.class);
        ResponseEntity<Boolean> emailResponse = notificationInterface.createOutBoundEmail(outBoundEmailDto);
        if (Boolean.FALSE.equals(emailResponse.getBody())) {
            throw new SpiceValidation(2006);
        }
    }

    /**
     * {@inheritDoc}
     */
    public User verifyJwtToken(String token) {
        User user = userRepository.findByForgetPasswordShortTokenAndIsDeletedFalseAndIsActiveTrue(token);
        if (Objects.isNull(user)) {
            throw new DataNotFoundException(2007);
        }
        try {
            byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(Constants.AES_KEY_TOKEN);
            Key secretKeySpec = Keys.hmacShaKeyFor(apiKeySecretBytes);
            Jwts.parser().verifyWith((SecretKey) secretKeySpec).build()
                    .parseSignedClaims(user.getForgetPasswordToken()).getPayload();
        } catch (ExpiredJwtException exception) {
            Logger.logError(StringUtil.constructString(ErrorConstants.LINK_EXPIRED));
            throw new ForbiddenException(2008);
        }
        return user;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean updatePassword(String token, Map<String, String> userInfo) {
        if (Objects.isNull(userInfo.get(FieldConstants.PASSWORD))) {
            throw new DataNotAcceptableException(2009);
        }
        User user = verifyJwtToken(token);
        if (!Objects.isNull(user.getPassword()) && user.getPassword().equals(userInfo.get(FieldConstants.PASSWORD))) {
            Logger.logError(StringUtil.constructString(ErrorConstants.SAME_PASSWORD));
            throw new SpiceValidation(2010);
        }
        setPassword(user, userInfo.get(FieldConstants.PASSWORD));
        userRepository.save(user);
        return Boolean.TRUE;
    }

    /**
     * {@inheritDoc}
     */
    public void changeSiteUserPassword(SearchRequestDTO request) {
        if (Objects.isNull(request) || Objects.isNull(request.getUserId())
                || Objects.isNull(request.getNewPassword())
                || request.getNewPassword().isBlank()) {
            throw new DataNotAcceptableException(2011);
        }
        User user =
                userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(request.getUserId());
        if (Objects.isNull(user)) {
            throw new DataNotFoundException(2003);
        }
        setPassword(user, request.getNewPassword());
        userRepository.save(user);
    }

    /**
     * {@inheritDoc}
     */
    public void validateUsers(List<UserRequestDTO> users) {
        boolean isValidUser = true;
        List<String> usernames = new ArrayList<>();
        List<String> phoneNumbers = new ArrayList<>();
        List<String> uniqueUsernames = new ArrayList<>();
        List<String> uniquePhoneNumbers = new ArrayList<>();

        for (UserRequestDTO user : users) {
            if (Objects.isNull(user.getUsername())
                    || (Objects.isNull(user.getRoleIds()) || user.getRoleIds().isEmpty())
                    || Objects.isNull(user.getPhoneNumber())
                    || !user.getPhoneNumber().matches(Constants.NUMBER_REGEX)) {
                isValidUser = false;
                break;
            } else if (Objects.isNull(user.getId())) {
                usernames.add(user.getUsername());
                phoneNumbers.add(user.getPhoneNumber());
            }
            if ((!uniqueUsernames.isEmpty() && uniqueUsernames.contains(user.getUsername()))
                    || (!uniquePhoneNumbers.isEmpty() && uniquePhoneNumbers.contains(user.getPhoneNumber()))) {
                throw new DataConflictException(2026);
            }
            uniqueUsernames.add(user.getUsername());
            uniquePhoneNumbers.add(user.getPhoneNumber());
        }
        if (!isValidUser) {
            throw new BadRequestException(2001);
        }
        List<User> existingUsers = userRepository.getUsersByUsernameOrPhoneNumber(usernames, phoneNumbers);
        users.stream().forEach(userObj -> {
            List<User> filteredUsers = existingUsers.stream()
                    .filter(user -> (user.getPhoneNumber().equals(userObj.getPhoneNumber()) && user.getCountryCode().equals(userObj.getCountryCode()))
                            || (user.getUsername().equals(userObj.getUsername())))
                    .toList();

            if (!filteredUsers.isEmpty()) {
                throw new SpiceValidation(2003);
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    public List<User> createUsers(List<User> users, List<Long> existingUserIds) {
        List<User> userResponse = null;
        userResponse = userRepository.saveAll(users);
        for (User user : userResponse) {
            if (!existingUserIds.contains(user.getId())) {
                forgotPassword(user, Boolean.TRUE, UserContextHolder.getUserDto().getClient(), null);
            }
        }
        return userResponse;
    }

    /**
     * {@inheritDoc}
     */
    public List<User> createUsers(List<User> users) {
        List<User> userResponse = null;
        userResponse = userRepository.saveAll(users);
        for (User user : userResponse) {
            forgotPassword(user, Boolean.TRUE, UserContextHolder.getUserDto().getClient(), null);
        }
        return userResponse;
    }

    /**
     * {@inheritDoc}
     */
    public List<User> saveAllUsers(List<User> users) {
        return userRepository.saveAll(users);
    }

    /**
     * {@inheritDoc}
     */
    public User saveUser(User user) {
        return userRepository.save(user);
    }

    @Override
    public List<User> getUserByVillageIds(Set<Long> villageIds, Long id) {
        return userRepository.findAllByVillageIds(villageIds, id);
    }

    /**
     * {@inheritDoc}
     */
    public void addOrganizationForUsers(List<Long> linkedSupervisorIds, Organization organization,
                                        List<String> appTypes) {
        boolean isCommunityApp = CommonUtil.isCommunityApp(appTypes);
        String roleName = isCommunityApp ? Constants.PEER_SUPERVISOR : Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT;
        List<User> peerSupervisors = userRepository.getUserByRoleOrId(roleName, List.of(organization.getId()),
                linkedSupervisorIds);
        List<String> deletedUsers = new ArrayList<>();
        Map<Long, String> usernameIdMap = new HashMap<>();
        if (!Objects.isNull(peerSupervisors) && !peerSupervisors.isEmpty()) {
            peerSupervisors.forEach(user -> {
                if (!linkedSupervisorIds.contains(user.getId())) {
                    List<UserSupervisor> userSupervisors = userSupervisorRepository.findBySupervisorIdAndIsDeletedAndIsActive(
                            user.getId(), false, true);
                    user.getOrganizations().removeIf(org -> organization.getId().equals(org.getId()));
                    validatePeerSupervisor(userSupervisors, List.of(organization.getId()), isCommunityApp);
                    if (user.getOrganizations().isEmpty()) {
                        user.setDeleted(true);
                        user.setActive(false);
                        deletedUsers.add(user.getFhirId());
                        usernameIdMap.put(user.getId(), user.getUsername());
                        if (!userSupervisors.isEmpty()) {
                            throw new SpiceValidation(2022);
                        }
                    }
                } else if (user.getOrganizations().stream().noneMatch(org -> organization.getId().equals(org.getId()))) {
                    user.getOrganizations().add(organization);
                }
            });
            userRepository.saveAll(peerSupervisors);
            if (!deletedUsers.isEmpty()) {
                deleteUsersToken(usernameIdMap);
                fhirServiceApiInterface.deleteUsers(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), deletedUsers);
            }
        }

    }

    /**
     * <p>
     * Validates if any of the specified user supervisors are assigned as CHWs (Community Health Workers) within the
     * given tenant IDs. This method checks if any user supervisors, identified by their user IDs, are currently
     * assigned the role of a CHW within the specified tenants. If any such supervisors are found, it throws a
     * {@link SpiceValidation} exception, indicating a validation failure.
     * </p>
     *
     * @param userSupervisors A list of {@link UserSupervisor} entities representing the supervisors to be validated.
     * @param tenantIds       A list of tenant IDs within which the supervisors' roles are to be validated.
     * @throws SpiceValidation if any of the specified supervisors are assigned as CHWs within the given tenant IDs.
     */
    private void validatePeerSupervisor(List<UserSupervisor> userSupervisors, List<Long> tenantIds,
                                        boolean isCommunityApp) {
        Set<Long> userId = isCommunityApp
                ? userSupervisors.stream()
                .filter(userSupervisor -> !(userSupervisor.getSupervisorId().equals(userSupervisor.getUserId())))
                .map(UserSupervisor::getUserId).collect(Collectors.toSet())
                : userSupervisors.stream().map(UserSupervisor::getUserId).collect(Collectors.toSet());

        List<User> userList = userRepository.findAllByRoleNamesAndTenantId(isCommunityApp ? List.of(Constants.ROLE_CHW) : List.of(Constants.ROLE_CHP), tenantIds);
        if (userList.stream().anyMatch(userObj -> userId.contains(userObj.getId()))) {
            throw new SpiceValidation(2022);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void addPeerSupervisors(List<Long> linkedSupervisorIds, Organization organization) {
        List<User> users = userRepository.findByIdIn(linkedSupervisorIds);
        for (User user : users) {
            if (user.getOrganizations().stream().noneMatch(org -> organization.getId().equals(org.getId()))) {
                user.getOrganizations().add(organization);
            }
        }
        userRepository.saveAll(users);
    }

        /**
     * {@inheritDoc}
     */
    public List<User> getUsersByIds(List<Long> ids) {
        List<User> users = userRepository.findByIdInAndIsDeletedAndIsActive(ids, false, true);
        return Objects.isNull(users) ? new ArrayList<>() : users;
    }

    /**
     * {@inheritDoc}
     */
    public User getUserById(Long id) {
        User user = userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(id);
        if (Objects.isNull(user)) {
            throw new DataNotFoundException(2003);
        }
        return user;
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO getUserDetails(Long id) {
        User user = getUserById(id);
        UserResponseDTO userResponse = modelMapper.map(user, UserResponseDTO.class);
        UserSupervisor userSupervisor = userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(id);
        if (!Objects.isNull(userSupervisor)) {
            User supervisor = userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(userSupervisor.getSupervisorId());
            if (!Objects.isNull(supervisor)) {
                userResponse.setSupervisor(modelMapper.map(supervisor, UserResponseDTO.class));
            }
        }
        return userResponse;
    }

    /**
     * {@inheritDoc}
     */
    public User updateOrganizationUser(UserRequestDTO request) {
        if (Objects.isNull(request.getId())) {
            throw new DataNotAcceptableException(2013);
        }
        User existingUser = getUserById(request.getId());
        userMapper.setExistingUser(request, existingUser);

        existingUser = userRepository.save(existingUser);
        if (Objects.nonNull(existingUser.getFhirId())) {
            request.setFhirId(existingUser.getFhirId());
            fhirServiceApiInterface.updateUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request);
        }
        return existingUser;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<UserResponseDTO> getUsersByTenants(SearchRequestDTO request) {
        ResponseListDTO<UserResponseDTO> response = new ResponseListDTO<>();
        String searchTerm = request.getSearchTerm();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.USER_SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        if(CommonUtil.isCommunityApp(request.getAppTypes())) {
            Pageable pageable = Pagination.setPagination(request.getSkip(), request.getLimit(),
                    Constants.UPDATED_AT, Constants.BOOLEAN_FALSE);
            List<Long> tenantIds = (Objects.nonNull(request.getTenantIds()) && !request.getTenantIds().isEmpty())
                    ? request.getTenantIds() : null;
            Long phoneNumber = CommonUtil.isAllNumeric(searchTerm) ? Long.valueOf(searchTerm) : null;
            List<String> roles = (Objects.isNull(request.getRoleNames()) || request.getRoleNames().isEmpty())
                    ? null : request.getRoleNames();

            Page<User> users = userRepository.getAllUsers(
                    Objects.isNull(tenantIds) ? null : new HashSet<>(tenantIds),
                    Objects.isNull(phoneNumber) ? searchTerm : null,
                    request.getCountryId(), roles, pageable, phoneNumber);
            if (!Objects.isNull(users) && !users.isEmpty()) {
                response.setData(modelMapper.map(users.stream().toList(), new TypeToken<List<UserResponseDTO>>() {
                }.getType()));
                response.setTotalCount(users.getTotalElements());
            }
            return response;
        }
        List<Sort.Order> sorts = new ArrayList<>();
        sorts.add(new Sort.Order(Sort.Direction.DESC, Constants.UPDATED_AT));
        sorts.add(new Sort.Order(Sort.Direction.DESC, Constants.ID));
        Pageable pageable = Pagination.setPagination(request.getSkip(), request.getLimit(),
                sorts);
        List<String> roleNames = null;
        List<String> notInRoleNames = null;
        Map<String, List<RoleResponseDTO>> roleMap = roleService.getRoleGroups(request);
        List<RoleResponseDTO> roles = roleMap.get(Constants.GROUP_NAME_SPICE);
        if (Objects.nonNull(roleMap.get(Constants.GROUP_NAME_SPICE_INSIGHTS))) {
            roles.addAll(roleMap.get(Constants.GROUP_NAME_SPICE_INSIGHTS));
        }
        if (Objects.nonNull(roleMap.get(Constants.GROUP_NAME_REPORTS))) {
            roles.addAll(roleMap.get(Constants.GROUP_NAME_REPORTS));
        }

        List<String> suiteAccessList;
        List<String> notInSuiteAccessList;
        if (Objects.nonNull(request.getIsSiteUsers())) {
            if (request.getIsSiteUsers().equals(Constants.BOOLEAN_TRUE)) {
                suiteAccessList = new ArrayList<>(Arrays.asList(Constants.CLIENT_CFR_USER, Constants.CLIENT_SPICE_MOBILE, Constants.CLIENT_CFR));
                notInSuiteAccessList = new ArrayList<>(Arrays.asList(Constants.CLIENT_CFR_ADMIN, Constants.CLIENT_ADMIN));
            } else {
                suiteAccessList = new ArrayList<>(Arrays.asList(Constants.CLIENT_CFR_ADMIN, Constants.CLIENT_ADMIN, Constants.CLIENT_CFR));
                notInSuiteAccessList = new ArrayList<>(Arrays.asList(Constants.CLIENT_CFR_USER, Constants.CLIENT_SPICE_MOBILE));
            }
        } else {
            suiteAccessList = new ArrayList<>(Arrays.asList(Constants.CLIENT_ADMIN, Constants.CLIENT_SPICE_MOBILE, Constants.CLIENT_CFR_ADMIN,
                    Constants.CLIENT_CFR_USER, Constants.CLIENT_CFR_QUICKSIGHT_ADMIN, Constants.CLIENT_CFR));
            notInSuiteAccessList = new ArrayList<>();
        }

        if (Objects.nonNull(request.getIsSiteUsers())) {
            roleNames = roles.stream()
                    .filter(roleResponseDTO -> suiteAccessList.contains(roleResponseDTO.getSuiteAccessName()))
                    .map(RoleResponseDTO::getName)
                    .collect(Collectors.toList());
            notInRoleNames = roles.stream()
                    .filter(roleResponseDTO -> notInSuiteAccessList.contains(roleResponseDTO.getSuiteAccessName()))
                    .map(RoleResponseDTO::getName)
                    .toList();
        }

        List<String> requestRoleNames = request.getRoleNames();
        Set<Long> tenantIds = new HashSet<>();
        roleNames = (Objects.nonNull(requestRoleNames) && requestRoleNames.isEmpty()) ? roleNames : requestRoleNames;

        if (Objects.nonNull(request.getTenantIds()) && !request.getTenantIds().isEmpty()) {
            for (Long tenantId : request.getTenantIds()) {
                List<Long> ids = organizationUtil.getParentChildTenantMap().get(tenantId);
                if (Objects.nonNull(ids)) {
                    tenantIds.addAll(organizationUtil.getParentChildTenantMap().get(tenantId));
                }
            }
        } else if (Objects.nonNull(request.getTenantId())) {
            tenantIds.addAll(organizationUtil.getParentChildTenantMap().get(request.getTenantId()));
        }
        Organization organization = Objects.nonNull(request.getTenantId()) ? organizationService.getOrganization(request.getTenantId()) : null;
        tenantIds = tenantIds.isEmpty() ? null : tenantIds;
        Boolean superAdminUsers = (Objects.nonNull(organization) && request.getCountryId().equals(organization.getFormDataId())) &&
                (Objects.nonNull(request.getIsSiteUsers()) && request.getIsSiteUsers().equals(Constants.BOOLEAN_FALSE))
                && (Objects.nonNull(roleNames) && roleNames.contains(Constants.ROLE_SUPER_ADMIN))
                ? Constants.BOOLEAN_TRUE : Constants.BOOLEAN_FALSE;
        List<Role> rolesList = roleService.getAllRoles();
        Map<String, Role> rolesMap = new HashMap<>();
        rolesList.forEach(role -> rolesMap.put(role.getName(), role));
        Page<Map<String, Object>> users;
        if (superAdminUsers) {
            users = userRepository.getUsersWithSuperAdminsNative(tenantIds.toArray(new Long[0]), searchTerm, (Objects.nonNull(roleNames) ? roleNames.toArray(new String[0]) : null),
                    (Objects.nonNull(notInRoleNames) ? notInRoleNames.toArray(new String[0]) : null), Constants.SUPER_USER_USERNAME, pageable);
        } else {
            users = userRepository.getUsersnative(tenantIds.toArray(new Long[0]), searchTerm, null, (Objects.nonNull(roleNames) ? roleNames.toArray(new String[0]) : null),
                    (Objects.nonNull(notInRoleNames) ? notInRoleNames.toArray(new String[0]) : null),
                    List.of(Constants.ROLE_REPORT_ADMIN, Constants.ROLE_FACILITY_REPORT_ADMIN).toArray(new String[0]), request.getIsFacilityUsersOnly(), pageable);
        }
        List<User> userList = new ArrayList<>();
        List<Map<String, Object>> mappedResult = users.getContent().stream().toList();
        setUsers(mappedResult, userList, rolesMap);
        List<User> usersList = userList.stream().toList();
        if (!Objects.isNull(userList) && !userList.isEmpty()) {
            response.setData(modelMapper.map(usersList, new TypeToken<List<UserResponseDTO>>() {
            }.getType()));
            response.setTotalCount(users.getTotalElements());
        }
        return response;
    }

    /**
     * <p>
     * Populates a list of User objects using data from a list of row maps and assigns various attributes
     * such as country, culture, designation, timezone, roles, and organizations to each User.
     * </p>
     *
     * @param mappedResult a List of Maps, where each map represents a row of user data. Each map contains
     *                     key-value pairs corresponding to user attributes (e.g., roles, country, timezone).
     * @param testUsers    a List to which the created User objects will be added.
     * @param roleMap      a Map where keys are role names (String) and values are Role objects, used to
     *                     assign roles to the User objects.
     *
     * This method iterates over each map in the mappedResult list, creates a User object, and populates it
     * with various fields such as personal details, roles, and organizations. The helper methods `setData`,
     * `getCountry`, `getCulture`, `getDesignation`, `getTimezone`, `setRoles`, `setOrganization`,
     * `setInsightUserOrganizations`, and `setReportUserOrganizations` are used to assign specific attributes
     * to the User. Once all the attributes are set, the User is added to the `testUsers` list.
     * - Each created User is added to the `testUsers` list, which is passed by reference.
     */
    private static void setUsers(List<Map<String, Object>> mappedResult, List<User> testUsers, Map<String, Role> roleMap) {
        mappedResult.forEach(row -> {
            User user = new User();
            setData(row, user);
            user.setCountry(getCountry(row));
            user.setCulture(getCulture(row));
            user.setDesignation(getDesignation(row));
            user.setTimezone(getTimezone(row));
            setRoles(roleMap, row, user);
            setOrganization(row, user);
            setInsightUserOrganizations(row, user);
            setReportUserOrganizations(row, user);
            testUsers.add(user);
        });
    }

    /**
     * <p>
     * Sets the Report User Organization for the specified User object using data from the provided row map.
     * </p>
     *
     * @param row   a Map containing the user-related organization data. Expected keys include:
     *              - "reportUserOrganizationId" (Long or String) representing the organization's ID.
     *              - "reportUserOrganizationName" (String) representing the organization's name.
     *              - "reportUserOrganizationFormDataId" (Long or String) representing the form data ID.
     *              - "reportUserOrganizationParentOrganizationId" (Long or String) representing the parent organization ID.
     *              - "reportUserOrganizationFormName" (String) representing the organization's form name.
     * @param user  the User object to which the Report User Organization will be assigned.
     *
     * The method creates an Organization object using data from the row map, setting the ID, name, form data ID,
     * parent organization ID, and form name. If the "reportUserOrganizationId" is present in the row, the newly
     * created organization is added to the User's Report User Organization list. If the ID is absent, an empty list
     * is assigned instead.
     * - The Report User Organization is stored as a list, even if it contains only a single organization.
     */
    private static void setReportUserOrganizations(Map<String, Object> row, User user) {
        List<Organization> reportUserOrganizationList = new ArrayList<>();
        Organization reportUserOrganization = new Organization();
        reportUserOrganization.setId(Objects.nonNull(row.get(Constants.REPORT_USER_ORGANIZATION_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.REPORT_USER_ORGANIZATION_ID))) : null);
        reportUserOrganization.setName(String.valueOf(row.get(Constants.REPORT_USER_ORGANIZATION_NAME)));
        reportUserOrganization.setFormDataId(Objects.nonNull(row.get(Constants.REPORT_USER_ORGANIZATION_FORM_DATA_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.REPORT_USER_ORGANIZATION_FORM_DATA_ID))) : null);
        reportUserOrganization.setParentOrganizationId(Objects.nonNull(row.get(Constants.REPORT_USER_ORGANIZATION_PARENT_ORGANIZATION_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.REPORT_USER_ORGANIZATION_PARENT_ORGANIZATION_ID))) : null);
        reportUserOrganization.setFormName(String.valueOf(row.get(Constants.REPORT_USER_ORGANIZATION_FORM_NAME)));
        reportUserOrganizationList.add(reportUserOrganization);
        user.setReportUserOrganization(Objects.nonNull(row.get(Constants.REPORT_USER_ORGANIZATION_ID)) ? reportUserOrganizationList : new ArrayList<>());
    }

    /**
     * <p>
     * Sets the Insight User Organization for the specified User object using data from the provided row map.
     * </p>
     *
     * @param row   a Map containing the user-related organization data. Expected keys include:
     *              - "insightUserOrganizationId" (Long or String) representing the organization's ID.
     *              - "insightUserOrganizationName" (String) representing the organization's name.
     *              - "insightUserOrganizationFormDataId" (Long or String) representing the form data ID.
     *              - "insightUserOrganizationParentOrganizationId" (Long or String) representing the parent organization ID.
     *              - "insightUserOrganizationFormName" (String) representing the organization's form name.
     * @param user  the User object to which the Insight User Organization will be assigned.
     * The method creates an Organization object using the data from the row map, including setting the ID,
     * name, form data ID, parent organization ID, and form name. If the "insightUserOrganizationId" is present
     * in the row, the newly created organization is added to the User's Insight User Organization list. If the ID
     * is absent, an empty list is assigned instead.
     */
    private static void setInsightUserOrganizations(Map<String, Object> row, User user) {
        List<Organization> insightUserOrganizationList = new ArrayList<>();
        Organization insightUserOrganization = new Organization();
        insightUserOrganization.setId(Objects.nonNull(row.get(Constants.INSIGHT_USER_ORGANIZATION_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.INSIGHT_USER_ORGANIZATION_ID))) : null);
        insightUserOrganization.setName(String.valueOf(row.get(Constants.INSIGHT_USER_ORGANIZATION_NAME)));
        insightUserOrganization.setFormDataId(Objects.nonNull(row.get(Constants.INSIGHT_USER_ORGANIZATION_FORM_DATA_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.INSIGHT_USER_ORGANIZATION_FORM_DATA_ID))) : null);
        insightUserOrganization.setParentOrganizationId(Objects.nonNull(row.get(Constants.INSIGHT_USER_ORGANIZATION_PARENT_ORGANIZATION_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.INSIGHT_USER_ORGANIZATION_PARENT_ORGANIZATION_ID))) : null);
        insightUserOrganization.setFormName(String.valueOf(row.get(Constants.INSIGHT_USER_ORGANIZATION_FORM_NAME)));
        insightUserOrganizationList.add(insightUserOrganization);
        user.setInsightUserOrganization(Objects.nonNull(row.get(Constants.INSIGHT_USER_ORGANIZATION_ID)) ? insightUserOrganizationList : new ArrayList<>());
    }

    /**
     * <p>
     * Assigns roles to a User object based on the roles specified in the provided row map.
     * </p>
     *
     * @param rolesMap a Map where the key is a role name (String) and the value is a Role object.
     *                 This map is used to look up Role objects corresponding to the roles defined for the user.
     * @param row      a Map containing user data, including a "roles" key that holds a comma-separated list
     *                 of role names (String).
     * @param user     the User object to which the roles will be assigned.
     *
     * The method extracts the "roles" string from the row map, splits it into a list of role names,
     * and looks up the corresponding Role objects in the rolesMap. These Role objects are then added
     * to a Set, which is subsequently assigned to the User object's roles field.
     * - The roles are stored in a Set to ensure uniqueness and eliminate duplicates.
     */
    private static void setRoles(Map<String, Role> rolesMap, Map<String, Object> row, User user) {
        String rolesString = (String) row.get(Constants.ROLE_REDIS_KEY);
        List<String> userRoles = Arrays.asList(rolesString.split(Constants.COMMA));
        Set<Role> roleSet = new HashSet<>();
        userRoles.forEach(role -> {
            roleSet.add(rolesMap.get(role));
        });
        user.setRoles(roleSet);
    }

    /**
     * <p>
     * Populates the organizations associated with a User object based on data from a provided row map.
     * </p>
     *
     * @param row  a Map containing organization-related data. Expected keys vary
     * @param user the User object to which organizations will be added. Any existing organizations
     *             associated with the user will remain and be augmented with new ones.
     *
     * The method identifies the type of organization data to process based on the "formName" field:
     * - For "chiefdom" forms, a single district-level organization is created and added.
     * - For "health facility" forms, both district-level and chiefdom-level organizations are created and added.
     *
     * Existing organizations in the User object are retained, and the new organizations are merged into the set.
     * - Each new organization is assigned an ID, form data ID, parent organization ID, name, and form type.
     */
    private static void setOrganization(Map<String, Object> row, User user) {
        Set<Organization> organizations = new HashSet<>();
        if (Constants.FORM_NAME_DISTRICT.equals(String.valueOf(row.get(Constants.FORM_NAME)))) {
            Organization districtOrganization = new Organization();
            districtOrganization.setId(Long.parseLong(String.valueOf(row.get(Constants.DISTRICT_TENANT_ID))));
            districtOrganization.setFormDataId(Long.parseLong(String.valueOf(row.get(Constants.DISTRICT_ID))));
            districtOrganization.setParentOrganizationId(Long.parseLong(String.valueOf(row.get(Constants.DISTRICT_PARENT_ORGANIZATION_ID))));
            districtOrganization.setName(String.valueOf(row.get(Constants.DISTRICT_NAME)));
            districtOrganization.setFormName(Constants.FORM_NAME_DISTRICT);
            organizations.add(districtOrganization);
        }
        else if (Constants.FORM_NAME_CHIEFDOM.equals(String.valueOf(row.get(Constants.FORM_NAME)))) {
            Organization districtOrganization = new Organization();
            districtOrganization.setId(Long.parseLong(String.valueOf(row.get(Constants.CHIEFDOM_DISTRICT_TENANT_ID))));
            districtOrganization.setFormDataId(Long.parseLong(String.valueOf(row.get(Constants.CHIEFDOM_DISTRICT_ID))));
            districtOrganization.setParentOrganizationId(Long.parseLong(String.valueOf(row.get(Constants.CHIEFDOM_DISTRICT_PARENT_ORGANIZATION_ID))));
            districtOrganization.setName(String.valueOf(row.get(Constants.CHIEFDOM_DISTRICT_NAME)));
            districtOrganization.setFormName(Constants.FORM_NAME_DISTRICT);
            organizations.add(districtOrganization);
            Organization chiefdomOrganization = new Organization();
            chiefdomOrganization.setId(Long.parseLong(String.valueOf(row.get(Constants.CHIEFDOM_TENANT_ID))));
            chiefdomOrganization.setFormDataId(Long.parseLong(String.valueOf(row.get(Constants.CHIEFDOM_ID))));
            chiefdomOrganization.setParentOrganizationId(Long.parseLong(String.valueOf(row.get(Constants.CHIEFDOM_PARENT_ORGANIZATION_ID))));
            chiefdomOrganization.setName(String.valueOf(row.get(Constants.CHIEFDOM_NAME)));
            chiefdomOrganization.setFormName(Constants.FORM_NAME_CHIEFDOM);
            organizations.add(chiefdomOrganization);
        } else if (Constants.FORM_NAME_HEALTH_FACILITY.equals(String.valueOf(row.get(Constants.FORM_NAME)))) {
            Organization districtOrganization = new Organization();
            districtOrganization.setId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_DISTRICT_TENANT_ID))));
            districtOrganization.setFormDataId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_DISTRICT_ID))));
            districtOrganization.setParentOrganizationId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_DISTRICT_PARENT_ORGANIZATION_ID))));
            districtOrganization.setName(String.valueOf(row.get(Constants.HEALTH_FACILITY_DISTRICT_NAME)));
            districtOrganization.setFormName(Constants.FORM_NAME_DISTRICT);
            Organization chiefdomOrganization = new Organization();
            chiefdomOrganization.setId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_CHIEFDOM_TENANT_ID))));
            chiefdomOrganization.setFormDataId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_CHIEFDOM_ID))));
            chiefdomOrganization.setParentOrganizationId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_CHIEFDOM_PARENT_ORGANIZATION_ID))));
            chiefdomOrganization.setName(String.valueOf(row.get(Constants.HEALTH_FACILITY_CHIEFDOM_NAME)));
            chiefdomOrganization.setFormName(Constants.FORM_NAME_CHIEFDOM);
            Organization healthFacilityOrganization = new Organization();
            healthFacilityOrganization.setId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_TENANT_ID))));
            healthFacilityOrganization.setFormDataId(Long.parseLong(String.valueOf(row.get(Constants.HEALTHFACILITY_ID))));
            healthFacilityOrganization.setParentOrganizationId(Long.parseLong(String.valueOf(row.get(Constants.HEALTH_FACILITY_PARENT_ORGANIZATION_ID))));
            healthFacilityOrganization.setName(String.valueOf(row.get(Constants.HEALTH_FACILITY_NAME)));
            healthFacilityOrganization.setFormName(Constants.FORM_NAME_HEALTH_FACILITY);
            organizations.add(districtOrganization);
            organizations.add(chiefdomOrganization);
            organizations.add(healthFacilityOrganization);
        }
        organizations.addAll(user.getOrganizations());
        user.setOrganizations(organizations);
    }

    /**
     * <p>
     * Populates the properties of a User object using data from a provided row map.
     * </p>
     *
     * @param row  a Map containing user data with keys corresponding to User fields.
     * @param user the User object to populate with data from the row map.
     *
     * The method converts and maps values from the row map to the corresponding
     * fields in the User object, applying necessary parsing or transformation.
     * If certain keys are missing or null, the corresponding User fields will
     * be set to null (e.g., "suiteAccess", "tenantId").
     */
    private static void setData(Map<String, Object> row, User user) {
        user.setId(Long.parseLong(String.valueOf(row.get(Constants.ID))));
        user.setFirstName(String.valueOf(row.get(Constants.PARAM_FIRST_NAME)));
        user.setUsername(String.valueOf(row.get(Constants.USERNAME)));
        user.setPhoneNumber(String.valueOf(row.get(Constants.PARAM_PHONE_NUMBER)));
        user.setLastName(String.valueOf(row.get(Constants.PARAM_LAST_NAME)));
        user.setCountryCode(String.valueOf(row.get(Constants.COUNTRY_CODE)));
        user.setFhirId(String.valueOf(row.get(Constants.FHIR_ID)));
        String suiteAccessString = Objects.nonNull(row.get(Constants.SUITE_ACCESS)) ? (String) row.get(Constants.SUITE_ACCESS) : null;
        Set<String> suiteAccessSet = suiteAccessString == null ? null : Arrays.stream(suiteAccessString.split(Constants.COMMA)).map(String::trim).collect(Collectors.toSet());
        user.setSuiteAccess(suiteAccessSet);
        user.setTenantId(Objects.nonNull(row.get(Constants.TENANT_PARAMETER_NAME)) ? Long.parseLong(String.valueOf(row.get(Constants.TENANT_PARAMETER_NAME))) : null);
    }

    /**
     * <p>
     * Converts a row from a Map representation into a Timezone object.
     * </p>
     *
     * @param row a Map containing key-value pairs representing timezone attributes.
     *            Expected keys:
     *            - "timezoneId" (optional): The unique identifier for the timezone, expected as a numeric value.
     *            - "offset": The offset value for the timezone, typically a string representation of the UTC offset.
     *            - "description": A textual description of the timezone.
     *
     * @return a Timezone object populated with the values from the row.
     *         If "timezoneId" is missing or invalid, the ID will be set to null.
     *         The "offset" and "description" fields will be set to their string representations.
     */
    private static Timezone getTimezone(Map<String, Object> row) {
        Timezone timezone = new Timezone();
        timezone.setId(Objects.nonNull(row.get(Constants.TIMEZONE_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.TIMEZONE_ID))) : null);
        timezone.setOffset(String.valueOf(row.get(Constants.OFFSET)));
        timezone.setDescription(String.valueOf(row.get(Constants.DESCRIPTION)));
        return timezone;
    }

    /**
     * <p>
     * Converts a row from a Map representation into a Designation object.
     * </p>
     *
     * @param row a Map containing key-value pairs representing designation attributes.
     *            Expected keys:
     *            - "designationId" (optional): The unique identifier for the designation, expected as a numeric value.
     *            - "designationName": The name of the designation.
     *
     * @return a Designation object populated with the values from the row.
     *         If "designationId" is missing or invalid, the ID will be set to null.
     *         The "designationName" field will be set to its string representation.
     */
    private static Designation getDesignation(Map<String, Object> row) {
        Designation designation = new Designation();
        designation.setId(Objects.nonNull(row.get(Constants.DESIGNATION_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.DESIGNATION_ID))) : null);
        designation.setName(String.valueOf(row.get(Constants.DESIGNATION_NAME)));
        return Objects.nonNull(row.get(Constants.DESIGNATION_ID)) ? designation : new Designation();
    }

    /**
     * <p>
     * Converts a row from a Map representation into a Culture object.
     * </p>
     *
     * @param row a Map containing key-value pairs representing culture attributes.
     *            Expected keys:
     *            - "cultureId" (optional): The unique identifier for the culture, expected as a numeric value.
     *            - "cultureName": The name of the culture.
     *            - "cultureCode": The code associated with the culture.
     *
     * @return a Culture object populated with the values from the row. If "cultureId" is missing or invalid,
     *         the ID will be set to null. Other fields are set to their string representations.
     */
    private static Culture getCulture(Map<String, Object> row) {
        Culture culture = new Culture();
        culture.setId(Objects.nonNull(row.get(Constants.CULTURE_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.CULTURE_ID))) : null);
        culture.setName(String.valueOf(row.get(Constants.CULTURE_NAME)));
        culture.setCode(String.valueOf(row.get(Constants.CULTURE_CODE)));
        return culture;
    }

    /**
     * <p>
     * Converts a map of row data into a Country object.
     * </p>
     *
     * This method takes a map containing key-value pairs representing attributes of a Country
     * and populates a Country object with the respective values. It handles null checks
     * and type conversions where necessary.
     *
     * @param row a map where keys are column names and values are the corresponding data
     *            retrieved from a database or another source.
     * @return a Country object populated with the values from the map. If certain keys are missing
     *         or have null values, the corresponding fields in the Country object will be null.
     */
    private static Country getCountry(Map<String, Object> row) {
        Country country = new Country();
        country.setId(Objects.nonNull(row.get(Constants.COUNTRY_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.COUNTRY_ID))) : null);
        country.setName(String.valueOf(row.get(Constants.COUNTRY_NAME)));
        country.setPhoneNumberCode(String.valueOf(row.get(Constants.PHONE_NUMBER_CODE)));
        country.setTenantId(Objects.nonNull(row.get(Constants.COUNTRY_TENANT_ID)) ? Long.parseLong(String.valueOf(row.get(Constants.COUNTRY_TENANT_ID))) : null);
        country.setUnitMeasurement(String.valueOf(row.get(Constants.UNIT_MEASUREMENT)));
        country.setRegionCode(String.valueOf(row.get(Constants.REGION_CODE)));
        String appTypesString = (String) row.get(Constants.APP_TYPES);
        country.setAppTypes(appTypesString!= null ?  Arrays.asList(appTypesString.split(Constants.COMMA)) : null);
        return country;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean unlockUser(SearchRequestDTO request) {
        if (Objects.isNull(request.getId())) {
            throw new DataNotAcceptableException(2013);
        }
        User user = getUserById(request.getId());
        if (!Boolean.TRUE.equals(user.getIsBlocked())) {
            throw new BadRequestException(2014);
        }
        user.setIsBlocked(Constants.BOOLEAN_FALSE);
        user.setInvalidLoginAttempts(Constants.ZERO);
        user.setForgetPasswordCount(Constants.ZERO);
        user.setInvalidLoginTime(null);
        user.setForgetPasswordTime(null);
        user.setActive(Constants.BOOLEAN_TRUE);
        user.setDeleted(Constants.BOOLEAN_FALSE);
        user.setBlockedDate(null);
        return !Objects.isNull(userRepository.save(user));
    }

    /**
     * {@inheritDoc}
     */
    public void createSuperAdmin(List<UserRequestDTO> usersList) {
        ModelMapper mapper = new ModelMapper();
        validateUsers(usersList);
        List<User> users = new ArrayList<>();
        usersList.stream().forEach(userRequest ->
        {
            Set<Role> roles = roleService.getRolesByIds(userRequest.getRoleIds());
            User user = mapper.map(userRequest, User.class);
            user.getRoles().addAll(roleService.getRolesByIds(userRequest.getRoleIds()));
            user.getSuiteAccess().addAll(roles.stream().map(Role::getSuiteAccessName).collect(Collectors.toSet()));
            users.add(user);
        });
        UserSelectedTenantContextHolder.set(null);
        List<User> userResponse = userRepository.saveAll(users);
        for (User user : userResponse) {
            forgotPassword(user, Boolean.TRUE, UserContextHolder.getUserDto().getClient(), null);
        }
    }

    /**
     * {@inheritDoc}
     */
    public User userProfileUpdate(UserRequestDTO user) {
        if (Objects.isNull(user.getId())) {
            throw new DataNotAcceptableException(2013);
        }
        User existingUser = getUserById(user.getId());
        if (!Objects.equals(UserContextHolder.getUserDto().getId(), user.getId())) {
            throw new SpiceValidation(2015);
        }
        userMapper.setExistingUser(user, existingUser);
        if (Objects.nonNull(existingUser.getFhirId())) {
            user.setFhirId(existingUser.getFhirId());
            fhirServiceApiInterface.updateUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                    CommonUtil.getClient(), user);
        }
        return userRepository.save(existingUser);
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO deleteOrganizationUser(SearchRequestDTO request) {
        if (Objects.isNull(request.getId())) {
            throw new DataNotAcceptableException(2013);
        }
        boolean isCommunityApp = CommonUtil.isCommunityApp(request.getAppTypes());
        String roleName = isCommunityApp ? Constants.PEER_SUPERVISOR : Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT;


        List<UserSupervisor> userSupervisors = new ArrayList<>();
        User existingUser = getUserById(request.getId());
        if (existingUser.getRoles().stream()
                .anyMatch(role -> Objects.equals(roleName, role.getName()))) {
            userSupervisors = userSupervisorRepository.findBySupervisorIdAndIsDeletedAndIsActive(request.getId(), false,
                    true);
            if (!Objects.isNull(request.getTenantIds()) && Constants.ONE == request.getTenantIds().size()) {
                validatePeerSupervisor(userSupervisors, request.getTenantIds(), isCommunityApp);
            }
            userSupervisors.removeIf(userSupervisor -> userSupervisor.getUserId().equals(userSupervisor.getSupervisorId()));
        }
        existingUser.getOrganizations().removeIf(organization -> request.getTenantIds().contains(organization.getId()));
        existingUser.getVillages().removeIf(village -> request.getHealthFacilityLinkedVillages().contains(village.getId()));
        if (existingUser.getOrganizations().isEmpty()) {
            if (!userSupervisors.isEmpty()) {
                throw new SpiceValidation(2022);
            }
            deletePeerSupervisorLink(existingUser, request.getId(), isCommunityApp ? Constants.ROLE_CHW : Constants.ROLE_CHP);
            deleteUserTokenByUserName(existingUser.getUsername(), existingUser.getId());
            existingUser.setDeleted(true);
            existingUser.setActive(false);
            fhirServiceApiInterface.deleteUser(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(),
                    existingUser.getFhirId());
            if (!Objects.isNull(existingUser.getInsightUserOrganization()) && !existingUser.getInsightUserOrganization().isEmpty()) {
                existingUser.getInsightUserOrganization().clear();
            }
            if (!Objects.isNull(existingUser.getReportUserOrganization()) && !existingUser.getReportUserOrganization().isEmpty()) {
                existingUser.getReportUserOrganization().clear();
            }
        } else if (request.getTenantIds().contains(existingUser.getTenantId())) {
            Organization org = existingUser.getOrganizations().stream()
                    .findFirst()
                    .orElse(new Organization());
            if (existingUser.getRoles().stream()
                    .anyMatch(role -> Objects.equals(Constants.ROLE_CHW, role.getName())) && existingUser.getVillages().isEmpty()) {
                throw new SpiceValidation(2027);
            }
            if (org != null) {
                existingUser.setTenantId(org.getId());
            }
        }
        userRepository.save(existingUser);
        return new ModelMapper().map(existingUser, UserResponseDTO.class);
    }

    /**
     * <p>
     * Deletes the peer supervisor link for a given user if they are assigned as CHWs (Community Health Workers).
     * This method checks if the specified user has the role of a CHW. If so, it retrieves all active and non-deleted
     * UserSupervisor entities associated with the user's ID. Each retrieved UserSupervisor entity is then marked as deleted
     * and inactive. This operation is batch-executed to update all affected UserSupervisor entities in the repository.
     * </p>
     *
     * @param existingUser The {@link User} entity whose peer supervisor links are to be deleted.
     * @param id           The unique identifier of the user, used to find associated UserSupervisor entities.
     */
    private void deletePeerSupervisorLink(User existingUser, Long id, String roleName) {
        if (existingUser.getRoles().stream().anyMatch(role -> Objects.equals(roleName, role.getName()))) {
            List<UserSupervisor> userSupervisorsList = userSupervisorRepository.findByUserIdAndIsDeletedAndIsActive(
                    id, false, true);
            if (!Objects.isNull(userSupervisorsList) && !userSupervisorsList.isEmpty()) {
                userSupervisorsList.forEach(userSupervisor -> {
                    userSupervisor.setDeleted(true);
                    userSupervisor.setActive(false);
                });
                userSupervisorRepository.saveAll(userSupervisorsList);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void removeSuperAdmin(Long id) {
        if (Objects.isNull(id)) {
            throw new DataNotAcceptableException(2013);
        }
        User user = userRepository.findById(id).orElseThrow(() -> new DataNotFoundException(2003));
        if (user.isDeleted()) {
            throw new DataConflictException(2016);
        }
        user.setTenantId(Constants.LONG_ZERO);
        user.setDeleted(Constants.BOOLEAN_TRUE);
        userRepository.save(user);
    }

    /**
     * {@inheritDoc}
     */
    public void updateSuperAdmin(UserSuperAdminDto userDto) {
        if (Objects.isNull(userDto.getId())) {
            throw new DataNotAcceptableException(2013);
        }
        User user = userRepository.findById(userDto.getId()).orElseThrow(() -> new DataNotFoundException(2003));
        user.setTenantId(Constants.LONG_ZERO);
        user = userMapper.setSuperAdminUser(userDto, user);
        userRepository.save(user);
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO validateUser(SearchRequestDTO requestData) {
        if (requestData.getEmail().isBlank()) {
            throw new DataNotAcceptableException(5003);
        }
        User user = userRepository.findByUsernameIgnoreCaseAndIsDeletedFalse(requestData.getEmail());
        UserResponseDTO userDTO = null;

        if (Objects.isNull(user)) {
            return userDTO;
        }

        if (CommonUtil.isCommunityApp(requestData.getAppTypes())) {
            if (Objects.nonNull(user.getTenantId()) && (user.getTenantId().equals(requestData.getTenantId()))) {
                throw new DataConflictException(2002);
            }
            userDTO = modelMapper.map(user, UserResponseDTO.class);
            if (user.getRoles().stream().anyMatch(role -> Constants.ROLE_CHW.equals(role.getName()))) {
                UserSupervisor userSupervisor = userSupervisorRepository.findByUserIdAndIsDeletedFalseAndIsActiveTrue(userDTO.getId());
                if (!Objects.isNull(userSupervisor)) {
                    userDTO.setSupervisor(modelMapper.map(getUserById(userSupervisor.getSupervisorId()), UserResponseDTO.class));
                }
            }
            return userDTO;
        }
        if (Objects.isNull(requestData.getIgnoreTenantId())
                && Objects.isNull(requestData.getParentOrganizationId())) {
            throw new DataConflictException(11003);
        }
        if (!Objects.isNull(requestData.getParentOrganizationId()) && (user.getOrganizations().stream()
                .map(Organization::getId).toList().contains(requestData.getIgnoreTenantId()))) {
            throw new DataConflictException(11003);
        }
        if (!Objects.isNull(requestData.getParentOrganizationId())) {
            organizationService.validateParentOrganization(requestData.getParentOrganizationId(), user);
        }
        if (Objects.nonNull(user.getTenantId()) && (user.getTenantId().equals(requestData.getTenantId()))) {
            throw new DataConflictException(2002);
        }
        userDTO = modelMapper.map(user, UserResponseDTO.class);
        if (requestData.getIsSiteUsers() != null && requestData.getIsSiteUsers() &&
                Constants.SPICE_WEB_ROLES.containsValue(userDTO.getDefaultRoleName())) {
            throw new DataConflictException(11003);
        }
        if (Constants.SPICE_CFR_ROLES.containsValue(userDTO.getDefaultRoleName())) {
            throw new BadRequestException(1106);
        }
        if (UserContextHolder.getUserDto().getRoles().stream().anyMatch(role -> role.getName().equals(Constants.ROLE_HEALTH_FACILITY_ADMIN))
                && userDTO.getDefaultRoleName().equals(Constants.ROLE_HEALTH_FACILITY_ADMIN)) {
            throw new InvalidPathException(50007);
        }
        return userDTO;
    }



    /**
     * {@inheritDoc}
     */
    public Map<Long, User> getUsersByIdsAsMap(List<Long> ids) {
        List<User> users = getUsersByIds(ids);
        Map<Long, User> usersMap = new HashMap<>();
        users.forEach(user ->
            usersMap.put(user.getId(), user)
        );
        return usersMap;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<UserSuperAdminDto> getSuperAdminUsers(SearchRequestDTO searchRequest) {
        ResponseListDTO<UserSuperAdminDto> response = new ResponseListDTO<>();
        Pageable pageable = null;
        String searchTerm = null;
        if (!Objects.isNull(searchRequest)) {
            searchTerm = searchRequest.getSearchTerm();
            if (!CommonUtil.isValidSearchData(searchTerm, Constants.USER_SEARCH_TERM)) {
                return response;
            }
            pageable = Pagination.setPagination(searchRequest.getSkip(), searchRequest.getLimit(), Constants.UPDATED_AT,
                    Constants.BOOLEAN_FALSE);
        }
        Page<User> usersList = userRepository.getSuperAdminUsers(Constants.ROLE_SUPER_ADMIN, searchTerm,
                pageable);
        modelMapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        List<UserSuperAdminDto> users = modelMapper.map(usersList.stream().toList(), new TypeToken<List<UserSuperAdminDto>>() {
        }.getType());
        response.setData(users);
        response.setTotalCount(usersList.getTotalElements());
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, List<RoleResponseDTO>> getRoleGroupes(SearchRequestDTO request) {
        return roleService.getRoleGroups(request);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> resetUserPassword(String token, Map<String, String> userInfo) {
        if (Objects.isNull(userInfo.get(FieldConstants.PASSWORD))) {
            throw new DataNotAcceptableException(1003);
        }
        User user = verifyJwtToken(token);
        if (!Objects.isNull(user.getPassword()) && user.getPassword().equals(userInfo.get(FieldConstants.PASSWORD))) {
            Logger.logError(StringUtil.constructString(ErrorConstants.SAME_PASSWORD));
            throw new SpiceValidation(1004);
        }
        setPassword(user, userInfo.get(FieldConstants.PASSWORD));
        userRepository.save(user);
        return Map.of(Constants.IS_PASSWORD_SET, Constants.BOOLEAN_TRUE);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> setUserPassword(String token, Map<String, String> userInfo) {
        if (Objects.isNull(userInfo.get(FieldConstants.PASSWORD))) {
            throw new DataNotAcceptableException(1003);
        }
        User user = verifyJwtToken(token);
        if (Objects.isNull(user.getPassword()) || user.getPassword().isBlank()) {
            setPassword(user, userInfo.get(FieldConstants.PASSWORD));
            userRepository.save(user);
            return Map.of(Constants.IS_PASSWORD_SET, Constants.BOOLEAN_FALSE);
        }
        return Map.of(Constants.IS_PASSWORD_SET, Constants.BOOLEAN_TRUE);
    }

    /**
     * {@inheritDoc}
     */
    public void changeOldToNewPassword(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO)
                || Objects.isNull(requestDTO.getUserId())
                || Objects.isNull(requestDTO.getNewPassword())
                || Objects.isNull(requestDTO.getOldPassword())) {
            throw new DataNotAcceptableException(1107);
        }

        if (!UserContextHolder.getUserDto().getId().equals(requestDTO.getUserId())) {
            throw new Validation(20005);
        }
        User user =
                userRepository.findById(requestDTO.getUserId()).orElseThrow(() -> new DataNotAcceptableException(2003));

        if (!user.getPassword().equals(requestDTO.getOldPassword())) {
            throw new DataConflictException(1015);
        }

        if (requestDTO.getOldPassword().equals(requestDTO.getNewPassword())) {
            Logger.logError(StringUtil.constructString(ErrorConstants.SAME_PASSWORD));
            throw new SpiceValidation(1004);
        }
        setPassword(user, requestDTO.getNewPassword());
        userRepository.save(user);
    }

    /**
     * used to set password to user
     *
     * @param user     user objecct
     * @param password password
     */
    private void setPassword(User user, String password) {
        user.setPassword(password);
        user.setIsBlocked(Boolean.FALSE);
        user.setBlockedDate(null);
        user.setInvalidLoginTime(null);
        user.setForgetPasswordTime(null);
        user.setInvalidLoginAttempts(Constants.ZERO);
        user.setForgetPasswordCount(Constants.ZERO);
        user.setForgetPasswordToken(null);
        user.setForgetPasswordShortToken(null);
    }

    /**
     * {@inheritDoc}
     */
    public List<VillageDTO> getUserVillages(Long userId) {
        User user = getUserById(userId);
        List<VillageDTO> villages = new ArrayList<>();
        if (!Objects.isNull(user.getVillages()) && !user.getVillages().isEmpty()) {
            villages = modelMapper.map(user.getVillages(), new TypeToken<List<VillageDTO>>() {
            }.getType());
        }
        return villages;
    }

    /**
     * {@inheritDoc}
     */
    public void phoneNumberValidation(SearchRequestDTO request) {
        User user = userRepository.findByPhoneNumberAndCountryCodeAndIsDeletedFalse(request.getPhoneNumber(), request.getCountryCode());
        if (request.getPhoneNumber().matches(Constants.NUMBER_ZERO_REGEX) || (!request.getPhoneNumber().matches(Constants.NUMBER_REGEX))) {
            throw new BadRequestException(2019);
        }
        if (!Objects.isNull(user) && !user.getId().equals(request.getId())) {
            throw new DataConflictException(2018);
        }
    }

    /**
     * {@inheritDoc}
     */
    public List<UserResponseDTO> getPeerSupervisor(SearchRequestDTO request) {
        List<Long> childTenantIds = new ArrayList<>();
        Map<Long, List<Long>> tenants = organizationUtil.getParentChildTenantMap();
        request.getTenantIds().forEach(tenantId -> {
            childTenantIds.addAll(tenants.get(tenantId));
            request.setTenantIds(childTenantIds);
        });
        return getPeerSupervisorByTenants(request);
    }

    /**
     * {@inheritDoc}
     */
    public List<UserResponseDTO> getPeerSupervisorByTenants(SearchRequestDTO request) {
        List<User> peerSupervisors = new ArrayList<>();
        Set<Long> tenantIds = new HashSet<>(request.getTenantIds());
        if (CommonUtil.isCommunityApp(request.getAppTypes())) {
            peerSupervisors.addAll(userRepository.getUsersByRoleName(Constants.PEER_SUPERVISOR, tenantIds));
        } else {
            peerSupervisors.addAll(userRepository.getUsersByRoleName(Constants.ROLE_COMMUNITY_HEALTH_ASSISTANT, tenantIds));
        }
        return modelMapper.map(peerSupervisors, new TypeToken<List<UserResponseDTO>>() {
        }.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<UserResponseDTO> getAllMobileUsers() {
        return mapUsersToUserResponseDTO(userRepository.findAllBySuiteAccess(UserContextHolder.getUserDto().getClient()));
    }

    /**
     * {@inheritDoc}
     */
    public List<UserResponseDTO> getAllUsersByTenantId(SearchRequestDTO request) {
        if (Objects.isNull(request.getTenantId())) {
            throw new DataNotFoundException(1108);
        }
        return mapUsersToUserResponseDTO(userRepository.findAllByTenantId(List.of(request.getTenantId())));
    }

    /**
     * <p>
     * Converts a list of {@link User} entities into a list of {@link UserResponseDTO} objects.
     * This method iterates over each {@link User} in the provided list, creating a new {@link UserResponseDTO}
     * for each, and populates it with the user's details such as ID, username, phone number, first name, last name,
     * FHIR ID, roles, and organizations. The resulting list of {@link UserResponseDTO} objects is then returned.
     * </p>
     *
     * @param users A list of {@link User} entities that need to be converted into {@link UserResponseDTO} objects.
     * @return A list of {@link UserResponseDTO} objects corresponding to the provided list of {@link User} entities.
     */
    private List<UserResponseDTO> mapUsersToUserResponseDTO(List<User> users) {
        List<UserResponseDTO> userResponseDTOs = new ArrayList<>();
        for (User user : users) {
            UserResponseDTO userResponseDTO = new UserResponseDTO();
            userResponseDTO.setId(user.getId());
            userResponseDTO.setUsername(user.getUsername());
            userResponseDTO.setPhoneNumber(user.getPhoneNumber());
            userResponseDTO.setSuiteAccess(user.getSuiteAccess());
            userResponseDTO.setFirstName(user.getFirstName());
            userResponseDTO.setLastName(user.getLastName());
            userResponseDTO.setFhirId(user.getFhirId());
            userResponseDTO.setRoles(user.getRoles());
            userResponseDTO.setOrganizations(user.getOrganizations());
            userResponseDTO.setGender(user.getGender());
            userResponseDTO.setCountryCode(user.getCountryCode());
            userResponseDTO.setTimezone(user.getTimezone());
            userResponseDTO.setDesignation(user.getDesignation());
            userResponseDTO.setReportUserOrganization(user.getReportUserOrganization());
            userResponseDTOs.add(userResponseDTO);
        }
        return userResponseDTOs;
    }

    /**
     * {@inheritDoc}
     */
    public void deleteOrganizationUsers(SearchRequestDTO request) {
        List<User> users = userRepository.findByOrganizations_IdAndIsDeletedAndIsActive(request.getTenantId(), false, true);
        List<String> fhirUsers = new ArrayList<>();
        List<UserSupervisor> userSupervisorList = new ArrayList<>();
        if (!Objects.isNull(users) && !users.isEmpty()) {
            users.forEach(user -> {
                user.getOrganizations().removeIf(organization -> organization.getId().equals(request.getTenantId()));
                if (user.getOrganizations().isEmpty()) {
                    user.setActive(false);
                    user.setDeleted(true);
                    getPeerLinkedSupervisor(user, user.getId(), userSupervisorList);
                    fhirUsers.add(user.getFhirId());
                } else if (user.getTenantId().equals(request.getTenantId())) {
                    Organization org = user.getOrganizations().stream().findFirst().get();
                    if (org != null) {
                        user.setTenantId(org.getId());
                    }
                }
            });
            userRepository.saveAll(users);
            Map<Long, String> usernameIdMap = new HashMap<>();
            users.forEach(user ->
                usernameIdMap.put(user.getId(), user.getUsername())
            );
            deleteUsersToken(usernameIdMap);
            fhirServiceApiInterface.deleteUsers(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), fhirUsers);
            userSupervisorRepository.saveAll(userSupervisorList);
        }
    }

    /**
     * <p>
     * Retrieves and deactivates peer supervisor links for a specified user if they are assigned as CHWs (Community Health Workers).
     * This method checks if the given user has the role of a CHW. If so, it fetches all active and non-deleted
     * UserSupervisor entities associated with the user's ID. Each found UserSupervisor entity is then marked as deleted
     * and inactive. These changes are intended to be saved in the database in a batch operation outside this method.
     * </p>
     *
     * @param existingUser    The {@link User} entity whose peer supervisor links are being processed.
     * @param id              The unique identifier of the user, used to find associated UserSupervisor entities.
     * @param userSupervisors A list of {@link UserSupervisor} entities to be updated with the deactivation changes.
     */
    private void getPeerLinkedSupervisor(User existingUser, Long id,
                                         List<UserSupervisor> userSupervisors) {
        if (existingUser.getRoles().stream().anyMatch(role -> Objects.equals(Constants.ROLE_CHP, role.getName()))) {
            List<UserSupervisor> userSupervisorsList = userSupervisorRepository.findByUserIdAndIsDeletedAndIsActive(
                    id, false, true);
            if (!Objects.isNull(userSupervisorsList) && !userSupervisorsList.isEmpty()) {
                userSupervisorsList.forEach(userSupervisor -> {
                    userSupervisor.setDeleted(true);
                    userSupervisor.setActive(false);
                });
                userSupervisors.addAll(userSupervisorsList);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO getUserProfile() {
        ModelMapper mapper = new ModelMapper();
        User user = getUserById(UserContextHolder.getUserDto().getId());
        UserResponseDTO userResponse = mapper.map(user, UserResponseDTO.class);
        List<Role> userRoles = user.getRoles().stream().sorted(Comparator.comparing(Role::getLevel, Comparator.nullsFirst(Long::compareTo))).toList();
        LinkedHashSet<Role> roles = new LinkedHashSet<>();
        String client = CommonUtil.getClient();
        List<String> clients = Constants.CLIENT_WEB.equals(client) ? Constants.WEB_CLIENTS : Constants.MOB_CLIENTS;
        for (Role userRole : userRoles) {
            for (String userClient : clients) {
                if (Objects.equals(userClient, userRole.getSuiteAccessName())) {
                    roles.add(userRole);
                }
            }
        }
        userResponse.setRoles(roles);
        return userResponse;
    }

    /**
     * {@inheritDoc}
     */
    public void validatePeerSupervisors(SearchRequestDTO request) {
        if (Objects.nonNull(request.getIds()) && !request.getIds().isEmpty()) {
            List<UserSupervisor> userSupervisors = userSupervisorRepository
                    .findBySupervisorIdInAndIsDeletedAndIsActive(request.getIds(),
                            false, true);
            validatePeerSupervisor(userSupervisors, List.of(request.getTenantId()), CommonUtil.isCommunityApp(request.getAppTypes()));
        }
    }

    /**
     * {@inheritDoc}
     */
    public List<UserVillageResponseDTO> getUserVillagesOfPeerSupervisor() {
        List<User> users = userRepository.getUsersMappedToPeerSupervisor(UserContextHolder.getUserDto().getId());
        if (Objects.isNull(users)) {
            return Collections.emptyList();
        }
        List<UserResponseDTO> userResponseDTOS = users.stream()
                .map(user -> modelMapper.map(user, UserResponseDTO.class)).toList();
        return userResponseDTOS.stream().map(user -> {
            UserVillageResponseDTO userVillageResponseDTO = new UserVillageResponseDTO(user.getId(), user.getUsername(),
                    user.getFirstName(), user.getLastName(), user.getFhirId());
            user.getVillages().forEach(village ->
                userVillageResponseDTO.getVillages()
                        .add(new VillageDTO(village.getId(), village.getName(), user.getId()))
            );
            return userVillageResponseDTO;
        }).toList();
    }

    /**
     * {@inheritDoc}
     */
    public List<UserVillageDTO> getUserVillagesOfPeerSupervisorWithPagination(SearchRequestDTO request) {
        Pageable pageable = Pagination.setPagination(request.getSkip(), request.getLimit(), Constants.ID,
                Constants.BOOLEAN_FALSE);
        Page<Map<String, Object>> users = userRepository.getUsersMappedToPeerSupervisorWithPagination(
                request.getUserId(), request.getUserIds(), request.getVillageIds(), pageable);
        return !Objects.isNull(users) ? users.stream().map(user -> modelMapper.map(user, UserVillageDTO.class)).toList() : null;
    }

    /**
     * {@inheritDoc}
     */
    public UserPreferencesDTO saveUserPreferences(UserPreferencesDTO request) {
        UserPreferences userPreferences = null;
        userPreferences = userPreferencesRepository.getPreferencesByType(request.getUserId(),
                Constants.PERFORMANCE_MONITORING);
        if (Objects.isNull(userPreferences)) {
            userPreferences = new UserPreferences();
            userPreferences.setUserId(request.getUserId());
            userPreferences.setType(Constants.PERFORMANCE_MONITORING);
        }
        userPreferences.setActive(Boolean.TRUE);
        userPreferences.setPreference(request.getPreference());
        return modelMapper.map(userPreferencesRepository.save(userPreferences), UserPreferencesDTO.class);
    }

    /**
     * get Short Url from server
     *
     * @param shortToken token
     * @return short url
     */
    private String getShortenUrl(String shortToken) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setUrl(appUrl.concat(shortToken));
        requestDTO.setApp(shortenApp);
        requestDTO.setEnv(environment);
        ResponseEntity<Map<String, Object>> shortUrlResponse = shortUrlInterface.shortenURL(requestDTO);
        if (Objects.isNull(shortUrlResponse.getBody())) {
            throw new SpiceValidation(2026);
        }
        return shortUrlResponse.getBody().get(Constants.SHORT_URL).toString();
    }

    /**
     * {@inheritDoc}
     */
    private void sendForgetPasswordSms(User user) {
        SMSTemplate smsTemplate = notificationInterface.getSmsTemplateValues(Constants.FORGOT_PASSWORD_USER).getBody();
        assert smsTemplate != null;
        String body = smsTemplate.getBody();
        Map<String, String> data = new HashMap<>();
        data.put(Constants.URL, user.getShortenUrl());
        data.put(Constants.NAME, StringUtil.concatString(user.getFirstName(), Constants.SPACE, user.getLastName()));
        body = StringUtil.parseEmailTemplate(body, data);
        SmsDTO smsDto = new SmsDTO(body, user.getCountryCode() + user.getPhoneNumber(), user.getTenantId(), String.valueOf(user.getId()), user.getUsername());
        notificationInterface.saveOutBoundSMS(List.of(smsDto));
    }

    /**
     * {@inheritDoc}
     */
    public String getPasswordResetUrl(String url) {
        String originalUrl = redisTemplate.opsForValue().get(url);
        if (originalUrl == null) {
            throw new DataNotFoundException(2025);
        }
        return originalUrl;
    }

    /**
     * {@inheritDoc}
     */
    public UserPreferencesDTO getUserPreferencesById(Long userId) {
        UserPreferences userPreferences = userPreferencesRepository.getPreferencesByType(userId,
                Constants.PERFORMANCE_MONITORING);
        return !Objects.isNull(userPreferences) ? modelMapper.map(
                userPreferencesRepository.getPreferencesByType(userId, Constants.PERFORMANCE_MONITORING),
                UserPreferencesDTO.class) : null;
    }

    /**
     * <p>
     * This method is used to create a secure random token using alphabets and numbers.
     * </p>
     *
     * @return a {@link String} Short token with mentioned length
     */
    private String getShortToken() {
        SecureRandom secureRandom = new SecureRandom();
        StringBuilder stringBuilder = new StringBuilder(URL_LENGTH);
        boolean isOldToken = true;
        while (isOldToken) {
            for (int i = Constants.ZERO; i < URL_LENGTH; i++)
                stringBuilder.append(Constants.LETTER_TEXT.charAt(secureRandom.nextInt(Constants.LETTER_TEXT.length())));
            isOldToken = userRepository.existsByForgetPasswordShortTokenAndIsActiveAndIsDeleted(stringBuilder.toString(),
                    true, false);
        }
        return stringBuilder.toString();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean activateDeactivateUser(List<Long> tenantIds, boolean isActive) {
        List<User> users = userRepository.findUsersByTenantIds(tenantIds, !isActive);
        List<String> fhirUsers = new ArrayList<>();
        if (!users.isEmpty()) {
            users.forEach(user -> {
                user.setActive(isActive);
                fhirUsers.add(user.getFhirId());
            });
            userRepository.saveAll(users);
            fhirServiceApiInterface.activateDeactivateUsers(CommonUtil.getAuthToken(),
                    CommonUtil.getClient(), fhirUsers, isActive);
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> getLockedUsers(SearchRequestDTO requestObject) {
        String searchTerm = requestObject.getSearchTerm();
        List<UserDTO> lockedUsersDtos = new ArrayList<>();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.USER_SEARCH_TERM)) {
            return Map.of(Constants.COUNT, Constants.ZERO, Constants.DATA, lockedUsersDtos);
        }
        Pageable pageable = Pagination.setPagination(requestObject.getSkip(), requestObject.getLimit(),
                Constants.UPDATED_AT, Constants.BOOLEAN_FALSE);
        Page<User> lockedUsers = null;
        long totalCount = 0;
        if (Objects.nonNull(requestObject.getRoleType()) && requestObject.getRoleType().equals(Constants.ROLE_HEALTH_FACILITY_ADMIN)) {
            Long userId = UserContextHolder.getUserDto().getId();
            modelMapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
            User user = this.getUserById(userId);
            List<Long> ids = user.getOrganizations().stream().map(Organization::getId).toList();
            lockedUsers = userRepository.getLockedUsers(searchTerm, ids, pageable);
        } else if (null == requestObject.getTenantId()) {
            lockedUsers = userRepository.getBlockedUsers(searchTerm, pageable);
        } else {
            Organization organization = organizationService.getOrganizationById(requestObject.getTenantId());
            if (!Objects.isNull(organization)) {
                List<Long> tenantIdList;
                Map<String, List<Long>> childIds = organizationService
                        .getChildOrganizations(requestObject.getTenantId(), organization.getFormName());
                tenantIdList = childIds.values().stream().flatMap(List::stream).collect(Collectors.toList());
                tenantIdList.add(requestObject.getTenantId());
                lockedUsers = userRepository.getLockedUsers(searchTerm, tenantIdList, pageable);
            }
        }
        if (!Objects.isNull(lockedUsers)) {
            lockedUsersDtos = modelMapper.map(lockedUsers.stream().toList(), new TypeToken<List<UserOrganizationDTO>>() {
            }.getType());
            totalCount = lockedUsers.getTotalElements();
        }
        return Map.of(Constants.COUNT, totalCount, Constants.DATA, lockedUsersDtos);
    }

    /**
     * {@inheritDoc}
     */
    public List<User> getUserByRoleName(String roleName) {
        return userRepository.getUsersByRole(roleName);
    }

    /**
     * <p>
     * Deletes UserTokens by username and user ID.
     * This method takes a map where the key is the user ID and the value is the username, and deletes the corresponding UserTokens.
     * </p>
     *
     * @param usernameIdMap A map where the key is the user ID and the value is the username.
     */
    private void deleteUsersToken(Map<Long, String> usernameIdMap) {
        try {
            List<UserToken> userTokens = userTokenRepository.findByUserIdInAndIsActiveTrue(usernameIdMap.keySet());
            List<String> tokens = userTokens.stream().map(authToken -> new StringBuilder().append(Constants.SPICE).append(Constants.COLON).append(Constants.LOGIN)
                    .append(Constants.COLON).append(usernameIdMap.get(authToken.getUserId())).append(Constants.COLON)
                    .append(authToken.getAuthToken()).toString()).toList();

            redisTemplate.delete(tokens);
            userTokens.stream().forEach(token -> {
                token.setActive(Constants.BOOLEAN_FALSE);
                token.setAuthToken(null);
            });
            userTokenRepository.saveAll(userTokens);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
    }

    /**
     * <p>
     * Deletes a UserToken by username and user ID.
     * This method takes a username and a user ID as parameters and deletes the corresponding UserToken.
     * </p>
     *
     * @param username The username of the user.
     * @param userId   The ID of the user.
     */
    public void deleteUserTokenByUserName(String username, Long userId) {
        try {
            List<UserToken> userTokens = userTokenRepository.findByUserIdAndIsActiveTrue(userId);
            List<String> tokens = userTokens.stream().map(authToken -> new StringBuilder().append(Constants.SPICE).append(Constants.COLON).append(Constants.LOGIN)
                    .append(Constants.COLON).append(username).append(Constants.COLON)
                    .append(authToken.getAuthToken()).toString()).toList();
            redisTemplate.delete(tokens);
            userTokens.stream().forEach(token -> {
                token.setActive(Constants.BOOLEAN_FALSE);
                token.setAuthToken(null);
            });
            userTokenRepository.saveAll(userTokens);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void updateUserTermsAndConditionDetailsById() {
        User user = userRepository.findByIdAndIsDeletedFalseAndIsActiveTrue(UserContextHolder.getUserDto().getId());

        if (Objects.isNull(user)) {
            throw new DataNotFoundException(2003);
        }

        if (!Boolean.FALSE.equals(user.getIsTermsAndConditionsAccepted())) {
            throw new DataNotAcceptableException(3001);
        }
        user.setIsTermsAndConditionsAccepted(Boolean.TRUE);
        userRepository.save(user);
    }

    /**
     * {@inheritDoc}
     */
    public List<User> getUserListByRole(CommonRequestDTO requestDto) {
        List<String> roleNames = Objects.isNull(requestDto.getRoleNames())
                ? new ArrayList<>(Arrays.asList(Constants.ROLE_PROVIDER, Constants.ROLE_PHYSICIAN_PRESCRIBER))
                : requestDto.getRoleNames();
        Set<Role> siteUserRoles = roleService
                .getRolesByName(roleNames);
        List<Long> roleIds = new ArrayList<>();
        for (Role role : siteUserRoles) {
            roleIds.add(role.getId());
        }
        return userRepository.findUsersByRoleIdS(roleIds, requestDto.getTenantId(), requestDto.getSearchTerm());
    }

    /**
     * {@inheritDoc}
     */
    public void updateCulture(UserDTO requestDTO) {
        User user = userRepository.findById(requestDTO.getId()).orElseThrow(() -> new DataNotFoundException(1010));
        user.setCulture(modelMapper.map(requestDTO.getCulture(), Culture.class));
        userRepository.save(user);
    }
}
