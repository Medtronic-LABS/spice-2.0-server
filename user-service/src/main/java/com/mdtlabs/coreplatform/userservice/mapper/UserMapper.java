package com.mdtlabs.coreplatform.userservice.mapper;

import java.util.Map;
import java.util.Objects;

import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.EmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserSuperAdminDto;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplate;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.User;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

/**
 * <p>
 * The UserMapper class contains methods for updating and setting properties of User objects, as well as setting email
 * templates for user creation and forgot password emails.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 09, 2023
 */
@Component
public class UserMapper {

    /**
     * <p>
     * Updates an existing user with new user details and adds an organization to the user's organization list.
     * This method takes a UserRequestDTO, an existing User object, and an Organization as parameters.
     * It first updates the existing user with the new user details using the setExistingUser method.
     * Then, it adds the provided organization to the user's organization list.
     * </p>
     *
     * @param user         The UserRequestDTO containing the new user details.
     * @param existingUser The existing User object to be updated.
     * @param organization The Organization to be added to the user's organization list.
     */
    public void setExistingUser(UserRequestDTO user, User existingUser, Organization organization) {
        setExistingUser(user, existingUser);
        existingUser.getOrganizations().add(organization);
    }

    /**
     * <p>
     * Updates an existing user with new user details.
     * This method takes a UserRequestDTO and an existing User object as parameters.
     * It updates the existing user's gender, first name, last name, country code, phone number, and villages (if provided) with the new user details.
     * </p>
     *
     * @param user         The UserRequestDTO containing the new user details.
     * @param existingUser The existing User object to be updated.
     */
    public void setExistingUser(UserRequestDTO user, User existingUser) {
        existingUser.setTimezone(user.getTimezone());
        existingUser.setGender(user.getGender());
        existingUser.setFirstName(user.getFirstName());
        existingUser.setLastName(user.getLastName());
        existingUser.setCountryCode(user.getCountryCode());
        existingUser.setPhoneNumber(user.getPhoneNumber());
        existingUser.setDesignation(user.getDesignation());
        if(Objects.isNull(existingUser.getTenantId())) {
            existingUser.setTenantId(user.getTenantId());
        }
        if (Objects.nonNull(user.getCulture())) {
            existingUser.setCulture(user.getCulture());
        }
        if (!Objects.isNull(user.getVillages())) {
            existingUser.setVillages(user.getVillages());
        }
    }

    /**
     * <p>
     * Sets up an email template for user creation.
     * This method takes a User object, an EmailTemplate, an EmailDTO, and a Map of data as parameters.
     * It sets the body, email template, subject, recipient, form data ID, and form name of the EmailDTO.
     * The body of the email is parsed from the email template using the provided data.
     * The recipient of the email is set to the username of the User object.
     * The form data ID is set to the ID of the User object.
     * The form name is set to a constant representing the user form.
     * </p>
     *
     * @param user          The User object for whom the email is being created.
     * @param emailTemplate The EmailTemplate to be used for the email.
     * @param emailDto      The EmailDTO to be set up.
     * @param data          The Map of data to be used for parsing the email template.
     * @return The set up EmailDTO.
     */
    public EmailDTO setUserCreationEmailTemplate(User user, EmailTemplate emailTemplate, EmailDTO emailDto,
                                                 Map<String, String> data) {
        emailDto.setBody(StringUtil.parseEmailTemplate(emailTemplate.getBody(), data));
        emailDto.setEmailTemplate(emailTemplate);
        emailDto.setSubject(emailTemplate.getSubject());
        emailDto.setTo(user.getUsername());
        emailDto.setFormDataId(String.valueOf(user.getId()));
        emailDto.setFormName(Constants.EMAIL_FORM_USER);
        return emailDto;
    }

    /**
     * <p>
     * This method is used to set the email template for a forgot password email and populates it with data
     * before returning the EmailDTO.
     * </p>
     *
     * @param emailTemplate {@link EmailTemplate} The email template to be used for the
     *                      forgot password email is given
     * @param mailUser      {@link String} The name of the user who is sending the email is given
     * @param user          {@link User} The user for whom the forgot password email is being sent is given
     * @param emailDto      {@link EmailDTO} The EmailDTO that represents the email message being
     *                      constructed and sent is given
     * @param data          {@link Map} A map containing key-value pairs of data that will be used to replace
     *                      placeholders in the email template is given
     * @return {@link EmailDTO} The EmailDTO is returned after setting the values from given details
     */
    public EmailDTO setForgotPasswordEmailTemplate(EmailTemplate emailTemplate, String mailUser, User user,
                                                   EmailDTO emailDto, Map<String, String> data) {
        emailDto.setBody(StringUtil.parseEmailTemplate(emailTemplate.getBody(), data));
        emailDto.setFromName(mailUser);
        emailDto.setSubject(emailTemplate.getSubject());
        emailDto.setTo(user.getUsername());
        emailDto.setFrom(mailUser);
        emailDto.setFormDataId(String.valueOf(user.getId()));
        emailDto.setFormName(Constants.EMAIL_FORM_USER);
        return emailDto;
    }

    /**
     * <p>
     * This method is used to set the properties of a User based on the values provided in a
     * UserSuperAdminDto.
     * </p>
     *
     * @param userDto {@link UserSuperAdminDto} It contains the updated information for the user
     * @param user    {@link User} The user that needs to be updated is given
     * @return {@link User} The User after setting the properties of UserSuperAdminDto for the
     * given conditions is returned
     */
    public User setSuperAdminUser(UserSuperAdminDto userDto, User user) {
        if (!Objects.isNull(userDto.getGender())) {
            user.setGender(userDto.getGender());
        }
        if (!Objects.isNull(userDto.getFirstName())) {
            user.setFirstName(userDto.getFirstName());
        }
        if (!Objects.isNull(userDto.getLastName())) {
            user.setLastName(userDto.getLastName());
        }
        if (!Objects.isNull(userDto.getPhoneNumber())) {
            user.setPhoneNumber(userDto.getPhoneNumber());
        }
        if (!Objects.isNull(userDto.getCountryCode())) {
            user.setCountryCode(userDto.getCountryCode());
        }
        if (!Objects.isNull(userDto.getTimezone())) {
            user.setTimezone(new Timezone(userDto.getTimezone().getId()));
        }
        return user;
    }

}
