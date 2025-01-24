package com.mdtlabs.coreplatform.commonservice.common;

/**
 * <p>
 * To define the common static error parameter used all over the application.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public class ErrorConstants {

    private ErrorConstants() {
    }

    public static final String TENANT_ID_NOT_NULL = "Tenant Id should not be null";
    public static final String SAME_PASSWORD = "New password cannot be same as old password";
    public static final String ERROR_USERNAME_PASSWORD_BLANK = "No Username and / or Password Provided";
    public static final String ERROR_INVALID_USER = "Invalid credentials";
    public static final String ERROR_ACCOUNT_DISABLED = "Disabled Account";
    public static final String ERROR_INVALID_ATTEMPTS = "Account locked due to multiple invalid login attempts.";
    public static final String INFO_USER_NOT_EXIST = "Username does not exist : ";
    public static final String INFO_USER_PASSWORD_NOT_MATCH = "Password doesn't match for the user : ";
    public static final String EXCEPTION_TOKEN_UTILS = "Exception occurred while loading token utils";
    public static final String INVALID_USER_ERROR = "{ \"error\": \"Invalid User\"}";
    public static final String LOGIN_ERROR = "Login Error ";
    public static final String ERROR_JWE_TOKEN = "Error while creating jwe token ";
    public static final String TOKEN_EXPIRED = "Token expired";
    public static final String EXCEPTION_DURING_TOKEN_UTIL = "Exception occurred while loading token utils";
    public static final String RESOLVER_ERROR = "Error message construction using resolver Error Message";
    public static final String LINK_EXPIRED = "Link has expired.";
    public static final String ERROR_USER_DOESNT_ROLE = "You don't have a permission. Please contact administrator";
    public static final String PASSWORD_NOT_EXIST = "Password not set for the user";
    public static final String INVALID_CLIENT = "Invalid client";
    public static final String ERROR_UPDATE_PASWORD = "Error while update user password : ";
    public static final String LABTEST_NAME_NOT_EMPTY = "Labtest name should not be empty.";
    public static final String INVALID_CLINT = "You don't have permission to perform this operation";
    public static final String TOKEN_EMPTY = "Token not provided";


    public static final String PHONE_NUMBER_INVALID = "Phone number is Invalid.";
    public static final String EMAIL_INVALID = "Invalid Email ID";
    public static final String PARENT_ORG_ID_NOT_NULL = "Parent Organization Id should not be null";
    public static final String SITE_NAME_NOT_NULL = "Site should not be empty";
    public static final String ADDRESS_TYPE_NOT_NULL = "Address type should not be empty";
    public static final String ADDRESS_USE_NOT_NULL = "Address use should not be empty";
    public static final String ADDRESS_NOT_NULL = "Address should not be empty";
    public static final String SITE_NOT_NULL = "Site should not be empty";
    public static final String POSTAL_CODE_NOT_NULL = "Postal code should not be empty";
    public static final String SITE_TYPE_NOT_NULL = "Site type should not be empty";
    public static final String ACCOUNT_ID_NOT_NULL = "District ID should not be empty";
    public static final String OPERATING_UNIT_NOT_NULL = "Operating unit should not be empty";
    public static final String OPERATING_UNIT_ID_NOT_NULL = "Operating unit ID should not be empty";
    public static final String CULTURE_NOT_NULL = "Culture should not be empty";
    public static final String USER_MIN_SIZE = "Site should have atleast one user";
    public static final String DISTRICT_USER_MIN_SIZE = "District should have atleast one user";
    public static final String OU_USER_MIN_SIZE = "Operating Unit should have atleast one user";
    public static final String REGION_USER_MIN_SIZE = "Region should have atleast one user";
    public static final String RANGE_MIN_VALUE_NOT_NULL = "Labtest result range minimum value should not be empty";
    public static final String RANGE_MAX_VALUE_NOT_NULL = "Labtest result range maximum value should not be empty";
    public static final String RANGE_UNIT_NOT_NULL = "Labtest result range unit should not be empty";
    public static final String RANGE_UNIT_ID_NOT_NULL = "Labtest result range unit id should not empty";
    public static final String RANGE_DISPLAY_ORDER_NOT_NULL = "Labtest result range display order should not be empty";
    public static final String RANGE_DISPLAY_NAME_NOT_EMPTY = "Labtest result range display name should not be empty";
    public static final String LABTEST_RESULT_RANGE_ID_NOT_NULL = "Labtest result range id should not be null";
    public static final String TIMEZONE_NOT_NULL = "Timezone should not be empty";
    public static final String REQUEST_NOT_EMPTY = "Request should not be empty.";
    public static final String LABTEST_NOT_FOUND = "No LabTest found for this ID - ";
    public static final String INVALID_REQUEST = "Invalid request.Unable to process.";

    //File
    public static final String INVALID_DATA = "The data is invalid";
    public static final String INVALID_FILE = "The provided file is invalid";
    
    public static final String PASSWORD_RESET_ERROR_MESSAGE = "Password reset attempt exceeded. Please try after sometime";
    public static final String INVALID_OFFLINE_SYNC_MESSAGE = "Error while construct the offline message : ";
    public static final String EXECUTION_EXCEPTION_MESSAGE = "Error while fetching concurrent data : ";
    public static final String COUNTY_NAME_NOT_NULL = "County name should not be empty";
    public static final String DISTRICT_NAME_NOT_NULL = "District name should not be empty";
    public static final String COUNTRY_ID_NOT_NULL = "Country id must not be null";
    public static final String CHIEFDOM_NOT_FOUND = "No chiefdom found for this ID - ";
    public static final String FORM_INPUT_NOT_NULL = "Form input should not be empty ";
    public static final String PATIENT_VALIDATION_ALERT_MESSAGE = "Entered National ID already exists in %s. Are you sure you want to override?";
    public static final String PATIENT_ALREADY_EXIST = "Patient already exist. ";
}
