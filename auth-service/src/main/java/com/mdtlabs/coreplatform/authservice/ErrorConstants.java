package com.mdtlabs.coreplatform.authservice;

/**
 * <p>
 * To define the common static error parameter used all over the application.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
public class ErrorConstants {

    private ErrorConstants() {}

    public static final String ERROR_INVALID_USER = "Invalid credentials";
    public static final String ERROR_ACCOUNT_DISABLED = "Disabled Account";
    public static final String ERROR_INVALID_ATTEMPTS = "Account locked due to multiple invalid login attempts.";
    public static final String INFO_USER_NOT_EXIST = "Username does not exist : ";
    public static final String INFO_USER_PASSWORD_NOT_MATCH = "Password doesn't match for the user : ";
    public static final String EXCEPTION_TOKEN_UTILS = "Exception occurred while loading token utils";
    public static final String INVALID_USER_ERROR = "{ \"error\": \"Invalid User\"}";
    public static final String ERROR_USERNAME_PASSWORD_BLANK = "No Username and / or Password Provided";
    public static final String PASSWORD_NOT_EXIST = "Password not set for the user";
    public static final String INVALID_CLIENT = "You don't have permission to perform this operation";
    public static final String LOGIN_ERROR = "Login Error ";
    public static final String ERROR_USER_DOESNT_ROLE = "You don't have a permission. Please contact administrator";
    public static final String ERROR_JWE_TOKEN = "Error while creating jwe token ";
    public static final String TOKEN_EXPIRED = "Token expired";
    public static final String EXCEPTION_DURING_TOKEN_UTIL = "Exception occurred while loading token utils";

}
