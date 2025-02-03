package com.mdtlabs.coreplatform.dwarfurl.common;

/**
 * Constants class to hold application-wide constant values.
 */
public class Constants {

    public static final int ZERO = 0;
    public static final int ONE = 1;
    public static final int NINE = 9;
    public static final int TEN = 10;
    public static final int SIXTY_TWO = 62;
    public static final int THIRTY_FIVE = 35;
    public static final int FORTY_EIGHT = 48;
    public static final int FIFTY_FIVE = 55;
    public static final int SIXTY_ONE = 61;
    public static final Long DAY_TO_MINUTES = 1440L;
    public static final String URL_REGEX = "^(?:(?:(?:https?|ftp):)?\\/\\/)" +
            "(?:\\S+(?::\\S*)?@)?" +
            "(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})" +
            "(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))" +
            "|(?:(?:[a-z0-9\\u00a1-\\uffff][a-z0-9\\u00a1-\\uffff_-]{0,62})?[a-z0-9\\u00a1-\\uffff]\\.)+(?:[a-z\\u00a1-\\uffff]{2,}\\.?)" +
            ")(?::\\d{2,5})?(?:[/?#]\\S*)?$";
    public static final String BEARER = "Bearer";
    public static final String BEARER_SPACE = "Bearer ";
    public static final String EMPTY_STRING = "";
    public static final String INVALID_TOKEN = "Invalid token";
    public static final String INVALID_URL = "Invalid url";
    public static final String URL_NOT_FOUND = "URL not found";
    public static final String URL_EXPIRED = "URL expired";
    public static final String LOGGER = "Logger";
    public static final String URL_CREATE_ERROR_MESSAGE = "Error occurred while creating shorten URL";
    public static final String URL_VALIDATE_ERROR_MESSAGE = "Error occurred while validating shorten URL";
}
