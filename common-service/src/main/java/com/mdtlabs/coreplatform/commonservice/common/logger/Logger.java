package com.mdtlabs.coreplatform.commonservice.common.logger;

import java.text.MessageFormat;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;


/**
 * <p>
 * Logger level with custom message.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 *
 */
public class Logger extends BaseLogger {

    private static final String LOGGER_01 = "{0} - {1}";
    private static final String LOGGER_012 = "{0} - {1} - {2}";
    private static final String LOGGER_ERROR_01 = "{0} - Error Code : {1}";

    /**
     * <p>
     * This method is used to get the prefix part of log.
     * </p>
     *
     * @return String - prefix log
     */
    private static String getPrefix() {
         return MessageFormat.format(LOGGER_01, CommonUtil.getLoggedInEmployeeLog());
    }

    /**
     * <p>
     * This method is used to log debug part.
     * </p>
     *
     * @param message - content of log
     * @param aClass  - entity class
     */
    public static void logDebug(String message, Class<?> aClass) {
        if (logger.isDebugEnabled()) {
            logger.debug(MessageFormat.format(LOGGER_012, getPrefix(), message, aClass.getSimpleName()));
        }
    }

    /**
     * <p>
     * This method is used to log debug part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logDebug(String message) {
        if (logger.isDebugEnabled()) {
            logger.debug(MessageFormat.format(LOGGER_01, getPrefix(), message));
        }
    }

    /**
     * <p>
     * This method is used to log error part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logError(String message) {
        if (logger.isErrorEnabled()) {
             logger.error(MessageFormat.format(LOGGER_012, getPrefix(), CommonUtil.getLoggedInEmployeeLog(), message));
        }
    }

    /**
     * <p>
     * This method is used to log error part with code.
     * </p>
     *
     * @param statusCode - code of error on status
     * @param message    - content of log
     */
    public static void logError(String statusCode, String message) {
        if (logger.isErrorEnabled()) {
            logger.error(MessageFormat.format(LOGGER_ERROR_01, getPrefix(), statusCode));
            logger.error(message);
        }
    }

    /**
     * <p>
     * This method is used to log error part.
     * </p>
     *
     * @param exception - trace of exception
     */
    public static void logError(Exception exception) {
        if (logger.isErrorEnabled()) {
            logger.error(MessageFormat.format(LOGGER_01, getPrefix(), exception.getMessage()), exception);
        }
    }

    /**
     * <p>
     * This method is used to log error part with message.
     * </p>
     *
     * @param message   - content of log
     * @param exception - trace of exception
     */
    public static void logError(String message, Exception exception) {
        if (logger.isErrorEnabled()) {
            logger.error(MessageFormat.format(LOGGER_01, getPrefix(), message), exception);
        }
    }

    /**
     * <p>
     * This method is used to log info part.
     * </p>
     *
     * @param message - content of log
     * @param aClass  - class entity
     */
    public static void logInfo(String message, Class<?> aClass) {
        if (logger.isInfoEnabled()) {            
            logger.info(MessageFormat.format(LOGGER_012, getPrefix(), message, aClass.getSimpleName()));
        }
    }

    /**
     * <p>
     * This method is used to log warning part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logInfo(String message) {
        if (logger.isInfoEnabled()) {
            logger.info(MessageFormat.format(LOGGER_01, getPrefix(), message));
        }
    }

    /**
     * <p>
     * This method is used to log warning part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logWarn(String message) {
        if (logger.isWarnEnabled()) {
            logger.warn(MessageFormat.format(LOGGER_01, getPrefix(), message));
        }
    }
/**
     * This method is used to log trace part.
     *
     * @param message - content of log
     */
    /**
     * <p>
     * This method is used to log trace part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logTrace(String message) {
        if (logger.isTraceEnabled()) {
            logger.trace(MessageFormat.format(LOGGER_01, getPrefix(), message));
        }
    }

}
