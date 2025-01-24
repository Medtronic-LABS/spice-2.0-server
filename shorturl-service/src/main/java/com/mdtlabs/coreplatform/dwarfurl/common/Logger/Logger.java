package com.mdtlabs.coreplatform.dwarfurl.common.Logger;

import java.text.MessageFormat;

/**
 * <p>
 * Logger information
 * </p>
 *
 * @author Praveen created on Sep 9, 2024
 */
public class Logger extends BaseLogger {

    private static final String LOGGER_01 = "{0}";
    private static final String LOGGER_012 = "{0} - {1}";


    /**
     * <p>
     * This method is used to log debug part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logDebug(String message) {
        if (logger.isDebugEnabled()) {
            logger.debug(MessageFormat.format(LOGGER_01, message));
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
            logger.error(MessageFormat.format(LOGGER_01, message));
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
            logger.error(MessageFormat.format(LOGGER_01, message), exception);
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
            logger.info(MessageFormat.format(LOGGER_012, message, aClass.getSimpleName()));
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
            logger.info(MessageFormat.format(LOGGER_01, message));
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
            logger.warn(MessageFormat.format(LOGGER_01, message));
        }
    }

    /**
     * <p>
     * This method is used to log trace part.
     * </p>
     *
     * @param message - content of log
     */
    public static void logTrace(String message) {
        if (logger.isTraceEnabled()) {
            logger.trace(MessageFormat.format(LOGGER_01, message));
        }
    }
}
