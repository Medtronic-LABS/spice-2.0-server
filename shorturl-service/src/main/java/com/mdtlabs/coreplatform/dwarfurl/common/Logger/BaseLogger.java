package com.mdtlabs.coreplatform.dwarfurl.common.Logger;

import com.mdtlabs.coreplatform.dwarfurl.common.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Base Logger information
 * </p>
 *
 * @author Praveen created on Sep 9, 2024
 *
 */
public abstract class BaseLogger {

    /**
     * <p>
     * The `protected BaseLogger() {}` is a constructor for the `BaseLogger` class.
     * </p>
     */
    protected BaseLogger() {}

    /**
     * <p>
     * A static logger object for the `BaseLogger` class using the
     * `LoggerFactory.getLogger()` method from the SLF4J logging framework
     * </p>
     */
    protected static Logger logger = LoggerFactory.getLogger(Constants.LOGGER);
}
