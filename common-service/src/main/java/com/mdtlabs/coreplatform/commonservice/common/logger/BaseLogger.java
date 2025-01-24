package com.mdtlabs.coreplatform.commonservice.common.logger;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mdtlabs.coreplatform.commonservice.common.Constants;


/**
 * <p>
 * Logger information
 * </p>
 * 
 * @author Karthick Murugesan created on Jan 11, 2024
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
