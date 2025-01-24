package com.mdtlabs.coreplatform.commonservice.common.exception;

import java.util.List;

/**
 * <p>
 * This class extends ServicesException and is used for forbidden operation related exceptions.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Feb 08, 2024
 */
public class ForbiddenException extends ServicesException {

    /**
     * <p>
     * This is the default constructor for the ForbiddenException class.
     * </p>
     *
     * <p>
     * When an instance of ForbiddenException is created using this constructor,
     * it calls another constructor in the same class, passing a predefined error code (1001)
     * and the simple name of the class as arguments.
     * </p>
     */
    public ForbiddenException() {
        this(1001, ForbiddenException.class.getSimpleName());
    }

    /**
     * <p>
     * Constructs an instance of ForbiddenException with the specified error code.
     * Calls another constructor in the same class, passing the specified error code and the simple name of the class as arguments.
     * </p>
     *
     * @param code the error code to be set for the exception.
     */
    public ForbiddenException(final Integer code) {
        this(code, ForbiddenException.class.getSimpleName());
    }

    /**
     * <p>
     * Constructs an instance of ForbiddenException with the specified error code and variable number of message parameters.
     * Calls the constructor of the superclass ServicesException with the specified error code and message parameters.
     * </p>
     *
     * @param code the error code to be set for the exception.
     * @param params the variable number of message parameters to be set for the exception.
     */
    public ForbiddenException(final Integer code, final String... params) {
        super(code, params);
    }

    /**
     * <p>
     * Constructs an instance of ForbiddenException with the specified error code and message.
     * Calls the constructor of the superclass ServicesException with the specified error code and message.
     * </p>
     *
     * @param code the error code to be set for the exception.
     * @param message the error message to be set for the exception.
     */
    public ForbiddenException(final Integer code, final String message) {
        super(code, message);
    }

    /**
     * <p>
     * Constructs an instance of ForbiddenException with the specified error code and list of message parameters.
     * Calls the constructor of the superclass ServicesException with the specified error code and message parameters.
     * </p>
     *
     * @param code the error code to be set for the exception.
     * @param params the list of message parameters to be set for the exception.
     */
    public ForbiddenException(final Integer code, final List<String> params) {
        super(code, params);
    }

}