package com.mdtlabs.coreplatform.commonservice.common.exception;

import java.util.List;

/**
 * This class is used to handle SendMailException.
 * 
 * @author Karthick Murugesan Created on Jan 11, 2024
 */
public class SendMailException extends ServicesException {

	/**
	 * <p>
	 * This is the default constructor for the SendMailException class.
	 * </p>
	 *
	 * <p>
	 * When an instance of SendMailException is created using this constructor,
	 * it calls another constructor in the same class, passing a predefined error code (1001)
	 * and the simple name of the class as arguments.
	 * </p>
	 */
	public SendMailException() {
		this(1001, SendMailException.class.getSimpleName());
	}

	/**
	 * <p>
	 * Constructs an instance of SendMailException with the specified error code.
	 * Calls another constructor in the same class, passing the specified error code and the simple name of the class as arguments.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 */
	public SendMailException(final Integer code) {
		this(code, SendMailException.class.getSimpleName());
	}

	/**
	 * <p>
	 * Constructs an instance of SendMailException with the specified error code and variable number of message parameters.
	 * Calls the constructor of the superclass ServicesException with the specified error code and message parameters.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param params the variable number of message parameters to be set for the exception.
	 */
	public SendMailException(final Integer code, final String... params) {
		super(code, params);
	}

	/**
	 * <p>
	 * Constructs an instance of SendMailException with the specified error code and message.
	 * Calls the constructor of the superclass ServicesException with the specified error code and message.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param message the error message to be set for the exception.
	 */
	public SendMailException(final Integer code, final String message) {
		super(code, message);
	}

	/**
	 * <p>
	 * Constructs an instance of SendMailException with the specified error code and list of message parameters.
	 * Calls the constructor of the superclass ServicesException with the specified error code and message parameters.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param params the list of message parameters to be set for the exception.
	 */
	public SendMailException(final Integer code, final List<String> params) {
		super(code, params);
	}
}
