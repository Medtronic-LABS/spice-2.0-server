package com.mdtlabs.coreplatform.commonservice.common.exception;

import java.util.List;

/**
 * <p>
 * This class extends the ServicesException class and is used to handle the SpiceValidation exception.
 * </p>
 *
 * @author Karthick Murugesan Created on Jan 11, 2024
 */
public class SpiceValidation extends ServicesException {

	/**
	 * <p>
	 * This is the default constructor for the SpiceValidation class.
	 * </p>
	 *
	 * <p>
	 * When an instance of SpiceValidation is created using this constructor,
	 * it calls another constructor in the same class, passing a predefined error code (1001)
	 * and the simple name of the class as arguments.
	 * </p>
	 */
	public SpiceValidation() {
		this(1001, SpiceValidation.class.getSimpleName());
	}

	/**
	 * <p>
	 * Constructs an instance of SpiceValidation with the specified error code.
	 * Calls another constructor in the same class, passing the specified error code and the simple name of the class as arguments.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 */
	public SpiceValidation(final Integer code) {
		this(code, SpiceValidation.class.getSimpleName());
	}

	/**
	 * <p>
	 * Constructs an instance of SpiceValidation with the specified error code and variable number of message parameters.
	 * Calls the constructor of the superclass ServicesException with the specified error code and message parameters.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param params the variable number of message parameters to be set for the exception.
	 */
	public SpiceValidation(final Integer code, final String... params) {
		super(code, params);
	}

	/**
	 * <p>
	 * Constructs an instance of SpiceValidation with the specified error code and message.
	 * Calls the constructor of the superclass ServicesException with the specified error code and message.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param message the error message to be set for the exception.
	 */
	public SpiceValidation(final Integer code, final String message) {
		super(code, message);
	}

	/**
	 * <p>
	 * Constructs an instance of SpiceValidation with the specified error code and list of message parameters.
	 * Calls the constructor of the superclass ServicesException with the specified error code and message parameters.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param params the list of message parameters to be set for the exception.
	 */
	public SpiceValidation(final Integer code, final List<String> params) {
		super(code, params);
	}
}