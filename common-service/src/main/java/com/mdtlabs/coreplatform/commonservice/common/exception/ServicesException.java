package com.mdtlabs.coreplatform.commonservice.common.exception;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;



/**
 * <p>
 * This class is used to handle ServicesException.
 * </p>
 * 
 * @author Karthick Murugesan Created on 30 Jun 2022
 */
public class ServicesException extends RuntimeException {

	private static final long serialVersionUID = 6918269662648545345L;
	private final Integer code;
	private final String message;

	/**
	 * <p>
	 * Constructs an instance of ServicesException with the specified error code.
	 * The message is set to the simple name of the class.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 */
	public ServicesException(final Integer code) {
		this.code = code;
		this.message = this.getClass().getSimpleName();
	}

	/**
	 * <p>
	 * Constructs an instance of ServicesException with the specified error code and message.
	 * The message is retrieved from the MessageValidator instance.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param message the error message to be set for the exception.
	 */
	public ServicesException(final Integer code, final String message) {
		this.code = code;
		this.message = MessageValidator.getInstance().getMessage(code.toString(), Constants.ERROR, message);
	}

	/**
	 * <p>
	 * Constructs an instance of ServicesException with the specified error code and variable number of message parameters.
	 * The message is retrieved from the MessageValidator instance.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param params the variable number of message parameters to be set for the exception.
	 */
	public ServicesException(final Integer code, final String... params) {
		this.code = code;
		this.message = MessageValidator.getInstance().getMessage(code.toString(), Constants.ERROR, params);
	}

	/**
	 * <p>
	 * Constructs an instance of ServicesException with the specified error code and list of message parameters.
	 * The message is retrieved from the MessageValidator instance.
	 * </p>
	 *
	 * @param code the error code to be set for the exception.
	 * @param params the list of message parameters to be set for the exception.
	 */
	public ServicesException(final Integer code, final List<String> params) {
		this.code = code;
		this.message = MessageValidator.getInstance().getMessage(code.toString(), Constants.ERROR, params);
	}

	/**
	 * <p>
	 * This method returns the error code of the exception.
	 * </p>
	 *
	 * @return the error code of the exception.
	 */
	public Integer getCode() {
		return code;
	}

	/**
	 * <p>
	 * This method returns the error message of the exception.
	 * </p>
	 *
	 * @return the error message of the exception.
	 */
	@Override
	public String getMessage() {
		return message;
	}

}
