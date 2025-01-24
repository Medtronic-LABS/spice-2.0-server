package com.mdtlabs.coreplatform.commonservice.common.message;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.domain.Paged;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;

/**
 * <p>
 * Generic success response.
 * </p>
 *
 * @author Sharveshkumar created on Apr 02, 2024
 *
 * @param <T> - object param
 */
public class SuccessResponse<T> extends ResponseEntity<Object> {

	/**
	 * Constructs a SuccessResponse with a success code, paged data, and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param paged The paged data to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(SuccessCode successCode, Paged<T> paged, HttpStatus responseCode) {
		this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
			Constants.SUCCESS), paged, responseCode);
	}

	/**
	 * Constructs a SuccessResponse with a message, paged data, and a response code.
	 *
	 * @param message The success message to be included in the response.
	 * @param paged The paged data to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(String message, Paged<T> paged, HttpStatus responseCode) {
		this(message, null, paged.getList(), responseCode,  paged.getCount());
	}

	/**
	 * Constructs a SuccessResponse with a success code, a list of entities, and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param entity The list of entities to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(SuccessCode successCode, List<T> entity, HttpStatus responseCode) {
		this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
			Constants.SUCCESS), null, entity, responseCode, null);
	}

	/**
	 * Constructs a SuccessResponse with a success code, a list of entities, total count, and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param entity The list of entities to be included in the response.
	 * @param totalCount The total count of entities.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(SuccessCode successCode, List<T> entity, long totalCount, HttpStatus responseCode) {
	    this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
	    	Constants.SUCCESS), null, entity, responseCode, totalCount);
	}

	/**
	 * Constructs a SuccessResponse with a success code, an entity, total count, and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param entity The entity to be included in the response.
	 * @param totalCount The total count of entities.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(SuccessCode successCode, Object entity, long totalCount, HttpStatus responseCode) {
		this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
			Constants.SUCCESS), entity, null, responseCode, totalCount);
	}

	/**
	 * Constructs a SuccessResponse with a message, a list of entities, and a response code.
	 *
	 * @param message The success message to be included in the response.
	 * @param entity The list of entities to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(String message, List<T> entity, HttpStatus responseCode) {
		this(message, null, entity, responseCode, null);
	}

	/**
	 * Constructs a SuccessResponse with a success code, an entity, and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param entity The entity to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode) {
		this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
			Constants.SUCCESS), entity, null, responseCode, null);
	}

	/**
	 * Constructs a SuccessResponse with a message, an entity, and a response code.
	 *
	 * @param message The success message to be included in the response.
	 * @param entity The entity to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(String message, Object entity, HttpStatus responseCode) {
		this(message, entity, null, responseCode, null);
	}

	/**
	 * Constructs a SuccessResponse with a success code and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param responseCode The HTTP status code to be set for the response.
	 */
	public SuccessResponse(SuccessCode successCode, HttpStatus responseCode) {
		this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
			Constants.SUCCESS), null, null, responseCode, null);
	}

	/**
	 * Constructs a SuccessResponse with a message, an entity, a list of entities, a total count, and a response code.
	 *
	 * @param message The success message to be included in the response.
	 * @param entity The entity to be included in the response.
	 * @param entityList The list of entities to be included in the response.
	 * @param httpStatus The HTTP status code to be set for the response.
	 * @param totalCount The total count of entities.
	 */
	public SuccessResponse(String message, Object entity, List<T> entityList, HttpStatus httpStatus,
		Long totalCount) {
		super(new SuccessMessage<T>(Boolean.TRUE, message, entity, entityList, 
			Integer.valueOf(httpStatus.value()), totalCount), httpStatus);
	}

	/**
	 * Constructs a SuccessResponse with a success code, a list of entities, a total count, and a response code.
	 *
	 * @param successCode The success code used to fetch the success message.
	 * @param entity The list of entities to be included in the response.
	 * @param responseCode The HTTP status code to be set for the response.
	 * @param totalCount The total count of entities.
	 */
	public SuccessResponse(SuccessCode successCode, List<T> entity, HttpStatus responseCode, Long totalCount) {
		this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), 
			Constants.SUCCESS), null, entity, responseCode, totalCount);
	}

}
