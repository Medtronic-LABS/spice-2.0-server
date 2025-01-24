package com.mdtlabs.coreplatform.userservice.message;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;

/**
 * <p>
 * The `SuccessResponse` class is a Java class that provides constructors for creating success responses with messages,
 * entities, and total counts.
 * </p>
 *
 * @param <T> - object
 * @author Prabu created on Sep 16, 2022
 */
public class SuccessResponse<T> extends ResponseEntity<Object> {

    /**
     * <p>
     * This constructor is used to create a new SuccessResponse object with a message, an entity, a list of entities, a HTTP status, and a total count.
     * It calls the parent class's constructor with a new SuccessMessage object and the provided HTTP status.
     * </p>
     *
     * @param message    The message of the success response.
     * @param entity     The entity of the success response.
     * @param entityList The list of entities of the success response.
     * @param httpStatus The HTTP status of the success response.
     * @param totalCount The total count of the success response.
     */
    public SuccessResponse(String message, Object entity, List<T> entityList, HttpStatus httpStatus, Long totalCount) {
        super(new SuccessMessage<T>(Boolean.TRUE, message, entity, entityList, Integer.valueOf(httpStatus.value()),
                totalCount), httpStatus);
    }

    /**
     * <p>
     * This constructor is used to create a new SuccessResponse object with a success code, an entity, and a HTTP response code.
     * It calls the parent class's constructor with a new SuccessMessage object and the provided HTTP status.
     * </p>
     *
     * @param successCode  The success code of the success response.
     * @param entity       The entity of the success response.
     * @param responseCode The HTTP response code of the success response.
     */
    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS),
                entity, null, responseCode, null);
    }

    /**
     * <p>
     * This constructor is used to create a new SuccessResponse object with a success code, a list of entities, a total count, and a HTTP response code.
     * It calls the parent class's constructor with a new SuccessMessage object and the provided HTTP status.
     * </p>
     *
     * @param successCode  The success code of the success response.
     * @param entityList   The list of entities of the success response.
     * @param totalCount   The total count of the success response.
     * @param responseCode The HTTP response code of the success response.
     */
    public SuccessResponse(SuccessCode successCode, List<T> entityList, Long totalCount, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS), null,
                entityList, responseCode, totalCount);
    }

    /**
     * <p>
     * This constructor is used to create a new SuccessResponse object with a success code and a HTTP response code.
     * It calls the parent class's constructor with a new SuccessMessage object and the provided HTTP status.
     * </p>
     *
     * @param successCode  The success code of the success response.
     * @param responseCode The HTTP response code of the success response.
     */
    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS), null,
                null, responseCode, null);
    }

    /**
     * <p>
     * This constructor is used to create a new SuccessResponse object with a success code, a HTTP response code, and an array of arguments.
     * It calls the parent class's constructor with a new SuccessMessage object and the provided HTTP status.
     * </p>
     *
     * @param successCode  The success code of the success response.
     * @param responseCode The HTTP response code of the success response.
     * @param args         The arguments to be included in the success message.
     */
    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode, String... args) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS, args),
                null, null, responseCode, null);
    }
}
