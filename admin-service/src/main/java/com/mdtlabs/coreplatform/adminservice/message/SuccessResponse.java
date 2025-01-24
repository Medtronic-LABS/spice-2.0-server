package com.mdtlabs.coreplatform.adminservice.message;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;

/**
 * <p>
 * The SuccessResponse class is a Java class that extends the ResponseEntity class and provides various constructors for
 * creating success responses with different types of data and HTTP status codes.
 * </p>
 *
 * @author Prabu created on Mar 03, 2023
 */
public class SuccessResponse<T> extends ResponseEntity<Object> {

    public SuccessResponse(String message, Object entity, List<T> entityList, HttpStatus httpStatus, Long totalCount) {
        super(new SuccessMessage<T>(Boolean.TRUE, message, entity, entityList, Integer.valueOf(httpStatus.value()),
                totalCount), httpStatus);
    }

    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS),
                entity, null, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, List<T> entityList, Long totalCount, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS), null,
                entityList, responseCode, totalCount);
    }

    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS), null,
                null, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode, String... args) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), Constants.SUCCESS, args),
                null, null, responseCode, null);
    }
}
