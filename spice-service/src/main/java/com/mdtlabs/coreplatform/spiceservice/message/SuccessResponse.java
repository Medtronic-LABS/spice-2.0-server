package com.mdtlabs.coreplatform.spiceservice.message;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;

/**
 * <p>
 * Generic success response.
 * </p>
 *
 * @param <T> - object
 * @author Nandhakumar created on Jan 04, 2024
 */
public class SuccessResponse<T> extends ResponseEntity<Object> {

    public SuccessResponse(String message, Object entity, List<T> entityList, HttpStatus httpStatus, Long totalCount) {
        super(new SuccessMessage<T>(Boolean.TRUE, message, entity, entityList, Integer.valueOf(httpStatus.value()),
                totalCount), httpStatus);
    }

    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), "SUCCESS"),
                entity, null, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, List<T> entityList, Long totalCount, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), "SUCCESS"),
                null, entityList, responseCode, totalCount);
    }

    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), "SUCCESS"), null,
                null, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode, String... args) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(
                successCode.getKey()), "SUCCESS", args), null, null, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode, String... args) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), "SUCCESS", args),
                entity, null, responseCode, null);
    }
}
