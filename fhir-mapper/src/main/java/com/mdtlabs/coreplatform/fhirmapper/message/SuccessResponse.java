package com.mdtlabs.coreplatform.fhirmapper.message;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;

/**
 * <p>
 * Generic success response.
 * </p>
 *
 * @param <T> - object param
 * @author Nandhakumar created on Feb 05, 2024
 */
public class SuccessResponse<T> extends ResponseEntity<Object> {

    public SuccessResponse(SuccessCode successCode, List<T> entity, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()),
                Constants.SUCCESS), null, entity, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, List<T> entity, long totalCount, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()),
                Constants.SUCCESS), null, entity, responseCode, totalCount);
    }

    public SuccessResponse(SuccessCode successCode, Object entity, long totalCount, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()),
                Constants.SUCCESS), entity, null, responseCode, totalCount);
    }

    public SuccessResponse(String message, List<T> entity, HttpStatus responseCode) {
        this(message, null, entity, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()),
                Constants.SUCCESS), entity, null, responseCode, null);
    }

    public SuccessResponse(String message, Object entity, HttpStatus responseCode) {
        this(message, entity, null, responseCode, null);
    }

    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()),
                Constants.SUCCESS), null, null, responseCode, null);
    }

    public SuccessResponse(String message, Object entity, List<T> entityList, HttpStatus httpStatus,
                           Long totalCount) {
        super(new SuccessMessage<T>(Boolean.TRUE, message, entity, entityList,
                Integer.valueOf(httpStatus.value()), totalCount), httpStatus);
    }

    public SuccessResponse(SuccessCode successCode, List<T> entity, HttpStatus responseCode, Long totalCount) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()),
                Constants.SUCCESS), null, entity, responseCode, totalCount);
    }

}
