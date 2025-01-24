package com.mdtlabs.coreplatform.offlineservice.message;

import com.mdtlabs.coreplatform.commonservice.common.MessageValidator;
import com.mdtlabs.coreplatform.commonservice.common.SuccessMessage;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.List;



/**
 * <p>
 * Generic success response.
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 *
 * @param <T> - object
 */

public class SuccessResponse<T> extends ResponseEntity<Object> {

    private static final String SUCCESS = "SUCCESS";
    
    public SuccessResponse(String message, Object entity, List<T> entityList, HttpStatus httpStatus, Long totalCount) {
        super(new SuccessMessage<T>(Boolean.TRUE, message, entity, entityList, Integer.valueOf(httpStatus.value()),
            totalCount), httpStatus);
    }
    
    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), SUCCESS),
            entity, null, responseCode, null);
    }
    
    public SuccessResponse(SuccessCode successCode, List<T> entityList, Long totalCount, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), SUCCESS),
            null, entityList, responseCode, totalCount);
    }
    
    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), SUCCESS), null,
            null, responseCode, null);
    }
    
    public SuccessResponse(SuccessCode successCode, HttpStatus responseCode, String ... args) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(
            successCode.getKey()), SUCCESS, args), null, null, responseCode, null);
    }
    
    public SuccessResponse(SuccessCode successCode, Object entity, HttpStatus responseCode, String... args) {
        this(MessageValidator.getInstance().getMessage(Integer.toString(successCode.getKey()), SUCCESS, args),
            entity, null, responseCode, null);
    }
}
