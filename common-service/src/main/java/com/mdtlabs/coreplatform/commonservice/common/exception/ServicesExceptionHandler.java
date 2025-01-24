package com.mdtlabs.coreplatform.commonservice.common.exception;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;

import java.util.Map;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletResponse;

import feign.FeignException;
import org.hibernate.exception.ConstraintViolationException;
import org.json.JSONObject;
import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.TransactionSystemException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

/**
 * <p>
 * The `ServicesExceptionHandler` class is a Java exception handler that handles various exceptions and
 * returns appropriate error messages with specific HTTP status codes.
 * </p>
 *
 * @author Divya S Created on Jan 22, 2024
 */
@ControllerAdvice
public class ServicesExceptionHandler extends ResponseEntityExceptionHandler {

    private ExceptionResolver resolver;

    /**
     * <p>
     * The function "initiateErrorCodeToResolver" initializes the "resolver" variable with a new instance
     * of the "ExceptionResolverImpl" class.
     * </p>
     */
    @PostConstruct
    public final void initiateErrorCodeToResolver() {
        resolver = new ExceptionResolverImpl();
    }


    /**
     * <p>
     * The above function is a Java exception handler that handles InvalidPathException and returns an
     * error message with a specific HTTP status code.
     * </p>
     *
     * @param runtimeException The `runtimeException` parameter is an instance of the `RuntimeException`
     *                         class. It represents an exception that occurred during the runtime of the application.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.PRECONDITION_FAILED)
    @ExceptionHandler(value = InvalidPathException.class)
    @ResponseBody
    public ErrorMessage runtimeExceptionHandler(RuntimeException runtimeException) {
        Logger.logError(StringUtil.constructString(runtimeException.getClass().getName(),
                ExceptionConstants.MESSAGE_GENERIC, getErrorStackString(runtimeException)));

        return resolver.resolveError(HttpStatus.INTERNAL_SERVER_ERROR, runtimeException.getMessage());
    }

    /**
     * <p>
     * Handles the invalid method Argument exception.
     * </p>
     *
     * @param ex      the exception
     * @param headers the headers to be written to the response
     * @param status  the selected response status
     * @param request the current request
     * @return Error message
     */
    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex,
                                                                  HttpHeaders headers, HttpStatusCode status, WebRequest request) {
        Logger.logError(StringUtil.constructString(ex.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(ex)));
        List<String> rejectedValues = ex.getBindingResult().getAllErrors().stream()
                .map(DefaultMessageSourceResolvable::getDefaultMessage).toList();
        return handleExceptionInternal(ex, resolver.resolveError(HttpStatus.BAD_REQUEST, rejectedValues.toString()),
                headers, status, request);
    }

    /**
     * <p>
     * The above function is a Java exception handler that handles DataConflictException by logging an
     * error and returning an ErrorMessage object with a conflict status and the exception message.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the "DataConflictException" class. It
     *                  is an exception that is thrown when there is a conflict in data, typically when trying to create or
     *                  update a resource that already exists.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.CONFLICT)
    @ExceptionHandler(value = DataConflictException.class)
    @ResponseBody
    protected final ErrorMessage handleDataConflict(DataConflictException exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));
        return resolver.resolveError(HttpStatus.CONFLICT, exception.getMessage());
    }

    /**
     * <p>
     * The above function handles the DataNotAcceptableException by logging an error and returning an
     * ErrorMessage with the appropriate HTTP status code and exception message.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the "DataNotAcceptableException"
     *                  class, which is an exception that is thrown when the data being processed is not acceptable or
     *                  valid.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    @ExceptionHandler(value = DataNotAcceptableException.class)
    @ResponseBody
    protected final ErrorMessage handleDataNotAcceptable(DataNotAcceptableException exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(),
                ExceptionConstants.MESSAGE_GENERIC, getErrorStackString(exception)));
        return resolver.resolveError(HttpStatus.NOT_ACCEPTABLE, exception.getMessage());
    }

    /**
     * <p>
     * The above function handles a BadRequestException by logging the error and returning an ErrorMessage
     * object with the appropriate status and message.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the "BadRequestException" class, which
     *                  is an exception that is thrown when a client sends a request that cannot be processed due to
     *                  invalid syntax or missing required parameters.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(value = BadRequestException.class)
    @ResponseBody
    protected final ErrorMessage handleBadRequest(BadRequestException exception) {

        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));
        return resolver.resolveError(HttpStatus.BAD_REQUEST, exception.getMessage());
    }

    /**
     * <p>
     * The function handles a DataNotFoundException by logging an error and returning an ErrorMessage with
     * a NOT_FOUND status and the exception message.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the "DataNotFoundException" class,
     *                  which is an exception that is thrown when data is not found.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ExceptionHandler(value = DataNotFoundException.class)
    @ResponseBody
    protected final ErrorMessage handleDataNotFoundException(DataNotFoundException exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));
        return resolver.resolveError(HttpStatus.NOT_FOUND, exception.getMessage());
    }


    /**
     * <p>
     * The function handles TransactionSystemException by logging the error and returning an error message.
     * </p>
     *
     * @param validationException The validationException parameter is an instance of the
     *                            TransactionSystemException class.
     * @return The method is returning an ErrorMessage object.
     */
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(value = TransactionSystemException.class)
    @ResponseBody
    public ErrorMessage handleTransactionSystemException(TransactionSystemException validationException) {
        Logger.logError(StringUtil.constructString(validationException.getClass().getName(),
                ExceptionConstants.MESSAGE_GENERIC, getErrorStackString(validationException)));
        return resolver.resolveError(HttpStatus.BAD_REQUEST, validationException.getMessage());
    }


    /**
     * <p>
     * This function handles ConstraintViolationException by logging the error and returning an error message with the
     * corresponding HTTP status code.
     * </p>
     *
     * @param exception {@link ConstraintViolationException} The "exception" parameter is an instance of the ConstraintViolationException class, which is thrown
     *                  when a constraint violation occurs during data validation.
     * @return {@link ErrorMessage}The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(value = ConstraintViolationException.class)
    @ResponseBody
    protected final ErrorMessage handleConstraintViolation(ConstraintViolationException exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));
        return resolver.resolveError(HttpStatus.BAD_REQUEST, exception.getMessage());
    }

    /**
     * <p>
     * This function handles ForbiddenException by logging the error and returning an error message with the
     * corresponding HTTP status code.
     * </p>
     *
     * @param exception {@link ForbiddenException} The "exception" parameter is an instance of the ForbiddenException class, which is thrown
     *                  when a forbidden action was performed.
     * @return {@link ErrorMessage}The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.FORBIDDEN)
    @ExceptionHandler(value = ForbiddenException.class)
    @ResponseBody
    protected final ErrorMessage handleForbiddenException(ForbiddenException exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));
        return resolver.resolveError(HttpStatus.FORBIDDEN, exception.getMessage());
    }


    /**
     * <p>
     * The above function is a Java exception handler that logs and returns an error message for any
     * exception that occurs.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the Exception class, which represents
     *                  an unexpected error or exception that occurred during the execution of a program. It can be any
     *                  type of exception, such as a NullPointerException, IOException, or any custom exception defined in
     *                  the code.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    @ExceptionHandler(value = Exception.class)
    @ResponseBody
    public ErrorMessage exceptionHandler(Exception exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));

        return resolver.resolveError(HttpStatus.INTERNAL_SERVER_ERROR, exception.getMessage());
    }

    /**
     * <p>
     * The function "getErrorStackString" takes an Exception object as input and returns a string
     * representation of the error stack trace.
     * </p>
     *
     * @param error The error parameter is an Exception object that represents an error or exception that
     *              occurred in the code.
     * @return The method is returning a string representation of the stack trace of the given exception.
     */
    private String getErrorStackString(Exception error) {
        StringWriter writer = new StringWriter();
        error.printStackTrace(new PrintWriter(writer));
        return writer.toString();
    }


    /**
     * <p>
     * The above function handles exceptions of type ServicesException and SpiceValidation by logging the
     * error and returning an ErrorMessage object with a status code and error message.
     * </p>
     *
     * @param servicesException The `servicesException` parameter is an exception object of type
     *                          `ServicesException`.
     * @return The method is returning an instance of the ErrorMessage class.
     */
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(value = {ServicesException.class,
            SpiceValidation.class})
    @ResponseBody
    protected final ErrorMessage handleException(ServicesException servicesException) {
        Logger.logError(StringUtil.constructString(servicesException.getClass().getName(),
                ExceptionConstants.MESSAGE_GENERIC, getErrorStackString(servicesException)));

        return resolver.resolveError(HttpStatus.BAD_REQUEST, servicesException.getMessage());
    }

    /**
     * <p>
     * This Java function handles unauthorized exceptions and returns an error message.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the "Exception" class, which
     *                  represents an exception that occurred during the execution of the code. In this case, it is used to
     *                  handle the FeignException.Unauthorized exception, which is thrown when an unauthorized request is
     *                  made to a Feign client.
     * @return The method is returning an ErrorMessage object.
     */
    @ResponseStatus(HttpStatus.UNAUTHORIZED)
    @ExceptionHandler(value = FeignException.Unauthorized.class)
    @ResponseBody
    public ErrorMessage authExceptionHandler(Exception exception) {
        Logger.logError(StringUtil.constructString(exception.getClass().getName(), ExceptionConstants.MESSAGE_GENERIC,
                getErrorStackString(exception)));

        return resolver.resolveError(HttpStatus.UNAUTHORIZED, exception.getMessage());
    }

    /**
     * <p>
     * The function handles a FeignException.NotFound exception by setting the response status to 404 and
     * returning a map of the exception content.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the FeignException.NotFound class,
     *                  which is thrown when a Feign client encounters a 404 Not Found response from the server it is
     *                  communicating with. This exception contains information about the HTTP response, such as the status
     *                  code and the response body.
     * @param response  The `response` parameter is an instance of the `HttpServletResponse` class, which
     *                  represents the HTTP response that will be sent back to the client. It is used to set the status code
     *                  of the response using the `setStatus()` method.
     * @return A Map<String, Object> is being returned.
     */
    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ExceptionHandler(FeignException.NotFound.class)
    @ResponseBody
    public Map<String, Object> handleFeignNotFoundException(FeignException exception, HttpServletResponse response) {
        response.setStatus(exception.status());
        return new JSONObject(exception.contentUTF8()).toMap();
    }

    /**
     * <p>
     * The function handles a FeignException of type NotAcceptable by setting the response status and
     * returning a map of the exception content.
     * </p>
     *
     * @param exception The exception parameter is an instance of the FeignException class, which is thrown
     *                  when a Feign client encounters an error while making a request to a remote service.
     * @param response  The `response` parameter is an instance of the `HttpServletResponse` class, which
     *                  represents the HTTP response that will be sent back to the client. It is used to set the status code
     *                  of the response using the `setStatus()` method.
     * @return A Map<String, Object> is being returned.
     */
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    @ExceptionHandler(FeignException.NotAcceptable.class)
    @ResponseBody
    public Map<String, Object> handleFeignNotAcceptableException(FeignException exception,
                                                                 HttpServletResponse response) {
        response.setStatus(exception.status());
        return new JSONObject(exception.contentUTF8()).toMap();
    }

    /**
     * <p>
     * The function handles a FeignException.BadRequest by setting the response status and returning a map
     * of the exception content.
     * </p>
     *
     * @param exception The "exception" parameter is an instance of the FeignException.BadRequest class,
     *                  which is thrown when a 400 Bad Request response is received from a Feign client.
     * @param response  The `response` parameter is an instance of the `HttpServletResponse` class, which
     *                  represents the HTTP response that will be sent back to the client. It is used to set the status code
     *                  of the response using the `setStatus()` method.
     * @return A Map<String, Object> is being returned.
     */
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(FeignException.BadRequest.class)
    @ResponseBody
    public Map<String, Object> handleFeignBadRequestException(FeignException exception, HttpServletResponse response) {
        response.setStatus(exception.status());
        return new JSONObject(exception.contentUTF8()).toMap();
    }

    /**
     * <p>
     * This Java function handles a FeignException of type Conflict by setting the HTTP response status
     * and returning a map of the exception content.
     *
     * @param exception The parameter "exception" is an object of type FeignException, which is an
     *                  exception thrown by the Feign client when a request to a remote service fails. It contains
     *                  information about the HTTP response status code, headers, and body.
     * @param response The `response` parameter is an instance of the `HttpServletResponse` class,
     *                  which represents the response that will be sent back to the client after the request has been
     *                  processed. It is used in this method to set the status of the response to the status code of the
     *                  FeignException`
     * @return A `Map<String, Object>` is being returned.
     */
    @ResponseStatus(HttpStatus.CONFLICT)
    @ExceptionHandler(FeignException.Conflict.class)
    @ResponseBody
    public Map<String, Object> handleFeignConflictException(FeignException exception, HttpServletResponse response) {
        response.setStatus(exception.status());
        return new JSONObject(exception.contentUTF8()).toMap();
    }

    /**
     * <p>
     * This Java function handles a FeignException of type Forbidden by setting the HTTP response status
     * and returning a map of the exception content.
     *
     * @param exception The parameter "exception" is an object of type FeignException, which is an
     *                  exception thrown by the Feign client when a request to a remote service fails. It contains
     *                  information about the HTTP response status code, headers, and body.
     * @param response The `response` parameter is an instance of the `HttpServletResponse` class,
     *                  which represents the response that will be sent back to the client after the request has been
     *                  processed. It is used in this method to set the status of the response to the status code of the
     *                  FeignException`
     * @return A `Map<String, Object>` is being returned.
     */
    @ResponseStatus(HttpStatus.FORBIDDEN)
    @ExceptionHandler(FeignException.Forbidden.class)
    @ResponseBody
    public Map<String, Object> handleFeignForbiddenException(FeignException exception, HttpServletResponse response) {
        response.setStatus(exception.status());
        return new JSONObject(exception.contentUTF8()).toMap();
    }
}
