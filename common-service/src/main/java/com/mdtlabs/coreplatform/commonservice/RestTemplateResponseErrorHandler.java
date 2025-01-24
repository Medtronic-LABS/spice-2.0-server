package com.mdtlabs.coreplatform.commonservice;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.client.ResponseErrorHandler;

import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;

/**
 * Custom error handler for {@link ResponseErrorHandler} to handle errors from external API calls.
 * <p>
 * This component intercepts errors from REST calls made using {@link ResponseErrorHandler} and throws
 * appropriate exceptions based on the HTTP status code of the response. It is capable of handling
 * common HTTP status errors such as 404 (Not Found), 409 (Conflict), and other client errors by
 * throwing custom exceptions that can be further handled by the application.
 * </p>
 *
 * @author Nandhakumar
 * @since 05 Feb 2024
 */
@Component
public class RestTemplateResponseErrorHandler implements ResponseErrorHandler {

    /**
     * Determines if the response has an error based on the HTTP status code.
     * <p>
     * This method checks if the HTTP status code of the response indicates an error
     * (4xx client error or 5xx server error).
     * </p>
     *
     * @param clientHttpResponse The response from the client.
     * @return true if the status code is an error, false otherwise.
     * @throws IOException If an input or output exception occurs.
     */
    @Override
    public boolean hasError(ClientHttpResponse clientHttpResponse) throws IOException {
        return clientHttpResponse.getStatusCode().isError();
    }

    /**
     * Handles the error in the client HTTP response.
     * <p>
     * This method extracts the HTTP status code and error message from the response and throws
     * a specific custom exception based on the status code. It supports handling 409 (Conflict),
     * 404 (Not Found), and other errors by throwing {@link DataConflictException},
     * {@link DataNotFoundException}, or {@link BadRequestException} respectively.
     * </p>
     *
     * @param clientHttpResponse The client HTTP response with the error.
     * @throws IOException If an input or output exception occurs.
     */
    @Override
    public void handleError(ClientHttpResponse clientHttpResponse) throws IOException {
        HttpStatus statusCode = (HttpStatus) clientHttpResponse.getStatusCode();
        String errorMessage = extractErrorMessage(clientHttpResponse);

        switch (statusCode) {
            case CONFLICT:
                throw new DataConflictException(1006, errorMessage);
            case NOT_FOUND:
                throw new DataNotFoundException(1006, errorMessage);
            default:
                throw new BadRequestException(1006, errorMessage);
        }
    }

    /**
     * Extracts the error message from the client HTTP response.
     * <p>
     * This method reads the response body and concatenates it into a single string to be used
     * as the error message in the exceptions thrown by {@link #handleError(ClientHttpResponse)}.
     * </p>
     *
     * @param clientHttpResponse The client HTTP response from which to extract the error message.
     * @return The extracted error message as a string.
     * @throws IOException If an input or output exception occurs while reading the response body.
     */
    private String extractErrorMessage(ClientHttpResponse clientHttpResponse) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(clientHttpResponse.getBody(), StandardCharsets.UTF_8))) {
            return reader.lines().collect(Collectors.joining("\n"));
        }
    }

}
