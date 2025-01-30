package com.mdtlabs.coreplatform.adminservice;

import feign.Request;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.TimeUnit;

/**
 * Configuration class for Feign client settings.
 *
 * @author Praveen Created on 13th May 2024
 */
@Configuration
public class FeignConfig {

    @Value("${app.connectionTimeOut:30000}")
    private Integer connectionTimeOut;

    @Value("${app.readTimeOut:30000}")
    private Integer readTimeOut;

    /**
     * Configures the request options for Feign clients.
     * <p>
     * This method sets up custom request options for Feign clients, specifically the connection timeout and read timeout values.
     * These timeouts are configured through application properties and are applied to all Feign clients in the application,
     * ensuring consistent behavior across external service calls.
     * </p>
     *
     * @return A configured {@link Request.Options} instance with specified connection and read timeout values.
     */
    @Bean
    public Request.Options requestOptions() {
        return new Request.Options(connectionTimeOut, TimeUnit.MILLISECONDS, readTimeOut, TimeUnit.MILLISECONDS, true);
    }
}