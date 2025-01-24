package com.mdtlabs.coreplatform.spiceservice;

import feign.Request;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
     * Provides custom configuration for Feign client request options.
     *
     * @return Request.Options object
     */
    @Bean
    public Request.Options requestOptions() {
        return new Request.Options(connectionTimeOut, readTimeOut); // Connect and read timeout in milliseconds
    }
}