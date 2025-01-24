package com.mdtlabs.coreplatform.offlineservice;

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
     * Provides custom configuration for Feign client request options.
     *
     * @return Request.Options object
     */
    @Bean
    public Request.Options requestOptions() {
        return new Request.Options(connectionTimeOut, TimeUnit.MILLISECONDS, readTimeOut, TimeUnit.MILLISECONDS, true); // Connect and read timeout in milliseconds
    }
}