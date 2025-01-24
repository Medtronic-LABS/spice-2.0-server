package com.mdtlabs.coreplatform.userservice;

import feign.Request;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.TimeUnit;

/**
 * <p>
 * Configuration class for Feign client settings.
 * </p>
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
     * <p>
     * Configures the request options for Feign clients.
     * This method sets the connection timeout and read timeout for requests made by Feign clients.
     * The timeout values are configurable through application properties.
     * </p>
     *
     * @return A configured Request.Options object with specified connection and read timeouts.
     */
    @Bean
    public Request.Options requestOptions() {
        return new Request.Options(connectionTimeOut, TimeUnit.MILLISECONDS, readTimeOut, TimeUnit.MILLISECONDS, true);
    }
}