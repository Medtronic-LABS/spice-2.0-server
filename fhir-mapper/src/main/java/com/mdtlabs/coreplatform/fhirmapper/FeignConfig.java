package com.mdtlabs.coreplatform.fhirmapper;

import java.util.concurrent.TimeUnit;

import feign.Request;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class for Feign client settings.
 * <p>
 * This class provides configuration for Feign clients used in the application, specifically setting
 * custom timeout values for connections and read operations. These timeout values are configurable
 * via application properties, allowing for flexibility across different environments.
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
     * Provides custom configuration for Feign client request options.
     * <p>
     * This bean method configures the Feign client request options, setting the connection and read timeouts
     * to the values specified in the application properties or their default values.
     * </p>
     *
     * @return Request.Options object configured with custom timeout settings.
     */
    @Bean
    public Request.Options requestOptions() {
        return new Request.Options(connectionTimeOut, TimeUnit.MILLISECONDS, readTimeOut, TimeUnit.MILLISECONDS, true); // Connect and read timeout in milliseconds
    }
}