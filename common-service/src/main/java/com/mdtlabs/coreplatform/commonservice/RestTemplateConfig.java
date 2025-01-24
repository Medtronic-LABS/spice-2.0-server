package com.mdtlabs.coreplatform.commonservice;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;

/**
 * Configuration class for customizing {@link RestTemplate} instances.
 * <p>
 * This class utilizes Spring's {@link RestTemplateBuilder} to create a {@link RestTemplate} bean
 * with custom connection and read timeouts. These timeouts are configurable via application properties.
 * Additionally, it sets a custom error handler for the {@link RestTemplate}.
 * </p>
 *
 * @author Nandhakumar
 * @since 05 Feb 2024
 */
@Configuration
public class RestTemplateConfig {

    @Value("${app.connectionTimeOut:30000}")
    private Long connectionTimeOut;

    @Value("${app.readTimeOut:30000}")
    private Long readTimeOut;

    /**
     * Creates and configures a {@link RestTemplate} bean.
     * <p>
     * The method configures the {@link RestTemplate} with custom connection and read timeouts.
     * It also sets a custom error handler provided by {@link RestTemplateResponseErrorHandler}.
     * </p>
     *
     * @param restTemplateResponseErrorHandler Custom error handler for handling REST client errors.
     * @return Configured {@link RestTemplate} instance.
     */
    @Bean
    public RestTemplate getRestTemplate(RestTemplateResponseErrorHandler restTemplateResponseErrorHandler) {
        RestTemplateBuilder restTemplateBuilder = new RestTemplateBuilder().setConnectTimeout(Duration.ofMillis(connectionTimeOut)).setReadTimeout(Duration.ofMillis(readTimeOut))
                .errorHandler(restTemplateResponseErrorHandler);
        return restTemplateBuilder.build();
    }
}