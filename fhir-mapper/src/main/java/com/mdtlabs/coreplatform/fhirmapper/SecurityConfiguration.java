package com.mdtlabs.coreplatform.fhirmapper;

import java.util.Arrays;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.HttpStatusEntryPoint;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;

import com.mdtlabs.coreplatform.commonservice.AuthenticationFilter;
import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * Security configuration for the FHIR Mapper application.
 * <p>
 * This configuration class sets up security filters, CORS policies, and Swagger API documentation settings.
 * It enables web security and configures HTTP security settings to secure the application endpoints.
 * </p>
 */
@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

    /**
     * Configures the security filter chain for the application.
     * <p>
     * This method sets up the application's security filters, including CORS configuration, request authorization,
     * session management, CSRF protection, and basic authentication. It also integrates a custom authentication filter
     * for additional security measures.
     * </p>
     *
     * @param http                 The {@link HttpSecurity} object to be configured.
     * @param authenticationFilter The custom authentication filter to be applied to incoming requests.
     * @return The configured {@link SecurityFilterChain} object.
     * @throws Exception if an error occurs during the configuration.
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http, AuthenticationFilter authenticationFilter) throws Exception {
        authenticationFilter.setServiceName(Constants.FHIR_MAPPER_SERVICE);
        http.cors(cors -> cors.configurationSource(r -> {
                    CorsConfiguration configuration = new CorsConfiguration();
                    configuration.setAllowedOrigins(Arrays.asList("*")); //NOSONAR
                    configuration.setAllowedMethods(Arrays.asList("*"));
                    configuration.setAllowedHeaders(Arrays.asList("*"));
                    return configuration;
                }))
                .authorizeHttpRequests(requests -> requests.anyRequest().authenticated())
                .addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED))).sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                ).csrf(AbstractHttpConfigurer::disable).httpBasic(Customizer.withDefaults()); //NOSONAR
        return http.build();
    }

    /**
     * Configures Swagger API documentation for the application.
     * <p>
     * This method sets up the OpenAPI configuration for Swagger, including application information and security schemes.
     * It defines API key security schemes for tenant identification and authorization headers.
     * </p>
     *
     * @return The {@link OpenAPI} configuration object for Swagger documentation.
     */
    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI().info(new Info().title(Constants.FHIR_MAPPER_OPENAPI_TITLE).version(Constants.OPEN_API_VERSION))
                .components(new Components()
                        .addSecuritySchemes(Constants.TENANT_COLUMN_NAME,
                                new SecurityScheme().type(SecurityScheme.Type.APIKEY).in(SecurityScheme.In.HEADER)
                                        .name(Constants.HEADER_TENANT_ID))
                        .addSecuritySchemes(Constants.AUTHORIZATION_HEADER,
                                new SecurityScheme().type(SecurityScheme.Type.APIKEY).in(SecurityScheme.In.HEADER)
                                        .name(Constants.AUTHORIZATION)))
                .addSecurityItem(new SecurityRequirement().addList(Constants.AUTHORIZATION_HEADER,
                        Constants.TENANT_COLUMN_NAME));
    }

}
