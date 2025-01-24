package com.mdtlabs.coreplatform.notificationservice;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;

import java.util.Arrays;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.jdbc.core.JdbcTemplate;
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

import net.javacrumbs.shedlock.core.LockProvider;
import net.javacrumbs.shedlock.provider.jdbctemplate.JdbcTemplateLockProvider;

/**
 * <p>
 * SecurityConfiguration class that configures security settings for an application, including
 * authentication and authorization mechanisms, and creates a custom OpenAPI specification
 * for notification service API with security schemes and requirements.
 * </p>
 *
 * @author Prabu Created on Mar 23, 2023
 */
@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

    /**
     * <p>
     * This method is used to create a lock provider using a JdbcTemplate object as the data source.
     * </p>
     *
     * @param dataSource {@link JdbcTemplate} A JdbcTemplate that provides a convenient way to interact with a relational
     *                   database using JDBC is given
     * @return {@link LockProvider} A LockProvider is being returned
     */
    @Bean
    public LockProvider lockProvider(JdbcTemplate dataSource) {
        return new JdbcTemplateLockProvider(dataSource);
    }

    /**
     * <p>
     * This method is used to configure the security filter chain for HTTP requests, allowing certain methods
     * and endpoints to be accessed without authentication while requiring authentication for all
     * other requests.
     * </p>
     *
     * @param http                 {@link HttpSecurity} The http security is used to configure security settings for
     *                             the application, and allows you to specify which requests should be authenticated,
     *                             which authentication mechanisms to use, and how to handle exceptions
     *                             related to authentication and authorization is given
     * @param authenticationFilter {@link AuthenticationFilter} The authenticationFilter is a custom filter
     *                             that handles authentication for incoming requests, and it is added to the
     *                             filter chain before the UsernamePasswordAuthenticationFilter, which is
     *                             the default filter for handling basic authentication is given
     * @return {@link SecurityFilterChain} The SecurityFilterChain that is built using http is returned
     * @throws Exception The exception in cors is thrown
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http, AuthenticationFilter authenticationFilter)
            throws Exception {
        authenticationFilter.setServiceName(Constants.NOTIFICATION_SERVICE);
        http.cors(cors -> cors.configurationSource(r -> {
                    CorsConfiguration configuration = new CorsConfiguration();
                    configuration.setAllowedOrigins(Arrays.asList("*")); //NOSONAR
                    configuration.setAllowedMethods(Arrays.asList("*"));
                    configuration.setAllowedHeaders(Arrays.asList("*"));
                    return configuration;
                })).authorizeHttpRequests(requests -> requests.anyRequest().authenticated())
                .addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
                .sessionManagement(session ->
                    session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                ).csrf(AbstractHttpConfigurer::disable).httpBasic(Customizer.withDefaults()); //NOSONAR
        return http.build();
    }

    /**
     * <p>
     * This method is used to create a custom OpenAPI specification for a user service API with
     * security schemes and requirements.
     * </p>
     *
     * @return {@link OpenAPI} A custom OpenAPI object is being returned
     */
    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI().info(new Info().title(Constants.NOTIFICATION_SERVICE_OPENAPI_TITLE).version(Constants.OPEN_API_VERSION))
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
