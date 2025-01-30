package com.mdtlabs.coreplatform.cqlservice;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
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
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import com.mdtlabs.coreplatform.commonservice.AuthenticationFilter;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

/**
 * <p>
 * SecurityConfiguration class that configures security settings for an application, including
 * authentication and authorization mechanisms, and creates a custom OpenAPI specification
 * for a Cql service API with security schemes and requirements.
 * </p>
 *
 * @author vishwa-i2it Created on 27 May 2024
 */
@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

    /**
     * This method is used to configure cors.
     *
     * @return CorsConfigurationSource - cors configuration
     */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {
        final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        final CorsConfiguration config = new CorsConfiguration();
        config.setAllowCredentials(Boolean.TRUE);
        config.addAllowedOriginPattern("*");
        config.addAllowedHeader("*");
        config.addAllowedMethod(HttpMethod.HEAD);
        config.addAllowedMethod(HttpMethod.GET);
        config.addAllowedMethod(HttpMethod.PUT);
        config.addAllowedMethod(HttpMethod.POST);
        config.addAllowedMethod(HttpMethod.DELETE);
        config.addAllowedMethod(HttpMethod.PATCH);
        config.addAllowedMethod(HttpMethod.OPTIONS);
        source.registerCorsConfiguration(StringUtil.concatString("*", "/", "*"), config);
        return source;
    }

    /**
     * <p>
     * This method is used to configure the security filter chain for HTTP requests, allowing certain methods
     * and endpoints to be accessed without authentication while requiring authentication for all
     * other requests.
     * </p>
     *
     * @param http                 {@link HttpSecurity} It is used to configure security settings for the application.
     *                             It allows you to specify which requests should be authenticated,
     *                             which authentication mechanisms to use, and how to handle exceptions
     *                             related to authentication and authorization
     * @param authenticationFilter {@link SecurityFilterChain} The authenticationFilter is a custom filter
     *                             that handles authentication for incoming requests.
     *                             It is added to the filter chain before the
     *                             UsernamePasswordAuthenticationFilter, which is
     *                             the default filter for handling basic authentication
     * @return {@link SecurityFilterChain} A SecurityFilterChain object is being returned
     * @throws Exception The exception in cors is thrown
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http, AuthenticationFilter authenticationFilter
    ) throws Exception {
        authenticationFilter.setServiceName(Constants.CQL_SERVICE);
        http.cors(Customizer.withDefaults())
                .authorizeHttpRequests(requests -> requests.anyRequest().authenticated())
                .addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
                .sessionManagement(session ->
                            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)).csrf(AbstractHttpConfigurer::disable).httpBasic(Customizer.withDefaults()); //NOSONAR
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
        return new OpenAPI().info(
                        new Info().title(Constants.FHIR_MAPPER_OPENAPI_TITLE).version(Constants.OPEN_API_VERSION))
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
