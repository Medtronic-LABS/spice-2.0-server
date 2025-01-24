package com.mdtlabs.coreplatform.adminservice;


import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;

import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.HttpStatusEntryPoint;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;

import com.mdtlabs.coreplatform.commonservice.AuthenticationFilter;
import com.mdtlabs.coreplatform.commonservice.common.Constants;

@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

    @Value("${app.allowed-origins}")
    private String allowedOrigins;

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
        authenticationFilter.setServiceName(Constants.ADMIN_SERVICE);
        http.cors(cors -> cors.configurationSource(r -> {
                    CorsConfiguration configuration = new CorsConfiguration();
                    configuration.setAllowedOrigins(List.of(allowedOrigins.split(Constants.COMMA)));
                    configuration.setAllowedMethods(List.of(Constants.ASTERISK_SYMBOL));
                    configuration.setAllowedHeaders(List.of(Constants.ASTERISK_SYMBOL));
                    configuration.setAllowCredentials(Boolean.TRUE);
                    return configuration;
                }))
                .authorizeHttpRequests((requests) -> requests.anyRequest().authenticated())
                .addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling((exception) -> exception
                        .authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED))).sessionManagement((session) -> {
                            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS);
                        }
                ).csrf((csrf) -> {
                    csrf.disable();
                }).httpBasic(Customizer.withDefaults());
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
        return new OpenAPI().info(new Info().title(Constants.ADMIN_SERVICE_OPENAPI_TITLE).version(Constants.OPEN_API_VERSION))
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
