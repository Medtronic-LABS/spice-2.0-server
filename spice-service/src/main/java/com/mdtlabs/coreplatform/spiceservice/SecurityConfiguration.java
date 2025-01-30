package com.mdtlabs.coreplatform.spiceservice;

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
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.HttpStatusEntryPoint;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;

import com.mdtlabs.coreplatform.commonservice.AuthenticationFilter;
import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * Security configuration for the Spice Service application.
 * <p>
 * This configuration class sets up security filters and rules for the application, including CORS configuration,
 * session management, CSRF protection, and basic authentication. It also configures Swagger API documentation
 * security settings.
 * </p>
 */
@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

    @Value("${app.allowed-origins}")
    private String allowedOrigins;
    
    /**
     * This method is used  to filter request.
     *
     * @param http - http request
     * @return SecurityFilterChain - security filter chain
     * @throws Exception - exception
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http, AuthenticationFilter authenticationFilter
    ) throws Exception {
        authenticationFilter.setServiceName(Constants.SPICE_SERVICE);
        http.cors(cors -> cors.configurationSource(r -> {
                    CorsConfiguration configuration = new CorsConfiguration();
                    configuration.setAllowedOrigins(List.of(allowedOrigins.split(Constants.COMMA)));
                    configuration.setAllowedMethods(List.of(Constants.ASTERISK_SYMBOL));
                    configuration.setAllowedHeaders(List.of(Constants.ASTERISK_SYMBOL));
                    configuration.setAllowCredentials(Boolean.TRUE);
                    return configuration;
                }))
                .authorizeHttpRequests(requests -> requests.anyRequest().authenticated())
                .addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED))).sessionManagement(session ->
                            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)).csrf(AbstractHttpConfigurer::disable).httpBasic(Customizer.withDefaults()); //NOSONAR
        return http.build();
    }

    /**
     * This method is used to define custom definition for api Swagger.
     *
     * @return OpenApi configuration
     */
    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI().info(new Info().title(Constants.SPICE_SERVICE_OPENAPI_TITLE).version(Constants.OPEN_API_VERSION))
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
