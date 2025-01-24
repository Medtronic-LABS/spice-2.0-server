package com.mdtlabs.coreplatform.offlineservice;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import net.javacrumbs.shedlock.core.LockProvider;
import net.javacrumbs.shedlock.provider.jdbctemplate.JdbcTemplateLockProvider;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
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
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import com.mdtlabs.coreplatform.commonservice.AuthenticationFilter;
import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * Authentication has been done here for this service.
 * </p>
 * 
 * @author Vigneshkumar Created on 30 Jun 2022
 */
@Configuration
@EnableWebSecurity
public class SecurityConfiguration {

	@Bean
	public LockProvider lockProvider(JdbcTemplate dataSource) {
		return new JdbcTemplateLockProvider(dataSource);
	}

	/**
	 * This method is used to configure filter registration for GzipDecompressingFilter.
	 *
	 * @return FilterRegistrationBean - filter registration
	 */
	@Bean
	public FilterRegistrationBean<GzipDecompressingFilter> gzipDecompressingFilter() {
		FilterRegistrationBean<GzipDecompressingFilter> registrationBean = new FilterRegistrationBean<>();
		registrationBean.setFilter(new GzipDecompressingFilter());
		registrationBean.addUrlPatterns("/*");
		registrationBean.setOrder(Constants.ONE);
		return registrationBean;
	}
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
		source.registerCorsConfiguration("*" + "/" + "*", config);
		return source;
	}

	/**
	 * This method is used to filter request.
	 *
	 * @param http - http request
	 * @return SecurityFilterChain - security filter chain
	 * @throws Exception - exception
	 */
	@Bean
	public SecurityFilterChain filterChain(HttpSecurity http, AuthenticationFilter authenticationFilter)
			throws Exception {
		authenticationFilter.setServiceName(Constants.OFFLINE_SERVICE);
		http.cors(Customizer.withDefaults()).authorizeHttpRequests(requests -> requests.anyRequest().authenticated())
				.addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class)
				.exceptionHandling(exception -> exception.authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
				.sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
				.csrf(AbstractHttpConfigurer::disable).httpBasic(Customizer.withDefaults()); //NOSONAR
		return http.build();
	}

	/**
	 * This method is used to define custom definition for api Swagger.
	 *
	 * @return OpenApi configuration
	 */
	@Bean
	public OpenAPI customOpenAPI() {
		return new OpenAPI().info(new Info().title(Constants.OFFLINE_SERVICE_OPENAPI_TITLE).version(Constants.OPEN_API_VERSION))
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
