package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.authservice.helper.HelperService;
import com.mdtlabs.coreplatform.authservice.repository.RoleRepository;
import com.mdtlabs.coreplatform.authservice.repository.UserRepository;
import com.mdtlabs.coreplatform.authservice.service.UserTokenService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.HttpStatusEntryPoint;
import org.springframework.web.cors.CorsConfiguration;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * In this class is the entry point for user authentication and authorization.
 * </p>
 *
 * @author Prabu
 * @since 16 Sep 2022
 */
@Configuration
@EnableWebSecurity
public class SecurityConfig {

	@Value("${app.allowed-origins}")
	private String allowedOrigins;

	String password = "password";

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public AuthenticationSuccess authenticationSuccess(UserTokenService userTokenService, RoleRepository roleRepository, UserRepository userRepository, RedisTemplate<String, Map<String, List<String>>> redisTemplate) {
        return new AuthenticationSuccess(userTokenService, roleRepository, userRepository, redisTemplate);
    }

    @Bean
    public AuthenticationFailure authenticationFailure() {
        return new AuthenticationFailure();
    }

    @Bean
    public AuthenticationProvider authenticationProvider(UserRepository userRepository, RoleRepository roleRepository) {
        return new AuthenticationProvider(userRepository,roleRepository);
    }

    @Bean
    public LogoutSuccess logoutSuccess(UserTokenService userTokenService, HelperService commonUtil) {
        return new LogoutSuccess(userTokenService, commonUtil );
    }

    /**
     * Configures the security filter chain for the application.
     * This method takes an HttpSecurity object and configures various security settings.
     * It sets up CORS configuration, authorizes HTTP requests, configures form login, exception handling, and logout settings.
     * It also disables CSRF protection.
     *
     * @param request The HttpSecurity object to configure.
     * @return The configured SecurityFilterChain.
     * @throws Exception If an error occurs during configuration.
     */
    @Bean
	public SecurityFilterChain securityFilterChain(HttpSecurity request, UserTokenService userTokenService,
												   RoleRepository roleRepository, UserRepository userRepository,
												   RedisTemplate<String, Map<String, List<String>>> redisTemplate,
												   HelperService commonUtil) throws Exception {
		request.cors(cors -> cors.configurationSource(r -> {
					CorsConfiguration configuration = new CorsConfiguration();
					configuration.setAllowedOrigins(List.of(allowedOrigins.split(Constants.COMMA)));
					configuration.setAllowedMethods(List.of(Constants.ASTERISK_SYMBOL));
					configuration.setAllowedHeaders(List.of(Constants.ASTERISK_SYMBOL));
					configuration.setAllowCredentials(Boolean.TRUE);
					return configuration;
				})).authorizeHttpRequests(
						requests -> requests.requestMatchers("/authenticate").permitAll()
								.requestMatchers(HttpMethod.GET, "/health-check").permitAll()
								.anyRequest().authenticated())
				.formLogin(
						form -> form.loginProcessingUrl("/session")
								.usernameParameter("username")
								.passwordParameter(password)
								.successHandler(authenticationSuccess(userTokenService, roleRepository, userRepository, redisTemplate))
								.failureHandler(authenticationFailure()))
				.exceptionHandling(exception ->
						exception.authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
				.logout(
						logout -> logout.logoutUrl("/logout")
								.deleteCookies("JSESSIONID")
								.invalidateHttpSession(true)
								.logoutSuccessHandler(logoutSuccess(userTokenService, commonUtil)))
				.csrf(AbstractHttpConfigurer::disable); //NOSONAR
		return request.build();
	}

}
