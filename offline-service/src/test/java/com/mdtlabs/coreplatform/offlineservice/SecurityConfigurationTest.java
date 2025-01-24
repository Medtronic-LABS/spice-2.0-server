package com.mdtlabs.coreplatform.offlineservice;

import javax.sql.DataSource;

import jakarta.servlet.Filter;

import io.swagger.v3.oas.models.OpenAPI;
import net.javacrumbs.shedlock.core.LockProvider;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfigurationSource;

import static org.mockito.Mockito.doNothing;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.AuthenticationFilter;

/**
 * <p>
 * This class has the test methods for Security configuration class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SecurityConfigurationTest {

    @InjectMocks
    SecurityConfiguration securityConfiguration;

    @Mock
    private HttpSecurity httpSecurity;

    @Mock
    private AuthenticationFilter authenticationFilter;

    @Test
    void corsConfigurationSource() {
        //then
        CorsConfigurationSource corsConfigurationSource = securityConfiguration.corsConfigurationSource();
        Assertions.assertNotNull(corsConfigurationSource);
    }

    @Test
    void customOpenAPI() {
        //then
        OpenAPI openAPI = securityConfiguration.customOpenAPI();
        Assertions.assertNotNull(openAPI);
    }

    @Test
    void gzipDecompressingFilter() {
        //then
        FilterRegistrationBean<GzipDecompressingFilter>  response = securityConfiguration.gzipDecompressingFilter();
        Assertions.assertNotNull(response);
    }

    @Test
    void lockProvider() {
        //given
        DataSource mockDataSource = mock(DataSource.class);
        JdbcTemplate mockJdbcTemplate = new JdbcTemplate(mockDataSource);

        //then
        LockProvider response = securityConfiguration.lockProvider(mockJdbcTemplate);
        Assertions.assertNotNull(response);
    }


    @BeforeEach
    public void setup() {
        try {
            when(httpSecurity.addFilterBefore(any(Filter.class), any(Class.class))).thenReturn(httpSecurity);
            when(httpSecurity.exceptionHandling(any())).thenReturn(httpSecurity);
            when(httpSecurity.sessionManagement(any())).thenReturn(httpSecurity);
            when(httpSecurity.csrf(any())).thenReturn(httpSecurity);
            when(httpSecurity.httpBasic(any())).thenReturn(httpSecurity);
            when(httpSecurity.cors(any())).thenReturn(httpSecurity);
            when(httpSecurity.authorizeHttpRequests(any())).thenReturn(httpSecurity);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    void testFilterChain() throws Exception {
        //when
        doNothing().when(authenticationFilter).setServiceName(any());

        //then
        securityConfiguration.filterChain(httpSecurity, authenticationFilter);
        verify(httpSecurity, times(1)).cors(any());
        verify(httpSecurity, times(1)).authorizeHttpRequests(any());
        verify(httpSecurity, times(1)).addFilterBefore(authenticationFilter, UsernamePasswordAuthenticationFilter.class);
        verify(httpSecurity, times(1)).exceptionHandling(any());
        verify(httpSecurity, times(1)).sessionManagement(any());
        verify(httpSecurity, times(1)).csrf(any());
        verify(httpSecurity, times(1)).httpBasic(any());
        verify(authenticationFilter, times(1)).setServiceName(any());
        verify(httpSecurity, times(1)).build();
    }
}
