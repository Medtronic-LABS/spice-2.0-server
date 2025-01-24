package com.mdtlabs.coreplatform.offlineservice;

import java.io.IOException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;

/**
 * <p>
 * GzipDecompressingFilterTest class used to test all possible positive
 * and negative cases for all methods and conditions used in GzipDecompressingFilter class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on July 2, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class GzipDecompressingFilterTest {
    @InjectMocks
    GzipDecompressingFilter gzipDecompressingFilter;

    @Test
    void doFilterWithException() {
        MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
        MockHttpServletResponse httpServletResponse = new MockHttpServletResponse();
        MockFilterChain mockFilterChain = new MockFilterChain();
        httpServletRequest.addHeader(Constants.CONTENT_ENCODING_HEADER, Constants.GZIP);
        Assertions.assertThrows(ServicesException.class, () -> gzipDecompressingFilter.doFilter(httpServletRequest, httpServletResponse, mockFilterChain));
    }

    @Test
    void doFilterWithoutException() throws ServletException, IOException {
        HttpServletRequest mockRequest = Mockito.mock(HttpServletRequest.class);
        HttpServletResponse mockResponse = Mockito.mock(HttpServletResponse.class);
        FilterChain mockChain = Mockito.mock(FilterChain.class);
        when(mockRequest.getHeader(Constants.CONTENT_ENCODING_HEADER)).thenReturn(Constants.TYPE);
        gzipDecompressingFilter.doFilter(mockRequest, mockResponse, mockChain);
        verify(mockChain).doFilter(mockRequest, mockResponse);
        verifyNoMoreInteractions(mockChain);
    }
}