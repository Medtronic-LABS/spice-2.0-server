package com.mdtlabs.coreplatform.commonservice;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.util.ContentCachingRequestWrapper;
import org.springframework.web.util.ContentCachingResponseWrapper;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AuthenticationFilterTest {

    @InjectMocks
    private AuthenticationFilter authenticationFilter;

    @Test
    void doLogApi() throws ServletException, IOException {
        //given
        ContentCachingRequestWrapper requestWrapper = getContentCachingRequestWrapper();
        ContentCachingResponseWrapper responseWrapper = getContentCachingResponseWrapper();
        FilterChain filterChain = new MockFilterChain();

        //then
        authenticationFilter.doLogApi(requestWrapper, responseWrapper, filterChain);
        assertNotNull(getContentCachingRequestWrapper().getRequest());
        assertNotNull(getContentCachingResponseWrapper().getResponse());
        assertNotNull(filterChain);
    }

    private ContentCachingRequestWrapper getContentCachingRequestWrapper() {
        return new ContentCachingRequestWrapper(new MockHttpServletRequest());
    }

    private ContentCachingResponseWrapper getContentCachingResponseWrapper() {
        return new ContentCachingResponseWrapper(new MockHttpServletResponse());
    }

    @Test
    void beforeRequest() throws IOException {
        //given
        ContentCachingRequestWrapper requestWrapper = mock(ContentCachingRequestWrapper.class);
        ContentCachingResponseWrapper responseWrapper = mock(ContentCachingResponseWrapper.class);

        //when
        when(requestWrapper.getContentAsByteArray()).thenReturn(new byte[]{'a','b'});
        when(requestWrapper.getCharacterEncoding()).thenReturn("ISO-8859-1");

        //then
        authenticationFilter.beforeRequest(requestWrapper, responseWrapper);
        verify(requestWrapper, atLeastOnce()).getContentAsByteArray();
    }

    @Test
    void afterRequest() {
        //given
        ContentCachingRequestWrapper requestWrapper = mock(ContentCachingRequestWrapper.class);
        ContentCachingResponseWrapper responseWrapper = mock(ContentCachingResponseWrapper.class);

        //when
        when(requestWrapper.getContentAsByteArray()).thenReturn(new byte[]{'a','b'});
        when(requestWrapper.getContentType()).thenReturn("application/json");
        when(responseWrapper.getContentType()).thenReturn("application/json");
        when(requestWrapper.getCharacterEncoding()).thenReturn("ISO-8859-1");
        when(responseWrapper.getStatus()).thenReturn(200);
        when(responseWrapper.getContentAsByteArray()).thenReturn(new byte[]{'a','b'});
        when(requestWrapper.getRemoteAddr()).thenReturn("aa");
        when(responseWrapper.getCharacterEncoding()).thenReturn("ISO-8859-1");

        //then
        authenticationFilter.afterRequest(requestWrapper, responseWrapper);
        verify(requestWrapper, atLeastOnce()).getContentAsByteArray();
    }
}