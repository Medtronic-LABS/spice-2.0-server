package com.mdtlabs.coreplatform.authservice.authenticationserver;

import java.io.IOException;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.core.AuthenticationException;

@ExtendWith(MockitoExtension.class)
class AuthenticationFailureTest {

    @InjectMocks
    AuthenticationFailure authenticationFailure;

    @Test
    void onAuthenticationFailure() throws IOException {
        //given
        AuthenticationException authenticationException = new AuthenticationException("exception") {
        };
        HttpServletResponse httpServletResponse = new MockHttpServletResponse();
        HttpServletRequest httpServletRequest = new MockHttpServletRequest();

        //then
        authenticationFailure.onAuthenticationFailure(httpServletRequest, httpServletResponse, authenticationException);
        assertEquals("exception", authenticationException.getMessage());
    }
}
