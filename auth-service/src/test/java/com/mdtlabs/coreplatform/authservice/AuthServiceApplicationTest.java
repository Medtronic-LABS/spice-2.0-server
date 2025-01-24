package com.mdtlabs.coreplatform.authservice;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import static org.mockito.Mockito.times;

/**
 * <p>
 * This class has the test methods for the AuthServiceApplication class.
 * </p>
 *
 * @author Divya S created on Jan 30, 2023
 */
@ExtendWith(MockitoExtension.class)
class AuthServiceApplicationTest {

    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    AuthServiceApplication authServerApplication;

    @Autowired
    ConfigurableApplicationContext context;

    @Test
    void authServerMain() {
        try (MockedStatic<AuthServiceApplication> authServerApplicationMock = Mockito.mockStatic(AuthServiceApplication.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            authServerApplicationMock.when(() -> AuthServiceApplication.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(AuthServiceApplication.class, ARGS))
                    .thenReturn(context);

            // when
            AuthServiceApplication.main(ARGS);

            //then
            authServerApplicationMock.verify(
                    () -> AuthServiceApplication.main(ARGS),
                    times(1));
            springStatic.verify(
                    () -> SpringApplication.run(AuthServiceApplication.class, ARGS),
                    times(1));
        }
    }
}
