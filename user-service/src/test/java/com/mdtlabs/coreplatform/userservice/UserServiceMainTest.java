package com.mdtlabs.coreplatform.userservice;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import static org.mockito.Mockito.times;

/**
 * <p>
 *   This class has the test methods for the User service main class.
 * </p>
 *
 * @author Divya S created on March 03, 2023
 */
@ExtendWith(MockitoExtension.class)
class UserServiceMainTest {
    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    UserserviceApplication userServiceMain;

    @Autowired
    ConfigurableApplicationContext context;

    @Test
    void testMain() {
        try (MockedStatic<UserserviceApplication> userServiceMainMockedStatic = Mockito.mockStatic(UserserviceApplication.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            userServiceMainMockedStatic.when(() -> UserserviceApplication.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(UserserviceApplication.class, ARGS))
                    .thenReturn(context);

            // when
            UserserviceApplication.main(ARGS);

            //then
            userServiceMainMockedStatic.verify(
                    () -> UserserviceApplication.main(ARGS),
                    times(1));
            springStatic.verify(
                    () -> SpringApplication.run(UserserviceApplication.class, ARGS),
                    times(1));
        }
    }

    @Test
    void modelMapper() {
        ModelMapper response = userServiceMain.modelMapper();
        Assertions.assertNotNull(response);
    }


}
