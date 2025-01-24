package com.mdtlabs.coreplatform.notificationservice;

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
 *   This class has the test methods for the GatewayApplication class.
 * </p>
 *
 * @author Divya S
 */
@ExtendWith(MockitoExtension.class)
class NotificationServiceMainTest {

    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    NotificationServiceMain notificationServiceMain;

    @Autowired
    ConfigurableApplicationContext context;

    @Test
    void main() {
        try (MockedStatic<NotificationServiceMain> gatewayApplicationMockedStatic = Mockito
                .mockStatic(NotificationServiceMain.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            gatewayApplicationMockedStatic.when(() -> NotificationServiceMain.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(NotificationServiceMain.class, ARGS))
                    .thenReturn(context);

            // when
            NotificationServiceMain.main(ARGS);

            //then
            gatewayApplicationMockedStatic.verify(
                    () -> NotificationServiceMain.main(ARGS),
                    times(1));
            springStatic.verify(
                    () -> SpringApplication.run(NotificationServiceMain.class, ARGS),
                    times(1));
        }
    }
}