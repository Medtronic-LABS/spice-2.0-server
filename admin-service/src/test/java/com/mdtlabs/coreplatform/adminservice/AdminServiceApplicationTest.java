package com.mdtlabs.coreplatform.adminservice;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import static org.mockito.Mockito.times;

/**
 * <p>
 * This class has the test methods for the AdminServiceApplication class.
 * </p>
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AdminServiceApplicationTest {
    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    private AdminServiceApplication adminServiceApplication;

    @Autowired
    private ConfigurableApplicationContext context;

    @Test
    void main() {
        try (MockedStatic<AdminServiceApplication> offlineApplicationMockedStatic = Mockito
                .mockStatic(AdminServiceApplication.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            offlineApplicationMockedStatic.when(() -> AdminServiceApplication.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(AdminServiceApplication.class, ARGS))
                    .thenReturn(context);

            // when
            AdminServiceApplication.main(ARGS);

            //then
            offlineApplicationMockedStatic.verify(() -> AdminServiceApplication.main(ARGS), times(1));
            springStatic.verify(() -> SpringApplication.run(AdminServiceApplication.class, ARGS), times(1));
        }
    }
}
