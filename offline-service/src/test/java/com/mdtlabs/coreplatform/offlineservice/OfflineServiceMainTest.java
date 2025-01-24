package com.mdtlabs.coreplatform.offlineservice;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.Mockito.times;

/**
 * <p>
 * This class has the test methods for the offline service class.
 * </p>
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class OfflineServiceMainTest {
    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    private OfflineServiceMain offlineServiceMain;

    @Autowired
    private ConfigurableApplicationContext context;

    @Test
    void main() {
        try (MockedStatic<OfflineServiceMain> offlineApplicationMockedStatic = Mockito
                .mockStatic(OfflineServiceMain.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            offlineApplicationMockedStatic.when(() -> OfflineServiceMain.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(OfflineServiceMain.class, ARGS))
                    .thenReturn(context);

            // when
            OfflineServiceMain.main(ARGS);

            //then
            offlineApplicationMockedStatic.verify(() -> OfflineServiceMain.main(ARGS), times(1));
            springStatic.verify(() -> SpringApplication.run(OfflineServiceMain.class, ARGS), times(1));
        }
    }

}