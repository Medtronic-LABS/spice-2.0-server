package com.mdtlabs.coreplatform.cqlservice;

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
 * This class has the test methods for the CqlServiceApplicationTest class.
 * </p>
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CqlServiceApplicationTest {
    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    private CqlServiceApplication cqlServiceApplication;

    @Autowired
    private ConfigurableApplicationContext context;

    @Test
    void main() {
        try (MockedStatic<CqlServiceApplication> cqlApplicationMockedStatic = Mockito
                .mockStatic(CqlServiceApplication.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            cqlApplicationMockedStatic.when(() -> CqlServiceApplication.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(CqlServiceApplication.class, ARGS))
                    .thenReturn(context);

            // when
            CqlServiceApplication.main(ARGS);

            //then
            cqlApplicationMockedStatic.verify(() -> CqlServiceApplication.main(ARGS), times(1));
            springStatic.verify(() -> SpringApplication.run(CqlServiceApplication.class, ARGS), times(1));
        }
    }
}
