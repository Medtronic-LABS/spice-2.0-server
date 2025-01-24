package com.mdtlabs.coreplatform.spiceservice;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import static org.mockito.Mockito.times;

/**
 * <p>
 * This class has the test methods for the SpiceServiceApplication class.
 * </p>
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SpiceServiceApplicationTest {

    private static final String ARG = "";
    private static final String[] ARGS = new String[]{ARG};

    @InjectMocks
    private SpiceServiceApplication spiceServiceApplication;

    @Autowired
    private ConfigurableApplicationContext context;

    @Test
    void main() {
        try (MockedStatic<SpiceServiceApplication> spiceServiceApplicationMockedStatic = Mockito
                .mockStatic(SpiceServiceApplication.class);
             MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
                     SpringApplication.class)) {
            spiceServiceApplicationMockedStatic.when(() -> SpiceServiceApplication.main(ARGS))
                    .thenCallRealMethod();
            springStatic.when(() -> SpringApplication.run(SpiceServiceApplication.class, ARGS))
                    .thenReturn(context);

            // when
            SpiceServiceApplication.main(ARGS);

            //then
            spiceServiceApplicationMockedStatic.verify(() -> SpiceServiceApplication.main(ARGS), times(1));
            springStatic.verify(() -> SpringApplication.run(SpiceServiceApplication.class, ARGS), times(1));
        }
    }

    @Test
    void modelMapper() {
        //then
        ModelMapper response = spiceServiceApplication.modelMapper();
        Assertions.assertNotNull(response);
    }
}
