package com.mdtlabs.coreplatform.commonservice;

import static org.mockito.Mockito.times;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import com.mdtlabs.coreplatform.commonservice.common.TestConstants;

@ExtendWith(MockitoExtension.class)
class CommonServiceApplicationTests {

	private static final String ARG = TestConstants.EMPTY_STRING;
	private static final String[] ARGS = new String[]{ARG};

	@Autowired
	ConfigurableApplicationContext context;

	@InjectMocks
	CommonServiceApplication commonServiceApplication;

	@Test
	void main() {
		try (MockedStatic<CommonServiceApplication> spiceServiceApplicationMock = Mockito.mockStatic(CommonServiceApplication.class);
			 MockedStatic<SpringApplication> springStatic = Mockito.mockStatic(
					 SpringApplication.class)) {
			spiceServiceApplicationMock.when(() -> CommonServiceApplication.main(ARGS))
					.thenCallRealMethod();
			springStatic.when(() -> SpringApplication.run(CommonServiceApplication.class, ARGS))
					.thenReturn(context);

			// when
			CommonServiceApplication.main(ARGS);

			//then
			spiceServiceApplicationMock.verify(
					() -> CommonServiceApplication.main(ARGS),
					times(1));
			springStatic.verify(
					() -> SpringApplication.run(CommonServiceApplication.class, ARGS),
					times(1));
		}
	}

}
