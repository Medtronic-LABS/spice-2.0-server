package com.mdtlabs.coreplatform.adminservice;

import org.modelmapper.ModelMapper;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * This is the main class for an Admin Service application in Java, which excludes certain
 * auto-configurations, scans specific packages and Feign clients
 * </p>
 *
 * @author Vigneshkumar created on Jun 30, 2022.
 */
@SpringBootApplication
@EnableFeignClients
@ComponentScan(Constants.PACKAGE_CORE_PLATFORM)
public class AdminServiceApplication {

    /**
     * <p>
     * This is the main function that runs the AdminServiceApplication class in a Spring Boot application.
     * </p>
     *
     * @param args {@link String} Argument array to be passed is given
     */
    public static void main(String[] args) {
        SpringApplication.run(AdminServiceApplication.class, args);
    }

	@Bean
	public ModelMapper modelMapper() {
		return new ModelMapper();
	}

}
