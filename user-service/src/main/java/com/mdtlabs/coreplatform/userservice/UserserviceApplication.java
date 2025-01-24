package com.mdtlabs.coreplatform.userservice;

import org.modelmapper.ModelMapper;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * This is the main class for a Spring Boot application that runs a user service with Feign clients
 * enabled.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 20224
 */
@SpringBootApplication
@EnableFeignClients
@ComponentScan(Constants.PACKAGE_CORE_PLATFORM)
public class UserserviceApplication {

    /**
     * <p>
     * This is the main function that runs the UserServiceMain class in a Spring Boot application.
     * </p>
     *
     * @param args {@link String} Argument array to be passed is given
     */
    public static void main(String[] args) {
        SpringApplication.run(UserserviceApplication.class, args);
    }

	@Bean
	public ModelMapper modelMapper() {
		return new ModelMapper();
	}

}
