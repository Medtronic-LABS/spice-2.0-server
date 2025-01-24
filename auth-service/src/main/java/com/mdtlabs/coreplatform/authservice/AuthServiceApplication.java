package com.mdtlabs.coreplatform.authservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * This is the main class for an authentication Service application in Java, which enables various
 * features such as JPA repositories, Feign Clients, and more.
 * </p>
 *
 * @author Vigneshkumar
 * @since Jun 30, 2022
 */
@SpringBootApplication
@EnableFeignClients
@ComponentScan(value = Constants.PACKAGE_CORE_PLATFORM)
@EnableJpaRepositories(value = Constants.PACKAGE_CORE_PLATFORM)
public class AuthServiceApplication {

    /**
     * <p>
     * This is the main function that runs the AuthApplication class in a Spring Boot application.
     * </p>
     *
     * @param args {@link String} Argument array to be passed is given
     */
    public static void main(String[] args) {
        SpringApplication.run(AuthServiceApplication.class, args);
    }

}
