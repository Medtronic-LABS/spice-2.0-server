package com.mdtlabs.coreplatform.cqlservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * This is the main class for an Cql Service application in Java, which enables various
 * features such as JPA repositories, Feign Clients, and more.
 * </p>
 *
 * @author vishwa-i2it
 * @since May 27, 2024
 */
@EnableFeignClients
@SpringBootApplication
@ComponentScan(value = Constants.PACKAGE_CORE_PLATFORM)
@EnableJpaRepositories(value = Constants.PACKAGE_CORE_PLATFORM)
public class CqlServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(CqlServiceApplication.class, args);
    }

}
