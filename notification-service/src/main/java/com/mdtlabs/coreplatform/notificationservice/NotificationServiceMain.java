package com.mdtlabs.coreplatform.notificationservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.redis.RedisRepositoriesAutoConfiguration;
import org.springframework.boot.autoconfigure.security.servlet.UserDetailsServiceAutoConfiguration;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableScheduling;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

import net.javacrumbs.shedlock.spring.annotation.EnableSchedulerLock;

/**
 * <p>
 * NotificationServiceMain is the main class for a Spring Boot application that enables scheduling,
 * and Feign clients.
 * </p>
 *
 * @author Vigneshkumar created on Jun 30, 2022.
 */
@SpringBootApplication(exclude = {UserDetailsServiceAutoConfiguration.class,
        RedisRepositoriesAutoConfiguration.class})
@ComponentScan(value = Constants.PACKAGE_CORE_PLATFORM)
@EnableScheduling
@EnableFeignClients
@EnableSchedulerLock(defaultLockAtMostFor = "${app.shedlock.sms.start}")
public class NotificationServiceMain {

    /**
     * <p>
     * This is the main function that runs the NotificationServiceApplication class in a Spring Boot application.
     * </p>
     *
     * @param args {@link String} Argument array to be passed is given
     */
    public static void main(String[] args) {
        SpringApplication.run(NotificationServiceMain.class, args);
    }
}
