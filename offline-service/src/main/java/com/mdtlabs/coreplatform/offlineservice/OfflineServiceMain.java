package com.mdtlabs.coreplatform.offlineservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * Offline service operation performed in this service.
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024.
 */
@EnableFeignClients
@SpringBootApplication
@ComponentScan(Constants.PACKAGE_CORE_PLATFORM)
public class OfflineServiceMain {

	public static void main(String[] args) {
		SpringApplication.run(OfflineServiceMain.class, args);
	}

}

