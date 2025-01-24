package com.mdtlabs.coreplatform.commonservice;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;

/**
 * <p>
 * Used to access common service based operations
 * </p>
 *
 * @author KarthickMurugaesan created on Jan 11, 2024
 */
@SpringBootApplication
@ComponentScan(value = Constants.PACKAGE_CORE_PLATFORM)
@EnableFeignClients
public class CommonServiceApplication {

	/**
	 * <p>
	 * The main method where the execution starts
	 * </p>
	 *
	 * @param args - argument array to be passed
	 */
	public static void main(String[] args) {
		SpringApplication.run(CommonServiceApplication.class, args);
	}

}
