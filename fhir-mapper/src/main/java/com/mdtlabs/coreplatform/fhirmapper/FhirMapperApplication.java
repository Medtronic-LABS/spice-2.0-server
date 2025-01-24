package com.mdtlabs.coreplatform.fhirmapper;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * Fhir mapping based operation performed in this service.
 * </p>
 *
 * @author Nandhakumar created on Feb p5, 2024.
 */
@EnableFeignClients
@ComponentScan(value = Constants.PACKAGE_CORE_PLATFORM)
@SpringBootApplication
public class FhirMapperApplication {

    public static void main(String[] args) {
        SpringApplication.run(FhirMapperApplication.class, args);
    }

}
