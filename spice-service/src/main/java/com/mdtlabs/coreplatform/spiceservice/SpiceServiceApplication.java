package com.mdtlabs.coreplatform.spiceservice;

import org.modelmapper.ModelMapper;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

@SpringBootApplication
@EnableFeignClients
@ComponentScan(Constants.PACKAGE_CORE_PLATFORM)
public class SpiceServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(SpiceServiceApplication.class, args);
    }

    /**
     * It used to create the ModelMapper object when it was autowired by any other class.
     *
     * @return ModelMapper object.
     */
    @Bean
    public ModelMapper modelMapper() {
        return new ModelMapper();
    }

}
