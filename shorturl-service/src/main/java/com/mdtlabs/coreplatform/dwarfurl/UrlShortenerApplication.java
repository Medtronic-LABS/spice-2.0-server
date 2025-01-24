package com.mdtlabs.coreplatform.dwarfurl;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * <p>
 * The UrlShortenerApplication class is the entry point for the URL Shortener application.
 * </p>
 * @author JohnKennedy Created on 04 sep 2024
 */
@SpringBootApplication
public class UrlShortenerApplication {

    /**
     * Main method to run the Spring Boot application.
     *
     * @param args command line arguments
     */
    public static void main(String[] args) {
        SpringApplication.run(UrlShortenerApplication.class, args);
    }
}