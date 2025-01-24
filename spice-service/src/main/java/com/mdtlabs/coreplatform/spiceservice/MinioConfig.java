package com.mdtlabs.coreplatform.spiceservice;

import io.minio.MinioClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class for setting up MinioClient in the application.
 *
 * <p>
 * This class reads the MinIO server configuration from the application properties
 * file and creates a singleton MinioClient bean that can be used throughout the application
 * to interact with MinIO for object storage operations.
 * </p>
 */
@Configuration
public class MinioConfig {

    @Value("${cloud.minio.credentials.access-name}")
    String accessKey;

    @Value("${cloud.minio.credentials.access-secret}")
    String accessSecret;

    @Value("${cloud.minio.credentials.url}")
    String minioUrl;

    @Bean
    public MinioClient generateMinioClient() {
        return MinioClient.builder().endpoint(minioUrl).credentials(accessKey, accessSecret).build();
    }
}
