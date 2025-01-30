package com.mdtlabs.coreplatform.spiceservice;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.amazonaws.auth.WebIdentityTokenCredentialsProvider;

import com.mdtlabs.coreplatform.spiceservice.common.Constants;

/**
 * <p>
 * A configuration class that provides a bean for the Amazon S3 client.
 * The region for the Amazon S3 client is set using the value from the application.properties file.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 14, 2024
 */
@Configuration
public class AwsConfig {

    @Value("${cloud.aws.region.static}")
    private String region;

    @Value("${cloud.aws.environment}")
    private String awsEnvironment;

    /**
     * Generates an Amazon S3 client based on the environment.
     * For EKS, it uses WebIdentityTokenCredentialsProvider.
     * For EC2, it uses region from the application properties.
     *
     * @return The Amazon S3 client.
     */
    @Bean
    public AmazonS3 amazonS3Client() {
        if (Constants.AWS_ENVIRONMENT_EKS.equalsIgnoreCase(awsEnvironment)) {
            return AmazonS3ClientBuilder.standard()
                    .withRegion(region)
                    .withCredentials(WebIdentityTokenCredentialsProvider.create())
                    .build();
        }
            return AmazonS3ClientBuilder.standard()
                    .withRegion(region)
                    .build();
    }
}