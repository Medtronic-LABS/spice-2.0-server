package com.mdtlabs.coreplatform.offlineservice;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.amazonaws.auth.WebIdentityTokenCredentialsProvider;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;

import com.mdtlabs.coreplatform.offlineservice.common.Constants;

/**
 * <p>
 * This class is a configuration class to handle the s3 operations.
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 */
@Configuration
public class AwsS3Config {

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
    public AmazonS3 getS3Client() {
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
