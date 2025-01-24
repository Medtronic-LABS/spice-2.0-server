package com.mdtlabs.coreplatform.adminservice;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.ssm.SsmClient;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * AwsConfig has been done here for aws services.
 * </p>
 *
 * @author Nandhakumar Created on 23 Feb 2023
 */
@Configuration
public class AwsSsmConfig {

    @Value("${cloud.aws.region.static}")
    private String regionCloud;

    @Value("${cloud.aws.environment}")
    private String awsEnvironment;

    /**
     * This method is used to generate SsmClient client configuration.
     *
     * @return AmazonS3 - bean
     */
    @Bean
    public SsmClient generateSsmClient() {
        Region region = Region.of(regionCloud);

        if (Constants.AWS_ENVIRONMENT_EKS.equalsIgnoreCase(awsEnvironment)) {
            return SsmClient.builder()
                    .region(region)
                    .credentialsProvider(DefaultCredentialsProvider.create())
                    .build();
        }
        return SsmClient.builder()
                .region(region)
                .build();
    }
}
