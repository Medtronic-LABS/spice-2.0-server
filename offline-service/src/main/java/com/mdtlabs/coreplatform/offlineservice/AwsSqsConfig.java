package com.mdtlabs.coreplatform.offlineservice;

import io.awspring.cloud.sqs.config.SqsMessageListenerContainerFactory;
import io.awspring.cloud.sqs.listener.acknowledgement.handler.AcknowledgementMode;
import io.awspring.cloud.sqs.operations.SqsTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.sqs.SqsAsyncClient;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;

/**
 * <p>
 * This class is a configuration class to handle the sqs operations.
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 */
@Configuration
@ConditionalOnProperty(name = "app.is-aws-sqs-enabled", havingValue = "true", matchIfMissing = false)
public class AwsSqsConfig {

    @Value("${cloud.aws.region.static}")
    private String region;

    @Value("${cloud.aws.environment}")
    private String awsEnvironment;

    /**
     * Generates an Amazon SQS client based on the environment.
     * For EKS, it uses WebIdentityTokenCredentialsProvider.
     * For EC2, it uses region from the application properties.
     *
     * @return The Amazon S!S client.
     */
    @Bean
    public SqsAsyncClient sqsAsyncClient() {
        if (Constants.AWS_ENVIRONMENT_EKS.equalsIgnoreCase(awsEnvironment)) {
            return SqsAsyncClient.builder()
                    .region(Region.of(region))
                    .credentialsProvider(DefaultCredentialsProvider.create())
                    .build();
        }
        return SqsAsyncClient.builder()
                .region(Region.of(region))
                .credentialsProvider(DefaultCredentialsProvider.create())
                .build();
    }

    @Bean
    public SqsTemplate sqsTemplate(SqsAsyncClient sqsAsyncClient) {
        return SqsTemplate.builder().sqsAsyncClient(sqsAsyncClient).build();
    }

    @Bean
    SqsMessageListenerContainerFactory<Object> defaultSqsListenerContainerFactory(
            SqsAsyncClient sqsAsyncClient) {
        Logger.logInfo("Sqs queue listener container factory enabled.");
        return SqsMessageListenerContainerFactory.builder()
                .configure(options -> options.maxConcurrentMessages(Constants.TEN))
                .configure(options -> options.acknowledgementMode(AcknowledgementMode.ALWAYS))
                .sqsAsyncClient(sqsAsyncClient).build();
    }
}
