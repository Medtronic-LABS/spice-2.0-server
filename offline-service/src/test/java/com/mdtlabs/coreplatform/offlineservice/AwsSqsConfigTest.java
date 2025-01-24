package com.mdtlabs.coreplatform.offlineservice;

import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import io.awspring.cloud.sqs.config.SqsMessageListenerContainerFactory;
import io.awspring.cloud.sqs.operations.SqsTemplate;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;
import software.amazon.awssdk.services.sqs.SqsAsyncClient;

/**
 * <p>
 * This class has the test methods for Aws SQS Config class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AwsSqsConfigTest {

    @InjectMocks
    AwsSqsConfig awsSqsConfig;


    @Test
    void sqsAsyncClientForEks() {
        //given
        ReflectionTestUtils.setField(awsSqsConfig, TestConstants.AWS_REGION_KEY, TestConstants.AWS_REGION);
        ReflectionTestUtils.setField(awsSqsConfig, TestConstants.AWS_ENVIRONMENT_KEY, TestConstants.EKS);

        //then
        SqsAsyncClient asyncClient = awsSqsConfig.sqsAsyncClient();
        Assertions.assertNotNull(asyncClient);
    }

    @Test
    void sqsAsyncClient() {
        //given
        ReflectionTestUtils.setField(awsSqsConfig, TestConstants.AWS_REGION_KEY, TestConstants.AWS_REGION);
        ReflectionTestUtils.setField(awsSqsConfig, TestConstants.AWS_ENVIRONMENT_KEY, TestConstants.DEV);

        //then
        SqsAsyncClient asyncClient = awsSqsConfig.sqsAsyncClient();
        Assertions.assertNotNull(asyncClient);
    }

    @Test
    void sqsTemplate() {
        //given
        ReflectionTestUtils.setField(awsSqsConfig, TestConstants.AWS_REGION_KEY, TestConstants.AWS_REGION);
        SqsAsyncClient sqsAsyncClient = awsSqsConfig.sqsAsyncClient();

        //then
        SqsTemplate template = awsSqsConfig.sqsTemplate(sqsAsyncClient);
        Assertions.assertNotNull(template);
    }

    @Test
    void defaultSqsListenerContainerFactory() {
        //given
        ReflectionTestUtils.setField(awsSqsConfig, TestConstants.AWS_REGION_KEY, TestConstants.AWS_REGION);
        SqsAsyncClient sqsAsyncClient = awsSqsConfig.sqsAsyncClient();

        //then
        SqsMessageListenerContainerFactory<Object> object = awsSqsConfig.defaultSqsListenerContainerFactory(sqsAsyncClient);
        Assertions.assertNotNull(object);
    }
}
