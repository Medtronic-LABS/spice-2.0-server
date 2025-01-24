package com.mdtlabs.coreplatform.offlineservice;

import com.amazonaws.services.s3.AmazonS3;
import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * <p>
 * This class has the test methods for Aws S3 Config class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AwsS3ConfigTest {

    @InjectMocks
    AwsS3Config awsS3Config;

    @Test
    void getS3ClientForEks() {
        //given
        ReflectionTestUtils.setField(awsS3Config, TestConstants.AWS_REGION_KEY, TestConstants.AWS_REGION);
        ReflectionTestUtils.setField(awsS3Config, TestConstants.AWS_ENVIRONMENT_KEY, TestConstants.EKS);

        //then
        AmazonS3 amazonS3 = awsS3Config.getS3Client();
        Assertions.assertNotNull(amazonS3);
    }

    @Test
    void getS3Client() {
        //given
        ReflectionTestUtils.setField(awsS3Config, TestConstants.AWS_REGION_KEY, TestConstants.AWS_REGION);
        ReflectionTestUtils.setField(awsS3Config, TestConstants.AWS_ENVIRONMENT_KEY, TestConstants.DEV);

        //then
        AmazonS3 amazonS3 = awsS3Config.getS3Client();
        Assertions.assertNotNull(amazonS3);
    }
}
