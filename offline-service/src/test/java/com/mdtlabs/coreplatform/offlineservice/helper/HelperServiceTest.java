package com.mdtlabs.coreplatform.offlineservice.helper;

import com.amazonaws.services.s3.AmazonS3;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.offlineservice.AwsS3Config;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * <p>
 * HelperServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in HelperServiceTest class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HelperServiceTest {

    @InjectMocks
    HelperService helperService;

    @Mock
    AwsS3Config awsS3Config;

    @Mock
    MultipartFile multipartFile;

    @Mock
    AmazonS3 amazonS3;

    @Test
    void uploadFileToS3() {
        //given
        String fileName = StringUtil.concatString(TestConstants.REQUEST_ID, Constants.FILE_EXTENSION_JSON);
        File file = new File(fileName);
        ReflectionTestUtils.setField(helperService, TestConstants.IS_MINIO_SERVER_ENABLE, false);
        ReflectionTestUtils.setField(helperService, TestConstants.S3_BUCKET_KEY, TestConstants.S3_BUCKET);
        AmazonS3 s3Client = mock(AmazonS3.class);
        MockedStatic<Files> files = mockStatic(Files.class);

        //when
        files.when(() -> Files.delete(file.toPath())).thenAnswer(invocation -> null);
        when(awsS3Config.getS3Client()).thenReturn(s3Client);

        //then
        helperService.uploadFileToS3(fileName, file);
        files.close();
        verify(awsS3Config, atLeastOnce()).getS3Client();
    }

    @Test
    void getHashString() {
        //given
        String data = TestConstants.ENCRYPTED_PASSWORD;

        //then
        String response = helperService.getHashString(data);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHashStringWithException() throws NoSuchAlgorithmException {
        //given
        String data = TestConstants.ENCRYPTED_PASSWORD;
        MockedStatic<MessageDigest> messageDigest = mockStatic(MessageDigest.class);

        //when
        when(MessageDigest.getInstance(Constants.HASH_ALGORITHM_SHA_256)).thenThrow(NoSuchAlgorithmException.class);

        //then
        assertThrows(SpiceValidation.class, () -> helperService.getHashString(data));
        messageDigest.close();
    }

    @Test
    void uploadConsentFormFileToS3() throws IOException, URISyntaxException {
        //given
        ReflectionTestUtils.setField(helperService, "consentFormBucketFolder", TestConstants.CONSENT_FORM_FOLDER);
        byte[] fileContent = "".getBytes();
        URI uri = new URI(TestConstants.URL + TestConstants.CONSENT_FORM_FOLDER + TestConstants.TEST_PDF);
        URL s3Url = uri.toURL();
        MockedStatic<StringUtil> stringUtil = mockStatic(StringUtil.class);

        //when
        when(multipartFile.getOriginalFilename()).thenReturn(TestConstants.TEST_PDF);
        when(multipartFile.getBytes()).thenReturn(fileContent);
        stringUtil.when(StringUtil :: concatString).thenReturn(null + TestConstants.TEST_PDF);

        when(awsS3Config.getS3Client()).thenReturn(amazonS3);
        when(amazonS3.getUrl(anyString(), anyString())).thenReturn(s3Url);

        //then
        Assertions.assertNotNull(helperService.uploadConsentFormFileToS3(multipartFile));
        stringUtil.close();
    }

}
