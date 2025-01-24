package com.mdtlabs.coreplatform.spiceservice.inappanalytics.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.security.GeneralSecurityException;
import java.util.Objects;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.PutObjectRequest;
import io.minio.MinioClient;
import io.minio.PutObjectArgs;
import io.minio.errors.MinioException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.inappanalytics.service.InAppAnalyticsService;

/**
 * <p>
 * Class provides the implementation for the EnapAnalyticsService.
 * It contains methods to upload a file to Amazon S3 and convert a MultipartFile into a File object.
 * It uses the AmazonS3 client to interact with the S3 service.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 14, 2024
 */
@Service
public class InAppAnalyticsServiceImpl implements InAppAnalyticsService {

    @Value("${application.bucket.name}")
    private String bucketName;

    @Value("${application.bucket.in-app-analytics-folder}")
    private String inAppAnalyticsBucket;

    @Value("${app.is-minio-server-enable}")
    private boolean isMinioServerEnable;

    @Value("${application.bucket.name}")
    String minioBucketName;

    @Value("${cloud.minio.credentials.console-url}")
    String minioConsoleUrl;

    private final AmazonS3 s3Client;

    private final MinioClient minioClient;

    public InAppAnalyticsServiceImpl(AmazonS3 s3Client, MinioClient minioClient) {
        this.s3Client = s3Client;
        this.minioClient = minioClient;
    }

    /**
     * {@inheritDoc}
     */
    public String uploadFileInS3(MultipartFile file) throws IOException, MinioException, GeneralSecurityException {
        File fileObj = convertMultipartFileToFile(file);
        String fileName = inAppAnalyticsBucket.concat(fileObj.getName());
        String location;
        if (isMinioServerEnable) {
            FileInputStream inputStream = new FileInputStream(fileObj);
            PutObjectArgs putObjectArgs = PutObjectArgs.builder().bucket(minioBucketName)
                    .object(fileName).stream(inputStream, 1567, -1).build();
            minioClient.putObject(putObjectArgs);
            location = StringUtil.concatString(minioConsoleUrl, Constants.FORWARD_SLASH, Constants.BROWSER,
                    Constants.FORWARD_SLASH, minioBucketName, fileName);
            Files.delete(fileObj.toPath());
        } else {
            s3Client.putObject(new PutObjectRequest(bucketName, fileName, fileObj));
            location = s3Client.getUrl(bucketName, fileName).toString();
            Files.delete(fileObj.toPath());
        }
        return location;
    }

    /**
     * Converts a MultipartFile into a File object.
     *
     * @param file The MultipartFile to be converted.
     * @return The converted File object.
     * @throws SpiceValidation If an IOException occurs during the conversion process.
     */
    private File convertMultipartFileToFile(MultipartFile file) {
        File convertedFile = new File(Objects.requireNonNull(file.getOriginalFilename()));
        try (FileOutputStream fos = new FileOutputStream(convertedFile)) {
            fos.write(file.getBytes());
        } catch (IOException e) {
            Logger.logError(e);
            throw new SpiceValidation(1514, e.getMessage());
        }
        return convertedFile;
    }
}
