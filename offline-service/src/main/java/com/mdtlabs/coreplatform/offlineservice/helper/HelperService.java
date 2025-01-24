package com.mdtlabs.coreplatform.offlineservice.helper;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.offlineservice.AwsS3Config;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;

import io.minio.MinioClient;
import io.minio.PutObjectArgs;
import io.minio.errors.MinioException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * <p>
 * This class is a service class to perform common operations
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 */
@Service
public class HelperService {

    @Value("${cloud.aws.s3.bucket.name}")
    private String s3Bucket;

    @Value("${cloud.aws.s3.bucket.consent-form-folder}")
    private String consentFormBucketFolder;

    @Value("${app.is-minio-server-enable}")
    private boolean isMinioServerEnable;

    @Value("${application.minio-bucket.name}")
    String minioBucketName;

    @Value("${cloud.minio.credentials.console-url}")
    String minioConsoleUrl;

    AwsS3Config awsS3Config;

    MinioClient minioClient;

    public HelperService(AwsS3Config awsS3Config, MinioClient minioClient){
        this.awsS3Config = awsS3Config;
        this.minioClient = minioClient;
    }

    /**
     * Upload the file into s3
     *
     * @param path - S3 folder path
     * @param data - Content of the file
     */
    public void uploadFileToS3(String path, File data) {
        try {
            if (isMinioServerEnable) {
                Logger.logInfo(StringUtil.concatString("Upload response file to Minio: ", path));
                FileInputStream inputStream = new FileInputStream(data);
                PutObjectArgs putObjectArgs = PutObjectArgs.builder().bucket(minioBucketName)
                        .object(path).stream(inputStream, 1567, -1).build();
                minioClient.putObject(putObjectArgs);
                Files.delete(data.toPath());
            } else {
                Logger.logInfo(StringUtil.concatString("Upload response file to S3: ", path));
                awsS3Config.getS3Client().putObject(s3Bucket, path, data);
                Files.delete(data.toPath());
            }
        } catch (IOException | MinioException | GeneralSecurityException e) {
            throw new SpiceValidation(1008, e.getMessage());
        }
    }

    /**
     * Upload consent form file into s3
     *
     * @param file - Signature file
     * @return path
     */
    public CompletableFuture<String> uploadConsentFormFileToS3(MultipartFile file) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                File fileObj = convertMultipartFileToFile(file);
                String fileName = StringUtil.concatString(consentFormBucketFolder, file.getOriginalFilename());
                String location;
                if (isMinioServerEnable) {
                    FileInputStream inputStream = new FileInputStream(fileObj);
                    PutObjectArgs putObjectArgs = PutObjectArgs.builder().bucket(minioBucketName)
                            .object(fileName).stream(inputStream, 1567, -1).build();
                    minioClient.putObject(putObjectArgs);
                    location = StringUtil.concatString(minioConsoleUrl, Constants.FORWARD_SLASH, Constants.BROWSER,
                            Constants.FORWARD_SLASH, minioBucketName, fileName);
                    Files.delete(fileObj.toPath());
                    return location;
                } else {
                    awsS3Config.getS3Client().putObject(s3Bucket, fileName, fileObj);
                    location = awsS3Config.getS3Client().getUrl(s3Bucket, fileName).toString();
                    Files.delete(fileObj.toPath());
                    return location;
                }
            } catch (IOException | MinioException | GeneralSecurityException e) {
                throw new SpiceValidation(1008, e.getMessage());
            }
        });
    }

    /**
     * To convert the multipart file to file object.
     *
     * @param file - multipart file
     * @return File
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

    /**
     * Get the hash string of the input data
     *
     * @param data - Input data
     * @return - Hash string
     */
    public String getHashString(String data) {
        try {
            MessageDigest messageDigest = MessageDigest.getInstance(Constants.HASH_ALGORITHM_SHA_256);
            byte[] hashBytes = messageDigest.digest(data.getBytes());
            return new BigInteger(Constants.ONE, hashBytes).toString(Constants.SIXTEENTH);
        } catch (NoSuchAlgorithmException e) {
            throw new SpiceValidation();
        }
    }
}
