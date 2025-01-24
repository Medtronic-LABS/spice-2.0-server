package com.mdtlabs.coreplatform.spiceservice.inappanalytics.service;

import java.io.IOException;
import java.security.GeneralSecurityException;

import io.minio.errors.MinioException;
import org.springframework.web.multipart.MultipartFile;

/**
 * Service interface for handling file uploads to Amazon S3 within the in-app analytics context.
 * <p>
 * Provides a method for uploading files received as {@link MultipartFile} to Amazon S3. The method is designed
 * to convert the multipart file into a format suitable for S3 storage and return the URL of the uploaded file
 * in the S3 bucket, facilitating access and integration within the application's analytics features.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 14, 2024
 */
public interface InAppAnalyticsService {

    /**
     * Uploads a file to Amazon S3.
     * <p>
     * Accepts a file as a {@link MultipartFile}, performs necessary preprocessing or conversion as required for S3
     * compatibility, and uploads it to a specified bucket in Amazon S3. The method returns the URL of the uploaded
     * file in the S3 bucket, which can be used for further operations or analytics within the application.
     * </p>
     *
     * @param multipartFile The file to be uploaded, encapsulated as a {@link MultipartFile}.
     * @return The URL of the uploaded file in the S3 bucket, as a {@link String}.
     */
    String uploadFileInS3(MultipartFile multipartFile) throws IOException, MinioException, GeneralSecurityException;
}
