package com.mdtlabs.coreplatform.spiceservice.inappanalytics.controller;

import java.io.IOException;
import java.security.GeneralSecurityException;

import io.minio.errors.MinioException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.inappanalytics.service.InAppAnalyticsService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * Controller for in-app analytics related operations.
 * <p>
 * This controller provides functionality for uploading files to Amazon S3 as part of in-app analytics.
 * It leverages the {@link InAppAnalyticsService} for the actual upload process.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 14, 2024
 */
@RestController
@RequestMapping("in-app-analytics")
public class InAppAnalyticsController {

    private final InAppAnalyticsService inAppAnalyticsService;

    public InAppAnalyticsController(InAppAnalyticsService inAppAnalyticsService) {
        this.inAppAnalyticsService = inAppAnalyticsService;
    }

    /**
     * Endpoint for uploading a file to Amazon S3.
     * <p>
     * This method accepts a file as a {@link MultipartFile} via a POST request and uploads it to Amazon S3.
     * It delegates the upload process to {@link InAppAnalyticsService}. Upon successful upload, it returns
     * a {@link SuccessResponse} containing the URL of the uploaded file in the S3 bucket.
     * </p>
     *
     * @param multipartFile The file to be uploaded to Amazon S3.
     * @return A {@link SuccessResponse} with the URL of the uploaded file in the S3 bucket.
     */
    @PostMapping("/upload-file")
    public SuccessResponse uploadFileInS3(@RequestParam("file") MultipartFile multipartFile) {
        try {
            inAppAnalyticsService.uploadFileInS3(multipartFile);
            return new SuccessResponse(SuccessCode.UPLOAD_FILE, HttpStatus.CREATED);
        } catch (IOException | MinioException | GeneralSecurityException iOException) {
            Logger.logError(iOException);
            throw new SpiceValidation(1514, iOException.getMessage());
        }
    }

}
