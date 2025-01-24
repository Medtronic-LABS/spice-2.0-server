package com.mdtlabs.coreplatform.spiceservice.prescription.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.security.GeneralSecurityException;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

import io.minio.MinioClient;
import io.minio.PutObjectArgs;
import io.minio.errors.MinioException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType;
import com.mdtlabs.coreplatform.commonservice.common.contexts.SelectedAppTypeContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionPredictionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PrescriptionRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.prescription.service.PrescriptionService;

/**
 * Implementation of the PrescriptionRequestService implementation.
 * This class provides methods to handle medical request-related operations.
 *
 * @author Yogeshwaran Mohan created on Apr 17, 2024
 */
@Service
public class PrescriptionServiceImpl implements PrescriptionService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final AmazonS3 s3Client;

    @Value("${application.bucket.name}")
    private String bucketName;

    @Value("${application.bucket.signature-folder}")
    private String signatureBucket;

    @Value("${app.is-minio-server-enable}")
    private boolean isMinioServerEnable;

    @Value("${application.bucket.name}")
    String minioBucketName;

    @Value("${cloud.minio.credentials.console-url}")
    String minioConsoleUrl;

    MinioClient minioClient;

    public PrescriptionServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, AmazonS3 s3Client, MinioClient minioClient) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.s3Client = s3Client;
        this.minioClient = minioClient;
    }

    /**
     * Endpoint for creating Medication request
     * This method handles HTTP POST requests to Create MedicationRequest.
     *
     * @param prescriptionRequestDTO The list of prescription DTO containing Prescription details to be saved.
     * @return ResponseEntity containing the response DTO indicating the result of the operation.
     * @throws IOException
     */
    @ConfigureAppType
    @Override
    public Map<String, String> createMedicationRequest(PrescriptionRequestDTO prescriptionRequestDTO) throws IOException, GeneralSecurityException, MinioException {
        String signature = uploadSignature(prescriptionRequestDTO.getSignatureFile(), prescriptionRequestDTO.getEncounter().getPatientId());
        prescriptionRequestDTO.setSignature(signature);
        prescriptionRequestDTO.setSignatureFile(null);
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            return fhirServiceApiInterface.updateNcdPrescriptionRequest(CommonUtil.getAuthToken(), CommonUtil.getClient(), prescriptionRequestDTO);
        } else {
            return fhirServiceApiInterface.updatePrescriptionRequest(CommonUtil.getAuthToken(), CommonUtil.getClient(), prescriptionRequestDTO);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @throws IOException
     */
    public String uploadSignature(MultipartFile file, String patientId) throws IOException, MinioException, GeneralSecurityException {
        File fileObj = convertMultipartFileToFile(file);
        Instant instant = Instant.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS")
                .withZone(ZoneId.of(com.mdtlabs.coreplatform.commonservice.common.Constants.TIMEZONE_UTC));
        String timestamp = formatter.format(instant);
        String fileName = StringUtil.concatString(patientId, com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE,
                timestamp, com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE,
                Constants.SIGNATURE_FILE_NAME);
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
     * <p>
     * To convert the multipart file to file object.
     * </p>
     *
     * @param file - multipart file
     * @return File - file
     */
    private File convertMultipartFileToFile(MultipartFile file) {
        File convertedFile = new File(file.getOriginalFilename());
        try (FileOutputStream fos = new FileOutputStream(convertedFile)) {
            fos.write(file.getBytes());
        } catch (IOException e) {
            Logger.logError(e);
            throw new SpiceValidation(1514, e.getMessage());
        }
        return convertedFile;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PrescriptionDTO> getPrescriptionHistory(RequestDTO request) {
        return fhirServiceApiInterface.getPrescriptionHistoryList(CommonUtil.getAuthToken(), CommonUtil.getClient(), CommonUtil.getTenantId(), request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrescriptionHistoryDTO getPrescribedDetails(RequestDTO request) {
        return fhirServiceApiInterface.getPrescribedDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), CommonUtil.getTenantId(), request);
    }

    /**
     * {@inheritDoc}
     */
    @ConfigureAppType
    @Override
    public List<PrescriptionDTO> getPrescriptions(RequestDTO requestDTO) {
        if (Constants.NON_COMMUNITY.equals(SelectedAppTypeContextHolder.get())) {
            return fhirServiceApiInterface.getNcdPrescriptions(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
        } else {
            return fhirServiceApiInterface.getPrescriptions(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removePrescription(PrescriptionRequestDTO prescriptionRequestDTO) {
        fhirServiceApiInterface.removePrescription(CommonUtil.getAuthToken(), CommonUtil.getClient(), CommonUtil.getTenantId(), prescriptionRequestDTO);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, String> updateFillPrescription(PrescriptionRequestDTO prescriptionRequestDTO) {
        return fhirServiceApiInterface.updatePrescriptionDispense(CommonUtil.getAuthToken(), CommonUtil.getClient(), prescriptionRequestDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PrescriptionDTO> getFillPrescriptions(RequestDTO request) {
        return fhirServiceApiInterface.getPrecriptionDispenseList(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PrescriptionDTO> getRefillPrescriptionHistory(RequestDTO request) {
        return fhirServiceApiInterface.getPrescriptionDispenseHistory(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrescriptionPredictionDTO getPrescriptionPrediction(RequestDTO request) {
        return fhirServiceApiInterface.getPrescriptionPrediction(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }
}
