package com.mdtlabs.coreplatform.spiceservice.registration.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.PutObjectRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.registration.service.RegistrationService;

/**
 * <p>
 * This class is a service class to perform operation on Resgistration.
 * </p>
 *
 * @author Karthick M created on Aug 05, 2024
 */
@Service
public class RegistrationServiceImpl implements RegistrationService {

    @Value("${application.bucket.name}")
    private String bucketName;


    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final AdminServiceApiInterface adminServiceApiInterface;
    private final CustomizedModulesService customizedModulesService;
    private final AmazonS3 s3Client;
    private final FollowUpService followUpService;


    public RegistrationServiceImpl(FhirServiceApiInterface fhirServiceApiInterface,
                                   AdminServiceApiInterface adminServiceApiInterface, CustomizedModulesService customizedModulesService, AmazonS3 s3Client,
                                   FollowUpService followUpService) {

        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.adminServiceApiInterface = adminServiceApiInterface;
        this.customizedModulesService = customizedModulesService;
        this.s3Client = s3Client;
        this.followUpService = followUpService;
    }
    
    /**
     * {@inheritDoc}
     */
    public EnrollmentResponseDTO registerPatientAndMember(EnrollmentRequestDTO request, MultipartFile file) {
        EnrollmentResponseDTO response = fhirServiceApiInterface.registerPatientAndMember(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
        HealthFacility healthFacility = adminServiceApiInterface.getHealthFacilitiy(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), request.getTenantId());
        response.setFacilityName(healthFacility.getName());
        createCustomizedModules(request, response);
        if (Objects.nonNull(file) && Objects.nonNull(response.getMemberId())) {
            String signatureUrl = uploadSignature(file, request.getBioData().getFirstName(), response.getMemberId());
            boolean isSignatureUpdated = fhirServiceApiInterface.updateMemberSignature(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                    new RequestDTO(request.getProvenance(), response.getMemberId(), signatureUrl));
            if (isSignatureUpdated) {
                Logger.logInfo("Signature file url is updated");
            } else {
                Logger.logInfo("Signature file url is not updated");
            }
        }

        followUpService.deleteNcdCallRegister(new FollowUpDTO(response.getMemberId(), AppointmentType.SCREENED));
        followUpService.transferCallRegisters(response.getMemberId(), request.getHealthFacilityFhirId());
        return response;
        
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PatientValidationResponseDTO isPatientRegisteredInRelatedPerson(BioDataDTO request) {
        return fhirServiceApiInterface.validatePatient(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * <p>
     * Creates the customized module for the registered patient.
     * </p>
     * @param response the response of the patient to be returned is given
     * @param requestDTO the patient request is given
     */
    private void createCustomizedModules(EnrollmentRequestDTO requestDTO, EnrollmentResponseDTO response) {
        if (!Objects.isNull(requestDTO.getCustomizedWorkflows())
                && !requestDTO.getCustomizedWorkflows().isEmpty()) {
            if (Objects.isNull(response.getPatientId()) || Objects.isNull(response.getMemberId())) {
                throw new DataNotFoundException(8001);
            }
            customizedModulesService.createCustomizedModules(requestDTO.getCustomizedWorkflows(),
                    Constants.WORKFLOW_ENROLLMENT, response.getMemberId(), response.getPatientId());
        }

        customizedModulesService.updateCustomizedModules(response.getMemberId(), response.getPatientId());
    }

    /**
     * <p>
     * To Upload signature file in S3
     * </p>
     *
     * @param multipartFile - the multipart file to be uploaded is given
     * @param memberId      - The saved member id is given
     * @param firstName      - The first name of the  member is given
     * @return The URL of uploaded file in S3 is given
     */
    private String uploadSignature(MultipartFile multipartFile, String firstName, String memberId) {
        try {
            File fileObj = CommonUtil.convertMultipartFileToFile(multipartFile);
            Instant instant = Instant.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.SIGNATURE_DATE_FORMAT)
                    .withZone(ZoneId.of(com.mdtlabs.coreplatform.commonservice.common.Constants.TIMEZONE_UTC));
            String timestamp = formatter.format(instant);
            String fileName = Constants.ENROLLMENT_FOLDER.concat(memberId).concat(com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE)
                    .concat(firstName.toLowerCase().trim()).concat(com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE).concat(timestamp)
                    .concat(com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE)
                    .concat(Constants.ENROLLMENT_FORMAT);
            s3Client.putObject(new PutObjectRequest(bucketName, fileName, fileObj));
            String url = s3Client.getUrl(bucketName, fileName).toString();
            Files.delete(fileObj.toPath());
            return url;
        } catch (IOException ioException) {
            Logger.logTrace(ioException.getMessage());
            throw new SpiceValidation(1513);
        }
    }
}
