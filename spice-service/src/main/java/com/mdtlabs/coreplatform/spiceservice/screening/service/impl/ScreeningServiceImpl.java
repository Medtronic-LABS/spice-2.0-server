package com.mdtlabs.coreplatform.spiceservice.screening.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.RedRiskNotification;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.NotificationApiInterface;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.Repository.RedRiskNotificationRepository;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.customizedmodules.service.CustomizedModulesService;
import com.mdtlabs.coreplatform.spiceservice.followup.service.FollowUpService;
import com.mdtlabs.coreplatform.spiceservice.screening.service.ScreeningService;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.PutObjectRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


/**
 * <p>
 *   Service implementation for handling screening related operations.
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Service
public class ScreeningServiceImpl implements ScreeningService {

    @Value("${application.bucket.name}")
    private String bucketName;

    @Value("${app.enable-red-risk-notification}")
    private boolean enableRedRiskNotification;

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final CustomizedModulesService customizedModulesService;

    private final FollowUpService followUpService;

    private final AmazonS3 s3Client;

    private final RedRiskNotificationRepository redRiskNotificationRepository;

    private final UserServiceApiInterface userApiInterface;

    private final NotificationApiInterface notificationApiInterface;

    @Autowired
    public ScreeningServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, CustomizedModulesService customizedModulesService, AmazonS3 s3Client,
                                FollowUpService followUpService, RedRiskNotificationRepository redRiskNotificationRepository, UserServiceApiInterface userApiInterface, NotificationApiInterface notificationApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.customizedModulesService = customizedModulesService;
        this.s3Client = s3Client;
        this.followUpService = followUpService;
        this.redRiskNotificationRepository = redRiskNotificationRepository;
        this.userApiInterface = userApiInterface;
        this.notificationApiInterface = notificationApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public BioDataDTO processScreeningLog(ScreeningLogRequestDTO request, MultipartFile file) {
        BioDataDTO response = fhirServiceApiInterface.screeningCreate(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                request);
        if (Boolean.TRUE.equals(request.getIsReferAssessment())
                && Constants.SCREENED.equalsIgnoreCase(response.getRelatedPersonStatus())
                && Objects.nonNull(response.getMemberReference())
                && Objects.nonNull(response.getPatientReference())) {
            CallRegister callRegister = new CallRegister();
            callRegister.setMemberId(response.getMemberReference());
            callRegister.setPatientId(response.getPatientReference());
            callRegister.setIsInitiated(Boolean.FALSE);
            callRegister.setType(AppointmentType.SCREENED);
            callRegister.setIsWrongNumber(Boolean.FALSE);
            callRegister.setReferredSiteId(String.valueOf(request.getSiteId()));
            if (Objects.nonNull(request.getBioData()) && Objects.nonNull(request.getBioData().getVillage())) {
                callRegister.setVillageId(request.getBioData().getVillage().getId().toString());
            }
            callRegister.setScreeningDateTime(request.getScreeningDateTime());
            followUpService.addCallRegister(callRegister, Boolean.TRUE);
        }
        if (Objects.nonNull(response.getMemberReference()) && Objects.nonNull(request.getPregnancyAnc()) &&
                Objects.nonNull(request.getPregnancyAnc().getPregnancySymptoms()) &&
                !Constants.NO_SYMPTOMS.equals(request.getPregnancyAnc().getPregnancySymptoms().getFirst().getName())) {
            RedRiskNotification redRiskNotification = createRedRiskNotification(response.getPatientReference(), response.getEncounterReference(),
                    response.getMemberReference());
            if (enableRedRiskNotification) {
                sendRedRiskSms(response.getPatientReference(), redRiskNotification);
            }
        }

        if (Objects.nonNull(response.getMemberReference())) {
            request.setMemberReference(response.getMemberReference());
            request.setFhirId(response.getPatientReference());
            updateSignature(file, request);
            createCustomizedModules(request);
        }
        return response;
    }

    /**
     * To send red risk notification.
     *
     * @param patientId                - patient id
     * @param notification Notification that having the id need to saved in appropriate outbound sms
     */
    private void sendRedRiskSms(String patientId, RedRiskNotification notification) {
        List<UserResponseDTO> users = userApiInterface.getUsersByRoleName(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), Constants.ROLE_RED_RISK_USER).getBody();
        List<SmsDTO> smsDtos = new ArrayList<>();
        SMSTemplate smsTemplate = notificationApiInterface
                .getSmsTemplateValues(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(), Constants.TEMPLATE_TYPE_RED_RISK)
                .getBody();
        if (Objects.nonNull(users) && !users.isEmpty()) {
            users.stream().forEach(user -> {
                if (Objects.equals(user.getTenantId(), UserSelectedTenantContextHolder.get())) {
                    smsDtos.add(new SmsDTO(smsTemplate.getBody(), user.getCountryCode() + user.getPhoneNumber(), user.getUsername(),
                            notification.getTenantId(), patientId, notification.getId()));
                }
            });
        }
        notificationApiInterface.saveOutBoundSms(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                smsDtos);
    }

    /**
     * Creates a red risk notification for a patient.
     *
     * @param patientId                - patient id
     * @param encounterId              - encounter id
     * @param memberId                 - member id
     * @return RedRiskNotification - entity
     */
    private RedRiskNotification createRedRiskNotification(String patientId, String encounterId, String memberId) {
        Logger.logInfo("In ScreeningServiceImpl, creating red risk notification information");
        RedRiskNotification redRiskNotification = new RedRiskNotification();
        redRiskNotification.setMemberId(memberId);
        redRiskNotification.setEncounterId(encounterId);
        redRiskNotification.setPatientId(patientId);
        redRiskNotification.setStatus(Constants.NEW);
        return redRiskNotificationRepository.save(redRiskNotification);
    }

    private void createCustomizedModules(ScreeningLogRequestDTO screeningLogRequestDTO) {
        if (!Objects.isNull(screeningLogRequestDTO.getCustomizedWorkflows())
                && !screeningLogRequestDTO.getCustomizedWorkflows().isEmpty()) {
            if (Objects.isNull(screeningLogRequestDTO.getMemberReference())) {
                throw new DataNotFoundException(8001);
            }
            customizedModulesService.createCustomizedModules(screeningLogRequestDTO.getCustomizedWorkflows(),
                    Constants.WORKFLOW_SCREENING, screeningLogRequestDTO.getMemberReference(), screeningLogRequestDTO.getFhirId());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DashboardDetails getPatientCountOfUsers(DashboardDetailsRequestDTO request) throws DataNotFoundException {
        DashboardDetails response = fhirServiceApiInterface.getPatientCount(CommonUtil.getAuthToken()
                , CommonUtil.getClient(), request);
        if (Objects.isNull(response)) {
            throw new DataNotFoundException(8002);
        }
        return response;
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
            String fileName = Constants.SCREENING_FOLDER.concat(memberId).concat(com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE)
                    .concat(firstName.toLowerCase().trim()).concat(com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE).concat(timestamp)
                    .concat(com.mdtlabs.coreplatform.commonservice.common.Constants.UNDER_SCORE)
                    .concat(Constants.SCREENING_FORMAT);
            s3Client.putObject(new PutObjectRequest(bucketName, fileName, fileObj));
            String url = s3Client.getUrl(bucketName, fileName).toString();
            Files.delete(fileObj.toPath());
            return url;
        } catch (IOException ioException) {
            Logger.logTrace(ioException.getMessage());
            throw new SpiceValidation(1514);
        }
    }

    /**
     * <p>
     * To update signature url in fhir resource
     * </p>
     *
     * @param file - the multipart file to be uploaded is given
     * @param request      - the screening request with member id
     */
    public void updateSignature(MultipartFile file, ScreeningLogRequestDTO request) {
        if (Objects.nonNull(file)) {
            String signatureUrl = uploadSignature(file, request.getBioData().getFirstName(), request.getMemberReference());
            request.setSignatureUrl(signatureUrl);
            boolean isSignatureUpdated = fhirServiceApiInterface.updateMemberSignature(CommonUtil.getAuthToken(), CommonUtil.getClient(),
                    new RequestDTO(new ProvenanceDTO(request.getUserId().toString(), request.getSiteId().toString(),
                            request.getScreeningDateTime()), request.getMemberReference(), signatureUrl));
            if (isSignatureUpdated) {
                Logger.logInfo("Signature file url is updated");
            } else {
                Logger.logInfo("Signature file url is not updated");
            }
        }
    }
}
