package com.mdtlabs.coreplatform.notificationservice.scheduler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.client.RestTemplate;

import com.amazonaws.services.sns.AmazonSNS;
import com.amazonaws.services.sns.AmazonSNSClientBuilder;
import com.amazonaws.services.sns.model.PublishRequest;
import com.amazonaws.services.sns.model.PublishResult;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.OutBoundSMS;
import com.mdtlabs.coreplatform.notificationservice.service.SmsService;
import com.twilio.Twilio;
import com.twilio.rest.api.v2010.account.Message;
import com.twilio.type.PhoneNumber;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;

/**
 * <p>
 * SmsScheduler class schedules and sends SMS notifications using either Twilio API or AWS SNS
 * and updates the status of the sent messages.
 * </p>
 *
 * @author ShrikanthC Mar 24, 2023
 */
@EnableScheduling
@Configuration
@ConditionalOnProperty(name = "spring.enable.scheduling.sms")
public class SmsScheduler {


    private final SmsService smsService;

    @Value("${app.twilio-account-sid}")
    private String twilioAccountSid;

    @Value("${app.twilio-auth-token}")
    private String twilioAuthToken;

    @Value("${app.twilio-from-phoneno}")
    private String twilioFromPhoneno;

    @Value("${app.enable-sms-notification-twilio}")
    private String enableSMSNotificationTwilio;

    @Value("${app.enable-sms-notification-sns}")
    private String enableSMSNotificationSNS;

    @Value("${app.aws-region}")
    private String serverRegion;

    @Value("${app.enable-sms-notification-digimiles}")
    private String enableSMSNotificationDigimiles;

    @Value("${app.digimiles-url}")
    private String digimilesURL;

    @Value("${app.digimiles-username}")
    private String digimilesUsername;

    @Value("${app.digimiles-password}")
    private String digimilesPassword;

    @Value("${app.digimiles-type}")
    private String digimilesType;

    @Value("${app.digimiles-dlr}")
    private String digimilesDlr;

    @Value("${app.digimiles-sender}")
    private String digimilesSender;

    public SmsScheduler(SmsService smsService) {
        this.smsService = smsService;
    }

    /**
     * <p>
     * This method is used to send SMS notifications using Twilio API based on a scheduled cron expression
     * and to update the status of the sent messages.
     * </p>
     */
    @Scheduled(cron = "${scheduler.cron.sms}")
    @SchedulerLock(name = "TaskScheduler_sms",
            lockAtLeastFor = "${app.shedlock.sms.start}", lockAtMostFor = "${app.shedlock.sms.stop}")
    public void scheduleTaskUsingCronExpression() {
        if (Boolean.parseBoolean(enableSMSNotificationTwilio)) {
            List<OutBoundSMS> smsList = smsService.getOutBoundSms();
            List<OutBoundSMS> smsListToUpdate = new ArrayList<>();
            if (!smsList.isEmpty()) {
                Twilio.init(twilioAccountSid, twilioAuthToken);
                for (OutBoundSMS outBoundSms : smsList) {
                    Message message = Message.creator(new PhoneNumber(outBoundSms.getPhoneNumber()),
                            new PhoneNumber(twilioFromPhoneno), outBoundSms.getBody()).create();
                    if (!Objects.isNull(message.getSid())) {
                        outBoundSms.setProcessed(true);
                    } else {
                        outBoundSms.setRetryAttempts(outBoundSms.getRetryAttempts() + 1);
                    }
                    smsListToUpdate.add(outBoundSms);
                }
                smsService.updateOutBoundSms(smsListToUpdate);

            }
        }

    }

    /**
     * <p>
     * This method is used to send SMS notifications using AWS SNS and to update the status of the sent messages.
     * </p>
     */
    @Scheduled(cron = "${scheduler.cron.sms}")
    @SchedulerLock(name = "TaskScheduler_sms",
            lockAtLeastFor = "${app.shedlock.sms.start}", lockAtMostFor = "${app.shedlock.sms.stop}")
    public void sendSMS() {
        if (Boolean.parseBoolean(enableSMSNotificationSNS)) {
            List<OutBoundSMS> smsList = smsService.getOutBoundSms();
            List<OutBoundSMS> smsListToUpdate = new ArrayList<>();
            if (!smsList.isEmpty()) {
                for (OutBoundSMS outBoundSms : smsList) {
                    AmazonSNS snsClient = AmazonSNSClientBuilder.standard().withRegion(serverRegion).build();
                    PublishRequest request = new PublishRequest();
                    request.withMessage(outBoundSms.getBody()).withPhoneNumber(outBoundSms.getPhoneNumber());
                    PublishResult publishResult = snsClient.publish(request);
                    if (!Objects.isNull(publishResult.getMessageId())) {
                        outBoundSms.setProcessed(true);
                    } else {
                        outBoundSms.setRetryAttempts(outBoundSms.getRetryAttempts() + 1);
                    }
                    smsListToUpdate.add(outBoundSms);
                }
                smsService.updateOutBoundSms(smsListToUpdate);

            }
        }

    }

    /**
     * <p>
     * Schedules unprocessed sms based on cron expression and sends to the corresponding user using digimiles.
     * </p>
     */
    @Scheduled(cron = "${scheduler.cron.sms}")
    @SchedulerLock(name = "TaskScheduler_sms",
            lockAtLeastFor = "${app.shedlock.sms.start}", lockAtMostFor = "${app.shedlock.sms.stop}")
    public void sendSMSUsingDigimiles() {
        if (Boolean.parseBoolean(enableSMSNotificationDigimiles)) {
            List<OutBoundSMS> smsList = smsService.getOutBoundSms();
            List<OutBoundSMS> smsListToUpdate = new ArrayList<>();
            if (!smsList.isEmpty()) {
                for (OutBoundSMS outBoundSms : smsList) {
                    HttpHeaders headers = new HttpHeaders();
                    HttpEntity<String> entity = new HttpEntity<>(headers);
                    String url = digimilesURL
                            .concat(Constants.USERNAME_URL).concat(digimilesUsername)
                            .concat(Constants.PASSWORD_URL).concat(digimilesPassword)
                            .concat(Constants.TYPE_URL).concat(digimilesType)
                            .concat(Constants.DLR_URL).concat((digimilesDlr)
                                    .concat(Constants.DESTINATION_URL).concat(outBoundSms.getPhoneNumber())
                                    .concat(Constants.SOURCE_URL).concat(digimilesSender)
                                    .concat(Constants.MESSAGE_URL).concat(outBoundSms.getBody()));
                    Logger.logInfo("URL " + url);
                    RestTemplate restTemplate = new RestTemplate();
                    ResponseEntity<String> responseValue = restTemplate.exchange(url, HttpMethod.POST, entity, String.class);
                    Logger.logInfo("Response value body " + responseValue.getBody());
                    String responseValueBody = responseValue.getBody();
                    Map<String, String> map = new HashMap<>();
                    constructResponseMessage(outBoundSms, responseValueBody, map, responseValue);
                    smsListToUpdate.add(outBoundSms);
                }
                smsService.updateOutBoundSms(smsListToUpdate);
            }
        }
    }

    /**
     * <p>
     * Constructs response message received from digimiles.
     * </p>
     */
    private void constructResponseMessage(OutBoundSMS outBoundSms, String responseValueBody, Map<String, String> map, ResponseEntity<String> responseValue) {
        if (Objects.nonNull(responseValueBody)) {
            String[] bodyArray = responseValueBody.split(Constants.DIGIMILES_REGEX);
            String value = bodyArray[0];
            map.put(Constants.MESSAGE, Constants.RESPONSE_CODES.get(value));
            map.put(Constants.VALUE, responseValue.getBody());
            ResponseEntity<Map<String, String>> responseEntity = new ResponseEntity<>(map, HttpStatus.OK);
            updateOutBoundSMS(outBoundSms, value);
            Logger.logInfo("Response Entity " + responseEntity);
        }
    }

    /**
     * <p>
     * Updates the outbound sms based on the response received from digimiles.
     * </p>
     */
    private void updateOutBoundSMS(OutBoundSMS outBoundSms, String value) {
        if (value.equals(Constants.SUCCESS_CODE_DIGIMILES)) {
            outBoundSms.setProcessed(Constants.BOOLEAN_TRUE);
        } else {
            outBoundSms.setRetryAttempts(outBoundSms.getRetryAttempts() + 1);
        }
    }
}
