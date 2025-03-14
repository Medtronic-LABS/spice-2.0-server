package com.mdtlabs.coreplatform.userservice.apiinterface;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.OutBoundEmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplate;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.commonservice.common.Constants;

/**
 * <p>
 * This interface is for feign that communicates with a notification service.
 * </p>
 *
 * @author Jeyaharini T A created on Feb 14, 2023
 */
@FeignClient(name = "notification-service", url = "${app.notification-service}")
public interface NotificationInterface {

    /**
     * <p>
     * This method is used to retrieve an email template based on the specified email type and application type.
     * </p>
     *
     * @param type    {@link String} The "type" parameter is a path variable that represents the type of email
     *                template that is being requested. It could be a welcome email, password reset email,
     *                confirmation email, etc. The actual value of this parameter will be provided in the
     *                URL when the API is called
     * @param appType {@link String} The appType parameter is a path variable that is used to specify the
     *                type of application for which the email template is being requested is given
     * @return {@link ResponseEntity<EmailTemplate>} The email template for given type and application type
     * is retrieved with status
     */
    @GetMapping("/email/email-type/{type}/{appType}")
    public ResponseEntity<EmailTemplate> getEmailTemplate(@PathVariable(Constants.TYPE) String type,
                                                          @PathVariable(Constants.APP_TYPE) String appType);

    /**
     * <p>
     * This method is used to create an outbound email using the provided email DTO and returns a boolean
     * response entity.
     * </p>
     *
     * @param emailDto {@link OutBoundEmailDTO} This OutBoundEmailDTO contains the necessary information to create
     *                 an outbound email
     * @return The boolean value is returned with the status indicating whether the creation of an outbound email is
     * successful or not
     */
    @PostMapping("/email/create")
    public ResponseEntity<Boolean> createOutBoundEmail(@RequestBody OutBoundEmailDTO emailDto);

    @GetMapping("/sms/get-sms-template-values/{templateType}")
    public ResponseEntity<SMSTemplate> getSmsTemplateValues(@PathVariable String templateType);

    @PostMapping("/sms/save-outboundsms")
    public boolean saveOutBoundSMS(@RequestBody List<SmsDTO> outBoundSMSList);
}
