package com.mdtlabs.coreplatform.notificationservice.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.OutBoundSMS;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.notificationservice.service.SmsService;
import com.mdtlabs.coreplatform.notificationservice.service.SmsTemplateService;

/**
 * <p>
 * SMS Controller class that defines REST API endpoints for managing SMS templates and outbound SMS messages.
 * </p>
 *
 * @author Prabu created on 23 Mar 2023
 */
@RestController
@RequestMapping(value = "/sms")
public class SmsController {

    private final SmsService smsService;

    private final SmsTemplateService templateService;

    private final SmsTemplateService smsTemplateService;

    public SmsController(SmsService smsService, SmsTemplateService templateService, SmsTemplateService smsTemplateService) {
        this.smsService = smsService;
        this.templateService = templateService;
        this.smsTemplateService = smsTemplateService;
    }

    /**
     * <p>
     * Saves a list of outbound SMS messages.
     * This method takes a list of SmsDTO objects, each representing an outbound SMS message, and saves them using the SmsService.
     * After the save operation, it returns a SuccessResponse with a success code and HTTP status.
     * </p>
     *
     * @param smsDataList The list of SmsDTO objects representing the outbound SMS messages to be saved.
     * @return A SuccessResponse indicating the success of the save operation, a success code, and an HTTP status.
     */
    @PostMapping("/save-outboundsms")
    public boolean saveOutBoundSms(@RequestBody List<SmsDTO> smsDataList) {
        return smsService.saveOutBoundSms(smsDataList);
    }

    /**
     * <p>
     * Retrieves the values of an SMS template by its type.
     * This method calls the getSmsTemplateValues method of the SmsTemplateService to fetch the SMS template values.
     * The fetched values are wrapped in a ResponseEntity object.
     * </p>
     *
     * @param templateType The type of the SMS template to fetch its values.
     * @return A ResponseEntity containing the fetched SMS template values.
     */
    @GetMapping("/get-sms-template-values/{templateType}")
    public ResponseEntity<SMSTemplate> getSmsTemplateValues(@PathVariable("templateType") String templateType) {
        return ResponseEntity.ok().body(templateService.getSmsTemplateValues(templateType));
    }

    /**
     * <p>
     * Retrieves a list of SMS templates by their types.
     * This method takes a list of types and calls the getSMSTemplates method of the SmsTemplateService to fetch the SMS templates.
     * </p>
     *
     * @param list The list of types of the SMS templates to be fetched.
     * @return A list of SMSTemplate objects representing the fetched SMS templates.
     */
    @PostMapping("/get-sms-template")
    public List<SMSTemplate> getSMSTemplates(@RequestBody List<String> list) {
        return smsTemplateService.getSMSTemplates(list);
    }

    /**
     * <p>
     * Saves a list of outbound SMS messages.
     * This method takes a list of OutBoundSMS objects, each representing an outbound SMS message, and updates them using the SmsService.
     * After the update operation, it returns a list of updated OutBoundSMS objects.
     * </p>
     *
     * @param outBoundSmsList The list of OutBoundSMS objects representing the outbound SMS messages to be updated.
     * @return A list of OutBoundSMS objects representing the updated outbound SMS messages.
     */
    @PostMapping("/save-outboundsms-values")
    public List<OutBoundSMS> saveOutBoundSmsValues(@RequestBody List<OutBoundSMS> outBoundSmsList) {
        return smsService.updateOutBoundSms(outBoundSmsList);
    }

}
