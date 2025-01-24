package com.mdtlabs.coreplatform.spiceservice.apiinterface;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.spiceservice.FeignConfig;
/**
 * This interface is used to access notification service APIs.
 *
 * @author Tamilarasi Shanmugasundaram
 *
 */
@FeignClient(name = "notification-service", url = "${app.notification-service}", configuration = FeignConfig.class)
public interface NotificationApiInterface {


    /**
     * Used to save outbound sms
     * @param token    - user tokem
     * @param tenantId - user tenant id
     * @param smsData  - sms data need to save
     * @return
     */
    @PostMapping("/sms/save-outboundsms")
    public String saveOutBoundSms(@RequestHeader("Authorization") String token,
                                  @RequestHeader("TenantId") Long tenantId, @RequestBody List<SmsDTO> smsData);

    /**
     * Used to get sms templave values based on template type
     * @param token        - user token
     * @param tenantId     - user tenant is
     * @param templateType - template type
     * @return
     */
    @GetMapping("/sms/get-sms-template-values/{templateType}")
    public ResponseEntity<SMSTemplate> getSmsTemplateValues(@RequestHeader("Authorization") String token,
                                                            @RequestHeader("TenantId") Long tenantId, @PathVariable String templateType);

}
