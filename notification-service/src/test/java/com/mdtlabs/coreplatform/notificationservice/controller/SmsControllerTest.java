package com.mdtlabs.coreplatform.notificationservice.controller;

import java.util.List;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SmsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.OutBoundSMS;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.SMSTemplate;
import com.mdtlabs.coreplatform.notificationservice.service.SmsService;
import com.mdtlabs.coreplatform.notificationservice.service.SmsTemplateService;
import com.mdtlabs.coreplatform.notificationservice.util.TestDataProvider;

/**
 * <p>
 * SMS Controller Test is used to test each method by providing
 * fake value as input and verify the output value, which is
 * expected.
 * </p>
 *
 * @author JohnKennedy
 */
@ExtendWith(MockitoExtension.class)
class SmsControllerTest {

    @InjectMocks
    private SmsController smsController;

    @Mock
    private SmsService smsService;

    @Mock
    private SmsTemplateService smsTemplateService;


    @Test
    void saveOutBoundSms() {
        //given
        List<SmsDTO> smsDataList = List.of(TestDataProvider.getSmsDTO());

        //when
        when(smsService.saveOutBoundSms(smsDataList)).thenReturn(Constants.BOOLEAN_TRUE);

        //then
        boolean response = smsController.saveOutBoundSms(smsDataList);
        Assertions.assertEquals(Boolean.TRUE, response);
    }

    @Test
    void getSmsTemplateValues() {

        ResponseEntity<SMSTemplate> response = smsController.getSmsTemplateValues(Constants
                .TEMPLATE_TYPE_ENROLL_PATIENT);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
        Assertions.assertNotNull(response);
    }

    @Test
    void getSMSTemplates() {
        //given
        SMSTemplate smsTemplate = TestDataProvider.getSmsTemplate();
        smsTemplate.setId(1L);
        List<SMSTemplate> smsTemplates = List.of(smsTemplate);

        //when
        when(smsTemplateService.getSMSTemplates(null)).thenReturn(smsTemplates);

        //then
        List<SMSTemplate> response = smsController.getSMSTemplates(null);
        Assertions.assertNotNull(response);
    }

    @Test
    void saveOutBoundSmsValues() {
        //given
        OutBoundSMS outBoundSMS = TestDataProvider.getOutBoundSMS();
        outBoundSMS.setId(1L);
        List<OutBoundSMS> outBoundSMSList = List.of(outBoundSMS);

        //when
        when(smsService.updateOutBoundSms(outBoundSMSList)).thenReturn(outBoundSMSList);

        //then
        List<OutBoundSMS> result = smsController.saveOutBoundSmsValues(outBoundSMSList);
        Assertions.assertNotNull(result);
        Assertions.assertEquals(1, result.get(0).getId());
    }
}