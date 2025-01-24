package com.mdtlabs.coreplatform.notificationservice.controller;

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
import com.mdtlabs.coreplatform.commonservice.common.model.dto.EmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OutBoundEmailDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.EmailTemplate;
import com.mdtlabs.coreplatform.notificationservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.notificationservice.service.EmailService;
import com.mdtlabs.coreplatform.notificationservice.util.TestDataProvider;

/**
 * <p>
 * Email Controller Test is used to test each method by providing
 * fake value as input and verify the output value, which is
 * expected.
 * </p>
 *
 * @author JohnKennedy
 */
@ExtendWith(MockitoExtension.class)
class EmailControllerTest {

    @InjectMocks
    private EmailController emailController;

    @Mock
    private EmailService emailService;

    @Test
    void sendEmailNotification() {
        //given
        EmailDTO emailDTO = TestDataProvider.getEmailDTO();

        //when
        when(emailService.sendEmailNotification(emailDTO)).thenReturn(Constants.BOOLEAN_TRUE);

        //then
        SuccessResponse<Boolean> successResponse = emailController.sendEmailNotification(emailDTO);
        Assertions.assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        Assertions.assertNotNull(successResponse);
    }

    @Test
    void getEmailTemplate() {
        //given
        EmailTemplate emailTemplate = TestDataProvider.getEmailTemplate();

        //when
        when(emailService.getEmailTemplate(Constants.APP_TYPE, Constants.TYPE)).thenReturn(emailTemplate);

        //then
        ResponseEntity<EmailTemplate> response = emailController.getEmailTemplate(Constants.APP_TYPE, Constants.TYPE);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
        Assertions.assertNotNull(response);
    }

    @Test
    void createOutBoundEmail() {
        //given
        OutBoundEmailDTO emailDTO = TestDataProvider.getOutBoundEmailDTO();

        //then
        ResponseEntity<Boolean> response = emailController.createOutBoundEmail(emailDTO);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
        Assertions.assertNotNull(response);
    }
}