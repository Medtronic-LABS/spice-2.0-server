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

import com.mdtlabs.coreplatform.commonservice.common.model.dto.NotificationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Notification;
import com.mdtlabs.coreplatform.notificationservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.notificationservice.service.NotificationService;
import com.mdtlabs.coreplatform.notificationservice.util.TestDataProvider;

/**
 * <p>
 * Notification Controller Test is used to test each method by providing
 * fake value as input and verify the output value, which is
 * expected.
 * </p>
 *
 * @author JohnKennedy
 */
@ExtendWith(MockitoExtension.class)
class NotificationControllerTest {

    @InjectMocks
    private NotificationController notificationController;

    @Mock
    private NotificationService notificationService;


    @Test
    void saveNotification() {
        //given
        NotificationDTO notificationDTO = TestDataProvider.getNotificationDTO();
        Notification notification = TestDataProvider.getNotification();

        //when
        when(notificationService.saveNotification(notification)).thenReturn(notification);

        //then
        SuccessResponse<Notification> successResponse = notificationController.saveNotification(notificationDTO);
        Assertions.assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        Assertions.assertNotNull(successResponse);
    }

    @Test
    void getAllNotification() {
        //given
        List<Notification> notifications = List.of(TestDataProvider.getNotification());

        //when
        when(notificationService.getAllNotification()).thenReturn(notifications);

        //then
        SuccessResponse<Notification> successResponse = notificationController.getAllNotification();
        Assertions.assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        Assertions.assertNotNull(successResponse);
    }

    @Test
    void updateNotification() {
        //given
        NotificationDTO notificationDTO = TestDataProvider.getNotificationDTO();
        Notification notification = TestDataProvider.getNotification();

        //when
        when(notificationService.updateNotification(notification)).thenReturn(notification);

        //then
        SuccessResponse<Notification> successResponse = notificationController.updateNotification(notificationDTO);
        Assertions.assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        Assertions.assertNotNull(successResponse);
    }

    @Test
    void deleteNotificationById() {
        //when
        when(notificationService.deleteNotificationById(1l)).thenReturn(1);

        //then
        SuccessResponse<Notification> successResponse = notificationController.deleteNotificationById(1l);
        Assertions.assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        Assertions.assertNotNull(successResponse);
    }

    @Test
    void getNotificationById() {
        //given
        Notification notification = TestDataProvider.getNotification();

        //when
        when(notificationService.getNotificationById(1l)).thenReturn(notification);

        //then
        SuccessResponse<Notification> successResponse = notificationController.getNotificationById(1l);
        Assertions.assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        Assertions.assertNotNull(successResponse);
    }
}