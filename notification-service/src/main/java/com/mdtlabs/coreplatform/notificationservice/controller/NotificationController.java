package com.mdtlabs.coreplatform.notificationservice.controller;

import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.NotificationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Notification;
import com.mdtlabs.coreplatform.notificationservice.message.SuccessCode;
import com.mdtlabs.coreplatform.notificationservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.notificationservice.service.NotificationService;

/**
 * <p>
 * Notification Controller defines REST API endpoints for creating, retrieving, updating, and deleting notifications
 * using a NotificationDTO object.
 * </p>
 *
 * @author Prabu created on 20 Feb 2023
 */
@RestController
@RequestMapping(value = "/notification")
public class NotificationController {

    private final NotificationService notificatonService;

    private ModelMapper modelMapper = new ModelMapper();

    public NotificationController(NotificationService notificatonService) {
        this.notificatonService = notificatonService;
    }

    /**
     * <p>
     * This method is used to create a notification using a NotificationDTO object.
     * </p>
     *
     * @param notificationDto {@link NotificationDTO} The updated information for a notification that needs to
     *                        be created is given
     * @return {@link SuccessResponse<Notification>} Returns a success message and status with the created Notification
     */
    @PostMapping
    public SuccessResponse<Notification> saveNotification(@RequestBody NotificationDTO notificationDto) {
        return new SuccessResponse<>(SuccessCode.NOTIFICATION_SAVE,
                notificatonService.saveNotification(modelMapper.map(notificationDto, Notification.class)),
                HttpStatus.OK);
    }

    /**
     * <p>
     * Retrieves all notifications from the database.
     * This method calls the getAllNotification method of the NotificationService to fetch all notifications.
     * It then wraps the list of notifications in a SuccessResponse object, with a success code and HTTP status.
     * </p>
     *
     * @return A SuccessResponse containing a list of all notifications, a success code, and an HTTP status.
     */
    @GetMapping
    public SuccessResponse<Notification> getAllNotification() {
        return new SuccessResponse<>(SuccessCode.GET_NOTIFICATIONS, notificatonService.getAllNotification(),
                HttpStatus.OK);
    }

    /**
     * <p>
     * Updates a notification using a NotificationDTO object.
     * This method maps the NotificationDTO to a Notification entity using ModelMapper.
     * It then calls the updateNotification method of the NotificationService to update the notification.
     * The updated notification is wrapped in a SuccessResponse object, with a success code and HTTP status.
     * </p>
     *
     * @param notificationDto The NotificationDTO object containing the updated information for the notification.
     * @return A SuccessResponse containing the updated notification, a success code, and an HTTP status.
     */
    @PutMapping
    public SuccessResponse<Notification> updateNotification(@RequestBody NotificationDTO notificationDto) {
        return new SuccessResponse<>(SuccessCode.NOTIFICATION_UPDATE,
                notificatonService.updateNotification(modelMapper.map(notificationDto, Notification.class)),
                HttpStatus.OK);
    }

    /**
     * <p>
     * Deletes a notification by its ID.
     * This method calls the deleteNotificationById method of the NotificationService to delete the notification.
     * The result is wrapped in a SuccessResponse object, with a success code and HTTP status.
     * </p>
     *
     * @param notificationId The ID of the notification to be deleted.
     * @return A SuccessResponse indicating the success of the deletion operation, a success code, and an HTTP status.
     */
    @PostMapping(value = "/{notificationId}")
    public SuccessResponse<Notification> deleteNotificationById(
            @PathVariable(value = Constants.NOTIFICATION_ID) long notificationId) {
        return new SuccessResponse<>(SuccessCode.NOTIFICATION_DELETE,
                notificatonService.deleteNotificationById(notificationId), HttpStatus.OK);
    }

    /**
     * <p>
     * Retrieves a notification by its ID.
     * This method calls the getNotificationById method of the NotificationService to fetch the notification.
     * The fetched notification is wrapped in a SuccessResponse object, with a success code and HTTP status.
     * </p>
     *
     * @param notificationId The ID of the notification to be fetched.
     * @return A SuccessResponse containing the fetched notification, a success code, and an HTTP status.
     */
    @GetMapping(value = "/{notificationId}")
    public SuccessResponse<Notification> getNotificationById(
            @PathVariable(value = Constants.NOTIFICATION_ID) long notificationId) {
        return new SuccessResponse<>(SuccessCode.GET_NOTIFICATION,
                notificatonService.getNotificationById(notificationId), HttpStatus.OK);
    }

}