package com.mdtlabs.coreplatform.notificationservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Notification;

/**
 * <p>
 * This is a Java interface for a repository that communicates with a database to perform
 * CRUD operations on Notification entities. It extends two interfaces, JpaRepository
 * and PagingAndSortingRepository, which provide methods for basic CRUD operations and
 * pagination/sorting respectively.
 * </p>
 *
 * @author Prabu created on Sep 16, 2022
 */
@Repository
public interface NotificationRepository
        extends JpaRepository<Notification, Long>, PagingAndSortingRepository<Notification, Long> {

    public static final String GET_ALL_NOTIFICATION = "select notification from Notification as notification";
    public static final String UPDATE_NOTIFICATION_STATUS_BY_ID = "update Notification as notification"
            + " set notification.status = 'Canceled' where notification.id =:notificationId";
    public static final String GET_NOTIFICATION_BY_ID = GET_ALL_NOTIFICATION
            + " where notification.id =:notificationId ";
    public static final String UPDATE_IS_READ_NOTIFICATION_VALUE = "update Notification as notification "
            + "set notification.is_read = :status where notification.id =:notificationId";
    public static final String GET_NOTIFICATION_MESSAGES_BY_EMAIL = "select notification.id as id, "
            + "notification.subject as subject, notification.message as message, "
            + "DATE_FORMAT(notification.created_at, '%Y-%m-%d') as time from notification as notification "
            + "where notification.to_email like %:toEmail% order by notification.created_at desc limit "
            + Constants.WEB_NOTIFICATION_LIMIT;

    /**
     * <p>
     * This method is used to retrieve all notifications from a database.
     * </p>
     *
     * @return {@link List<Notification>} A list of notifications is retrieved from the database and returned
     */
    @Query(value = GET_ALL_NOTIFICATION)
    public List<Notification> getAllNotification();

    /**
     * <p>
     * This method used to update the status of a notification by its ID in a database.
     * </p>
     *
     * @param notificationId The notification ID associated with the notification that needs to update is given
     * @return An integer value is returned indicating the number of rows affected by the update query
     */
    @Modifying
    @Transactional
    @Query(value = UPDATE_NOTIFICATION_STATUS_BY_ID)
    public int updateNotificationStatusById(@Param(Constants.NOTIFICATION_ID) long notificationId);

    /**
     * <p>
     * This method used to get the notification detail for the given ID.
     * </p>
     *
     * @param notificationId The notification ID to retrieve a specific notification from the
     *                       database is given
     * @return {@link Notification} The notification for the given notification ID is retrieved
     * from the database and returned
     */
    @Query(value = GET_NOTIFICATION_BY_ID)
    public Notification getNotificationById(@Param(Constants.NOTIFICATION_ID) long notificationId);

}