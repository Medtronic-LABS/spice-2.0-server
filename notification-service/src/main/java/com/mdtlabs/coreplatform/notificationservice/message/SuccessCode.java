package com.mdtlabs.coreplatform.notificationservice.message;

/**
 * <p>
 * Success code to fetch message from property. Property
 * file(application.property) present in resource folder.
 * </p>
 *
 * @author Vigneshkumar created on Jun 30, 2022
 */
public enum SuccessCode {

    NOTIFICATION_SAVE(8001),
    NOTIFICATION_UPDATE(8002),
    GET_NOTIFICATIONS(8003),
    GET_NOTIFICATION(8004),
    NOTIFICATION_DELETE(8005),
    NOTIFICATION_MESSAGE_GET(8006),
    PARSE_STRING(8007),
    PARSE_STRING_ERROR(8008),
    SEND_EMAIL_USING_SMTP(8009),
    GET_EMAIL_TEMPLATE_BY_TYPE(8010);

    private int key;

    /**
     * <p>
     * Constructor for the SuccessCode enum.
     * This constructor sets the key for each enum constant.
     * </p>
     *
     * @param key The key to be set for the enum constant.
     */
    SuccessCode(int key) {
        this.key = key;
    }

    /**
     * <p>
     * The function returns the value of the "key" variable.
     * </p>
     *
     * @return An integer value which is the value of the instance variable `key` is returned
     */
    public int getKey() {
        return this.key;
    }
}
