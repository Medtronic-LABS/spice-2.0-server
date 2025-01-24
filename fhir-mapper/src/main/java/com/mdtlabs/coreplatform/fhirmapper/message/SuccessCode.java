package com.mdtlabs.coreplatform.fhirmapper.message;

/**
 * <p>
 * Success code to fetch message from property. Property
 * file(application.property) present in resource folder.
 * </p>
 *
 * @author Nandhakumar created on Feb 05, 2024
 */
public enum SuccessCode {
    HOUSEHOLD_CREATED(1000), HOUSEHOLD_MEMBER_CREATED(1001),
    GOT_HOUSEHOLD(1002), GOT_PATIENT(1003), HEALTH_CHECK(9000),
    GOT_BP_LOG_LIST(1004);
    private int key;

    SuccessCode(int key) {
        this.key = key;
    }

    public int getKey() {
        return this.key;
    }
}
