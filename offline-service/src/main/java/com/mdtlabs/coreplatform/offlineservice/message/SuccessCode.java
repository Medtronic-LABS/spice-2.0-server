package com.mdtlabs.coreplatform.offlineservice.message;

/**
 * <p>
 * Success code to fetch message from property. Property
 * file(application.property) present in resource folder.
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 *
 */
public enum SuccessCode {

    // Household
    OFFLINE_SYNC_REQUEST_SAVE(3100),
    OFFLINE_SYNC_GENERATE_RESPONSE(3101),
    OFFLINE_SYNC_GET_STATUS(3102),
    OFFLINE_SYNC_FETCH_SYNCED_DATA(3103),
    OFFLINE_SYNC_SIGNATURE_UPLOAD(3104);


 
    private int key;

    SuccessCode(int key) {
        this.key = key;
    }

    public int getKey() {
        return this.key;
    }
}
