package com.mdtlabs.coreplatform.spiceservice.devicedetails.service;

import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;

/**
 * <p>
 * This is an interface to perform any actions in deviceDetails related
 * entities.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 2026
 *
 */
public interface DeviceDetailsService {

    /**
     * <p>
     * Validates the provided device details and returns a map containing validation results.
     * The map includes error codes or success indicators for various validation checks.
     * </p>
     *
     * @param deviceDetails The {@link DeviceDetails} object to be validated.
     * @return A {@link Map} with validation results. The map keys are validation criteria
     *         (such as field names), and the values represent validation error codes or
     *         success indicators (such as error counts or flags).
     */
    public Map<String, Long> validateDeviceDetails(DeviceDetails deviceDetails);

    /**
     * <p>
     * Retrieves the device details associated with the specified reference ID.
     * </p>
     *
     * @param refId The reference ID of the device to retrieve.
     * @return A {@link DeviceDetails} object containing the details of the device,
     *         or null if no device with the specified reference ID is found.
     */
    public DeviceDetails getDeviceByRefId(String refId);

    /**
     * <p>
     * Retrieves the device details associated with the specified device information ID (refId).
     * </p>
     * 
     * @param deviceInfoId The unique identifier (refId) of the device to retrieve.
     * @return A {@link DeviceDetails} object containing the details of the device,
     *         or null if no device with the specified ID is found.
     */
    public DeviceDetails findByRefId(String deviceInfoId);

}
