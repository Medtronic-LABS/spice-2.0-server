package com.mdtlabs.coreplatform.spiceservice.devicedetails.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the user module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 2026
 */
@Repository
public interface DeviceDetailsRepository extends JpaRepository<DeviceDetails, Long> {


    /**
     * <p>
     * Finds a device's details based on the specified user ID, tenant ID, device ID,
     * and ensures the device is not deleted and is active.
     * </p>
     *
     * @param userId The ID of the user associated with the device.
     * @param tenantId The ID of the tenant associated with the device.
     * @param deviceId The unique identifier of the device.
     * @return A {@link DeviceDetails} object containing the details of the device
     *         if found, or null if no matching device is found.
     */
    DeviceDetails findByUserIdAndTenantIdAndDeviceIdAndIsDeletedFalseAndIsActiveTrue(Long userId, Long tenantId, String deviceId);

    /**
     * <p>
     * Retrieves the device details based on the specified reference ID,
     * ensuring the device is not deleted and is currently active.
     * </p>
     *
     * @param refId The reference ID associated with the device.
     * @return A {@link DeviceDetails} object containing the details of the device
     *         if found, or null if no matching device is found.
     */
    DeviceDetails findByRefIdAndIsDeletedFalseAndIsActiveTrue(String refId);
}
