package com.mdtlabs.coreplatform.spiceservice.devicedetails.service.impl;


import java.util.Date;
import java.util.Map;
import java.util.Objects;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.repository.DeviceDetailsRepository;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.service.DeviceDetailsService;

/**
 * This class implements the DeviceDetailsService interface and contains actual
 * business logic to perform operations on PatientVisit entity.
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 2026
 */
@Service
public class DeviceDetailsServiceImpl implements DeviceDetailsService {

    private final DeviceDetailsRepository deviceDetailsRepository;

    @Autowired
    public DeviceDetailsServiceImpl(DeviceDetailsRepository deviceDetailsRepository) {
        this.deviceDetailsRepository = deviceDetailsRepository;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Long> validateDeviceDetails(DeviceDetails deviceDetails) {
        DeviceDetails response;
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        UserContextDTO userDto = UserContextHolder.getUserDto();
        DeviceDetails existingDeviceDetails = deviceDetailsRepository
                .findByUserIdAndTenantIdAndDeviceIdAndIsDeletedFalseAndIsActiveTrue(userDto.getId(), deviceDetails.getTenantId(),
                        deviceDetails.getDeviceId());
        if (!Objects.isNull(existingDeviceDetails)) {
            mapper.map(deviceDetails, existingDeviceDetails);
            existingDeviceDetails.setLastLoggedIn(new Date());
            response = deviceDetailsRepository.save(existingDeviceDetails);
        } else {
            deviceDetails.setUserId(userDto.getId());
            deviceDetails.setLastLoggedIn(new Date());
            response = deviceDetailsRepository.save(deviceDetails);
        }
        return Map.of(Constants.ID, response.getId());
    }

    /**
     * {@inheritDoc}
     */
    public DeviceDetails getDeviceByRefId(String refId) {
        return deviceDetailsRepository.findByRefIdAndIsDeletedFalseAndIsActiveTrue(refId);
    }

    /**
     * {@inheritDoc}
     */
    public DeviceDetails findByRefId(String deviceInfoId) {
        return deviceDetailsRepository.findByRefIdAndIsDeletedFalseAndIsActiveTrue(deviceInfoId);
    }

}