package com.mdtlabs.coreplatform.spiceservice.devicedetails.controller;

import java.util.Map;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DeviceDetailsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.service.DeviceDetailsService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * This controller class performs the REST operations on DeviceDetails entity.
 *
 * @author Tamilarasi Shanmugasundaram created on Nov 2026
 */
@RestController
@RequestMapping(value = "/devicedetails")
@Validated
public class DeviceDetailsController {

    private final DeviceDetailsService deviceDetailsService;
    private final ModelMapper modelMapper;

    @Autowired
    public DeviceDetailsController(DeviceDetailsService deviceDetailsService, ModelMapper modelMapper) {
        this.deviceDetailsService = deviceDetailsService;
        this.modelMapper = modelMapper;
    }

    /**
     * This method is used to validate device details.
     *
     * @param deviceDetailsDto - device details entity
     * @return deviseDetails Entity.
     */
    @PostMapping
    @TenantValidation
    public SuccessResponse<Map<String, Long>> validateDeviceDetails(@RequestBody DeviceDetailsDTO deviceDetailsDto) {
        return new SuccessResponse<>(SuccessCode.DEVICE_DETAILS_SAVE,
                deviceDetailsService.validateDeviceDetails(modelMapper.map(deviceDetailsDto, DeviceDetails.class)),
                HttpStatus.CREATED);
    }

    /**
     * This method is used to retrieve the device details by referenceId.
     *
     * @param refId - referenceId
     * @return deviseDetails  Entity.
     */
    @GetMapping(value = "/get-by-refid/{refId}")
    public DeviceDetails getDeviceByRefId(@PathVariable("refId") String refId) {
        return deviceDetailsService.getDeviceByRefId(refId);
    }

}
