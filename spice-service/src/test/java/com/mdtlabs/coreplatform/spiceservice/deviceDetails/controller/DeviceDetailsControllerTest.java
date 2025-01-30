package com.mdtlabs.coreplatform.spiceservice.deviceDetails.controller;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;

import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.DeviceDetailsDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.controller.DeviceDetailsController;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.service.DeviceDetailsService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DeviceDetailsControllerTest {

    @InjectMocks
    private DeviceDetailsController deviceDetailsController;

    @Mock
    private DeviceDetailsService deviceDetailsService;

    @Mock
    private ModelMapper modelMapper;

    @Test
    void validateDeviceDetails() {
        DeviceDetailsDTO deviceDetailsDTO = new DeviceDetailsDTO();
        when(deviceDetailsService.validateDeviceDetails(modelMapper.map(deviceDetailsDTO, DeviceDetails.class))).thenReturn(Map.of());
        deviceDetailsController.validateDeviceDetails(deviceDetailsDTO);
        verify(deviceDetailsService, atLeastOnce()).validateDeviceDetails(modelMapper.map(deviceDetailsDTO, DeviceDetails.class));
    }

    @Test
    void getDeviceByRefId() {
        String refId = TestConstants.STRING_ONE;
        DeviceDetails deviceDetails = new DeviceDetails();
        when(deviceDetailsService.getDeviceByRefId(refId)).thenReturn(deviceDetails);
        deviceDetailsController.getDeviceByRefId(refId);
        verify(deviceDetailsService, atLeastOnce()).getDeviceByRefId(refId);
    }
}
