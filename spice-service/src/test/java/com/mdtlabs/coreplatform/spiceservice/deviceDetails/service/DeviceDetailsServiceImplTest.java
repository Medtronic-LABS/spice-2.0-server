package com.mdtlabs.coreplatform.spiceservice.deviceDetails.service;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;

import static org.mockito.Mockito.*;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.repository.DeviceDetailsRepository;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.service.impl.DeviceDetailsServiceImpl;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DeviceDetailsServiceImplTest {
    @InjectMocks
    private DeviceDetailsServiceImpl deviceDetailsServiceImpl;
    @Mock
    private DeviceDetailsRepository deviceDetailsRepository;
    @Mock
    private ModelMapper modelMapper;

    @Test
    void getDeviceByRefId() {
        String refId = TestConstants.STRING_ONE;
        DeviceDetails deviceDetails = new DeviceDetails();
        when(deviceDetailsRepository.findByRefIdAndIsDeletedFalseAndIsActiveTrue(refId)).thenReturn(deviceDetails);
        deviceDetailsServiceImpl.getDeviceByRefId(refId);
        verify(deviceDetailsRepository, atLeastOnce()).findByRefIdAndIsDeletedFalseAndIsActiveTrue(refId);
    }

    @Test
    void findByRefId() {
        String deviceInfoId = TestConstants.STRING_ONE;
        DeviceDetails deviceDetails = new DeviceDetails();
        when(deviceDetailsRepository.findByRefIdAndIsDeletedFalseAndIsActiveTrue(deviceInfoId)).thenReturn(deviceDetails);
        deviceDetailsServiceImpl.findByRefId(deviceInfoId);
        verify(deviceDetailsRepository, atLeastOnce()).findByRefIdAndIsDeletedFalseAndIsActiveTrue(deviceInfoId);

    }

    @Test
    void validateDeviceDetails() {
        TestDataProvider.init();
        DeviceDetails existingDeviceDetails = new DeviceDetails();
        DeviceDetails deviceDetails = new DeviceDetails();
        deviceDetails.setRefId(TestConstants.STRING_ONE);
        deviceDetails.setId(1L);
        existingDeviceDetails.setLastLoggedIn(new Date());
        existingDeviceDetails.setTenantId(1L);
        existingDeviceDetails.setDeviceId(TestConstants.STRING_ONE);
        existingDeviceDetails.setId(1L);
        UserContextDTO userDto = new UserContextDTO();
        userDto.setId(1L);
        TestDataProvider.getStaticMock();
        when(deviceDetailsRepository
                .findByUserIdAndTenantIdAndDeviceIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE, existingDeviceDetails.getTenantId(),
                        existingDeviceDetails.getDeviceId())).thenReturn(existingDeviceDetails);
        when(deviceDetailsRepository.save(existingDeviceDetails)).thenReturn(existingDeviceDetails);
        deviceDetailsServiceImpl.validateDeviceDetails(existingDeviceDetails);
        verify(deviceDetailsRepository, atLeastOnce()).findByUserIdAndTenantIdAndDeviceIdAndIsDeletedFalseAndIsActiveTrue(TestConstants.ONE, existingDeviceDetails.getTenantId(),
                existingDeviceDetails.getDeviceId());
        TestDataProvider.cleanUp();
    }
}
