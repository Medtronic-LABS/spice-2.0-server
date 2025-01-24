package com.mdtlabs.coreplatform.spiceservice.deviceDetails.service;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.DeviceDetails;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.repository.DeviceDetailsRepository;
import com.mdtlabs.coreplatform.spiceservice.devicedetails.service.impl.DeviceDetailsServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;

import java.util.Date;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DeviceDetailsServiceImplTest {
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
    }

    @Test
    void findByRefId() {
        String deviceInfoId = TestConstants.STRING_ONE;
        DeviceDetails deviceDetails = new DeviceDetails();
        when(deviceDetailsRepository.findByRefIdAndIsDeletedFalseAndIsActiveTrue(deviceInfoId)).thenReturn(deviceDetails);
        deviceDetailsServiceImpl.findByRefId(deviceInfoId);
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
        TestDataProvider.cleanUp();

    }
}
