package com.mdtlabs.coreplatform.offlineservice.consumer;

import com.fasterxml.jackson.core.JsonProcessingException;

import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import com.mdtlabs.coreplatform.offlineservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.OfflineSyncService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.messaging.MessageHeaders;

import java.util.HashMap;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * <p>
 * This class has the test methods for Consumer Service class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ConsumerServiceTest {

    @InjectMocks
    AWSConsumerService consumerService;

    @Mock
    private OfflineSyncService offlineSyncService;

    @Test
    void loadMessagesFromRequestQueue() throws JsonProcessingException {
        //given
        MessageHeaders messageHeaders = TestDataProvider.getMessageHeader(TestConstants.ONE);
        String serializedData = TestDataProvider.serializeData(new HashMap<>());

        //then
        consumerService.loadMessagesFromRequestQueue(serializedData, messageHeaders);
        verify(offlineSyncService, atLeastOnce()).processRequestQueue(serializedData, messageHeaders);
        doThrow(new RuntimeException(TestConstants.EXCEPTION)).when(offlineSyncService).processRequestQueue(serializedData, messageHeaders);
        consumerService.loadMessagesFromRequestQueue(serializedData, messageHeaders);
        verify(offlineSyncService, times(2)).processRequestQueue(serializedData, messageHeaders);
    }
}
