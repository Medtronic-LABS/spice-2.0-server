package com.mdtlabs.coreplatform.offlineservice.producer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.mdtlabs.coreplatform.offlineservice.common.TestConstants;
import com.mdtlabs.coreplatform.offlineservice.common.TestDataProvider;
import io.awspring.cloud.sqs.operations.SendResult;
import io.awspring.cloud.sqs.operations.SqsTemplate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * <p>
 * This class has the test methods for Producer Service class.
 * </p>
 *
 * @author Praveen created on Mar 29, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ProducerServiceTest {

    @InjectMocks
    ProducerService producerService;

    @Mock
    private SqsTemplate sqsTemplate;

    @Mock
    private StreamBridge streamBridge;

    @Test
    void sendMessage() throws JsonProcessingException {
        //given
        Map<String, Object> messageAttributes = new HashMap<>();
        String serializedData = TestDataProvider.serializeData(TestDataProvider.getHouseholdData());
        ReflectionTestUtils.setField(producerService, TestConstants.ISAWSSQSENABLED, Boolean.TRUE);
        ReflectionTestUtils.setField(producerService, TestConstants.ISRABBITMQSQSENABLED, Boolean.TRUE);

        //when
        when(sqsTemplate.send(any())).thenReturn(mock(SendResult.class));
        when(streamBridge.send(anyString(), anyList())).thenReturn(true);

        //then
        producerService.sendMessage(serializedData, TestConstants.GROUP_ID,
                messageAttributes, TestConstants.QUEUE_URL);
        verify(sqsTemplate, atLeastOnce()).send(any());
    }

    @Test
    void sendMessageWhenIsAwsSqsEnabledAndIsRabbitMqSqsEnabledIsFalse() throws JsonProcessingException {
        //given
        Map<String, Object> messageAttributes = new HashMap<>();
        String serializedData = TestDataProvider.serializeData(TestDataProvider.getHouseholdData());
        ReflectionTestUtils.setField(producerService, TestConstants.ISAWSSQSENABLED, Boolean.FALSE);
        ReflectionTestUtils.setField(producerService, TestConstants.ISRABBITMQSQSENABLED, Boolean.FALSE);

        //then
        producerService.sendMessage(serializedData, TestConstants.GROUP_ID,
                messageAttributes, TestConstants.QUEUE_URL);
        verify(sqsTemplate, never()).send(any());
    }

    @Test
    void sendBulkMessage() throws JsonProcessingException {
        //given
        String serializedData = TestDataProvider.serializeData(new HashMap<>());
        Message<String> message = MessageBuilder
                .withPayload(serializedData)
                .copyHeaders(TestDataProvider.getMessageHeader(TestConstants.ONE))
                .build();
        List<Message<String>> messageEntries = List.of(message);
        ReflectionTestUtils.setField(producerService, TestConstants.ISAWSSQSENABLED, Boolean.TRUE);
        ReflectionTestUtils.setField(producerService, TestConstants.ISRABBITMQSQSENABLED, Boolean.TRUE);

        when(sqsTemplate.send(any())).thenReturn(mock(SendResult.class));
        when(streamBridge.send(anyString(), anyList())).thenReturn(true);

        //then
        producerService.sendBulkMessage(messageEntries, TestConstants.QUEUE_URL);
        verify(sqsTemplate, atLeastOnce()).sendMany(TestConstants.QUEUE_URL, messageEntries);
    }

    @Test
    void sendBulkMessageWhenIsAwsSqsEnabledAndIsRabbitMqSqsEnabledIsFalse() throws JsonProcessingException {
        //given
        String serializedData = TestDataProvider.serializeData(new HashMap<>());
        Message<String> message = MessageBuilder
                .withPayload(serializedData)
                .copyHeaders(TestDataProvider.getMessageHeader(TestConstants.ONE))
                .build();
        List<Message<String>> messageEntries = List.of(message);
        ReflectionTestUtils.setField(producerService, TestConstants.ISAWSSQSENABLED, Boolean.FALSE);
        ReflectionTestUtils.setField(producerService, TestConstants.ISRABBITMQSQSENABLED, Boolean.FALSE);

        //then
        producerService.sendBulkMessage(messageEntries, TestConstants.QUEUE_URL);
        verify(sqsTemplate, never()).sendMany(TestConstants.QUEUE_URL, messageEntries);

    }
}
