package com.mdtlabs.coreplatform.offlineservice.producer;

import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import io.awspring.cloud.sqs.operations.SqsTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
public class ProducerService {

    private final SqsTemplate sqsTemplate;

    @Value("${app.is-aws-sqs-enabled}")
    public Boolean isAwsSqsEnabled;

    @Value("${app.is-rabbitmq-sqs-enabled}")
    public Boolean isRabbitMqSqsEnabled;

    @Value("${app.mq-producer-channnel}")
    public String producerChannel;

    public final StreamBridge streamBridge;

    public ProducerService(StreamBridge streamBridge, @Autowired(required = false) SqsTemplate sqsTemplate) {
        this.streamBridge = streamBridge;
        this.sqsTemplate = sqsTemplate;
    }


    public void sendMessage(String messageBody, String messageGroupId,
                            Map<String, Object> messageAttributes, String requestQueueURL) {
        if(Boolean.TRUE.equals(isAwsSqsEnabled)) {
            sqsTemplate.send(sqsSendOptions -> sqsSendOptions.queue(requestQueueURL).payload(messageBody)
                    .messageGroupId(messageGroupId)
                    .headers(messageAttributes));
        }
        if(Boolean.TRUE.equals(isRabbitMqSqsEnabled)) {
            messageAttributes.put(Constants.FIELD_MESSAGE_GROUP_ID, messageGroupId);
            MessageHeaders messageHeaders = new MessageHeaders(messageAttributes);
            Message<String> message = MessageBuilder
                    .withPayload(messageBody)
                    .copyHeaders(messageHeaders)
                    .build();

            streamBridge.send(producerChannel, List.of(message));
        }
    }

    public void sendBulkMessage(List<Message<String>> messageEntries, String requestQueueURL)
    {
        if(Boolean.TRUE.equals(isAwsSqsEnabled))
            sqsTemplate.sendMany(requestQueueURL, messageEntries);
        if(Boolean.TRUE.equals(isRabbitMqSqsEnabled))
            streamBridge.send(producerChannel, messageEntries);
    }
}
