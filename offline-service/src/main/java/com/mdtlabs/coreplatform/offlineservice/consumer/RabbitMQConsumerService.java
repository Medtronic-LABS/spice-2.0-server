package com.mdtlabs.coreplatform.offlineservice.consumer;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.offlineservice.common.dto.ConsumerData;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.OfflineSyncService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.function.Consumer;

/**
 * <p>
 * This class is a service class to perform operation on listen queue from Rabbit MQ
 * </p>
 *
 * @author jayarathinam created on Sep 18, 2024
 */
@Service
@ConditionalOnProperty(name = "app.is-rabbitmq-sqs-enabled", havingValue = "true", matchIfMissing = true)
public class RabbitMQConsumerService implements Consumer<Message<String>>
{

    OfflineSyncService offlineSyncService;

    ObjectMapper objectMapper;

    public RabbitMQConsumerService(OfflineSyncService offlineSyncService, ObjectMapper objectMapper){
        this.offlineSyncService = offlineSyncService;
        this.objectMapper = objectMapper;
    }

    /**
     *  Poll data from queue.
     *  @param messages the content from queue
     */
    @Override
    public void accept(Message<String> messages) {
        try {
            Logger.logInfo("Listening Data from Rabbit MQ");
            List<ConsumerData> consumerArr = objectMapper.readValue(messages.getPayload(), new TypeReference<List<ConsumerData>>() {});

            consumerArr.forEach( message ->
                    offlineSyncService.processRequestQueue(message.getPayload(), new MessageHeaders(message.getHeaders())));

        } catch (Exception e) {
            Logger.logError(StringUtil.concatString("Error processing message from request queue(Rabbit MQ): ", e.getMessage()));
        }

    }

}
