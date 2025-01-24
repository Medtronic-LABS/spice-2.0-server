package com.mdtlabs.coreplatform.offlineservice.consumer;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.offlineservice.offlinesync.service.OfflineSyncService;
import io.awspring.cloud.sqs.annotation.SqsListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.handler.annotation.Headers;
import org.springframework.stereotype.Service;

/**
 * <p>
 * This class is a service class to perform operation on listen queue
 * </p>
 *
 * @author Gopinath created on Feb 14, 2024
 */
@Service
@ConditionalOnProperty(name = "app.is-aws-sqs-enabled", havingValue = "true", matchIfMissing = false)
public class AWSConsumerService {

    OfflineSyncService offlineSyncService;

    public AWSConsumerService(OfflineSyncService offlineSyncService){
        this.offlineSyncService = offlineSyncService;
    }

    /**
     * Load request queue based on the name of the request queue.
     *
     * @param message - Content of the message
     * @param headers - Request headers
     */
    @SqsListener(value = "${cloud.aws.sqs.name.requestQueue}")
    public void loadMessagesFromRequestQueue(String message, @Headers MessageHeaders headers) {
        try {
            Logger.logInfo("Listening request queue from AWS");
            offlineSyncService.processRequestQueue(message, headers);
        } catch (Exception e) {
            Logger.logError(StringUtil.concatString("Error processing message from request queue: ", e.getMessage()));
        }
    }
}
