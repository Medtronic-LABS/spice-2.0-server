package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.Map;

@Data
public class ConsumerData
{
    private String payload;
    private Map<String,Object> headers;
}
