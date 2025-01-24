package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

@Data
public class CallRegisterDetailDTO implements Serializable {

    private Date callDate;

    private int duration;
}
