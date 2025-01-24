package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.Date;

@Data
public class PrescriberDetailsDTO {

    private String firstName;

    private String lastName;

    private String phoneNumber;

    private Date lastRefillDate;

    private String lastRefillVisitId;
}
