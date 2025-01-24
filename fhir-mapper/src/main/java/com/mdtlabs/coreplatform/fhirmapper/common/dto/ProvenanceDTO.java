package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Provenance Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 29, 2024.
 */
@Data
public class ProvenanceDTO {

    private String userId;

    private Long spiceUserId;

    private String organizationId;

    private Date modifiedDate;
}
