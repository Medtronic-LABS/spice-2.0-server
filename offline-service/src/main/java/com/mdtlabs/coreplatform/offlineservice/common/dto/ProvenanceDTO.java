package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.util.Date;

/**
 * <p>
 * This is a DTO class for Provenance Details.
 * </p>
 *
 */
@Data
public class ProvenanceDTO {

    private String userId;

    private Long spiceUserId;

    private String organizationId;

    private Date modifiedDate;

}
