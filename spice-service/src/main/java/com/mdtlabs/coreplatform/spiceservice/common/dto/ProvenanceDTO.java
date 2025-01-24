package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * <p>
 * This is a DTO class for Provenance Details.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 29, 2024.
 */
@Data
@NoArgsConstructor
public class ProvenanceDTO {

    private String userId;

    private Long spiceUserId;

    private String organizationId;

    private Date modifiedDate;

    public ProvenanceDTO(String userId, String organizationId, Date modifiedDate) {
        this.userId = userId;
        this.organizationId = organizationId;
        this.modifiedDate = modifiedDate;
    }

}
