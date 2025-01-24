package com.mdtlabs.coreplatform.cqlservice.model.dto;

import java.util.Date;
import java.util.List;
import java.util.Set;

import lombok.Data;

/**
 * <p>
 * This is the DTO class for handling the cql requests.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024.
 */
@Data
public class CqlRequestDTO {

    private String resourceBundle;

    private Set<String> libraries;

    private Set<String> expressions;

    private List<String> villageIds;

    private Date lastSyncTime;

    private Date currentSyncTime;
}
