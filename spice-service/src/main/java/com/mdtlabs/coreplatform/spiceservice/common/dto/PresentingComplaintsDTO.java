package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Presenting Complaints.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
public class PresentingComplaintsDTO {

    private Long id;

    private String name;

    private Integer displayOrder;

    private String type;

    private String value;
}
