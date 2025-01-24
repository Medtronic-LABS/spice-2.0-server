package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

/**
 * This class is for user preferences DTO
 *
 * @author Nandhakumar Karthikeyan created on July 09, 2024
 */
@Data
public class UserPreferencesDTO {

    private Long userId;

    private Object preference;

    private String type;

    private boolean isActive;
}
