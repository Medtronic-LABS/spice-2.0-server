package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * This class is for user preferences DTO
 *
 * @author Nandhakumar Karthikeyan created on July 31, 2024
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserVillageDTO {

    private Long id;

    private String firstName;

    private String lastName;

    private Long villageId;

    private String villageName;

    private String fhirId;

}
