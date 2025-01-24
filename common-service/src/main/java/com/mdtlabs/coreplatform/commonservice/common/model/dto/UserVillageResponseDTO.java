package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

/**
 * This class is for user preferences DTO
 *
 * @author Nandhakumar Karthikeyan created on July 31, 2024
 */
@Data
@NoArgsConstructor
public class UserVillageResponseDTO {

    private Long id;

    private String username;

    private String name;

    private List<VillageDTO> villages;

    private String fhirId;

    private String firstName;

    private String lastName;

    public UserVillageResponseDTO(Long id, String username, String firstName,
                                                            String lastName, String fhirId) {
        this.id = id;
        this.username = username;
        this.name = StringUtil.concatString(firstName, Constants.SPACE, lastName);
        this.fhirId = fhirId;
        this.firstName = firstName;
        this.lastName = lastName;
        this.villages = new ArrayList<>();
    }

}
