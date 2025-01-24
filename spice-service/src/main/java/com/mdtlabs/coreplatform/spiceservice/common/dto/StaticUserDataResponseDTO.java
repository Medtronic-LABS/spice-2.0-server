package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO.DistrictDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HealthFacilityDTO.ChiefdomDTO;

import com.mdtlabs.coreplatform.spiceservice.common.model.Menu;
import lombok.Data;

/**
 * This a DTO class for user static data.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class StaticUserDataResponseDTO {
    
    private List<HealthFacilityDTO> nearestHealthFacilities;

    private List<VillageDTO> villages;

    private HealthFacilityDTO defaultHealthFacility;

    private Menu menu;

    private UserResponseDTO userProfile;

    private Set<ClinicalWorkflow> clinicalWorkflows;

    private Set<Long> workflowIds;

    private Map<String, String> consentForm;

    private List<HealthFacilityDTO> userHealthFacilities;

    private List<UserResponseDTO> mobileUsers;

    private List<HealthFacilityDTO> healthFacilities;

    private Boolean smartAncEnabled;

    private List<FrequencyDTO> frequency;

    private List<DistrictDTO> districts;

    private List<ChiefdomDTO> chiefdoms;

    private List<Map<String, String>> identityTypes;

    private List<Program> programs;

    private List<Culture> cultures;

    private int remainingAttemptsCount;

    private List<String> appTypes;
}
