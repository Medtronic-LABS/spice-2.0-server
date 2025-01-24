package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;

/**
 * This DTO class for healthfacility entity.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class HealthFacilityDTO {

    private Long id;

    private String name;

    private String type;

    private ChiefdomDTO chiefdom;

    private DistrictDTO district;

    private String latitude;

    private String longitude;

    private String postalCode;

    private String language;

    private Long tenantId;

    private String fhirId;

    private List<ClinicalWorkflow> clinicalWorkflows;

    private List<ClinicalWorkflow> customizedWorkflows;


    private List<VillageDTO> linkedVillages;

    @Data
    public static class ChiefdomDTO {
        private Long id;

        private String name;

        private String code;

        private Long countryId;

        private Long districtId;

    }

    @Data
    public static class DistrictDTO {
        private Long id;

        private String name;

        private String code;

        private Long countryId;

    }
}
