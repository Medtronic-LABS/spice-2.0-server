package com.mdtlabs.coreplatform.adminservice.model.dto;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CultureDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;

import lombok.Data;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;

/**
 * This is a DTO class for request handling.
 *
 * @author Karthick M Created on 30 Jun 2024
 */
@Data
public class HealthFacilityDTO {

    private Long id;

    private String name;

    private String type;

    private String phuFocalPersonName;

    private String phuFocalPersonNumber;

    private String address;

    private ChiefdomDTO chiefdom;

    private DistrictDTO district;

    private String cityName;

    private String latitude;

    private String longitude;

    private String postalCode;

    private String language;

    private List<VillageDTO> linkedVillages;

    private Long tenantId;

    private String fhirId;

    private List<ClinicalWorkflow> clinicalWorkflows;

    private List<ClinicalWorkflow> customizedWorkflows;

    private List<UserResponseDTO> peerSupervisors;

    private Culture culture;

    public CultureDTO getCulture() {
        if (!Objects.isNull(this.culture)) {
            return new ModelMapper().map(this.culture, CultureDTO.class);
        }
        return null;
    }

    public List<ClinicalWorkflowDTO> getClinicalWorkflows() {
        if (!Objects.isNull(this.clinicalWorkflows)) {
            return new ModelMapper().map(this.clinicalWorkflows, new TypeToken<List<ClinicalWorkflowDTO>>() {
            }.getType());
        }
        return Collections.emptyList();
    }

    public List<ClinicalWorkflowDTO> getCustomizedWorkflows() {
        if (!Objects.isNull(this.customizedWorkflows)) {
            return new ModelMapper().map(this.customizedWorkflows, new TypeToken<List<ClinicalWorkflowDTO>>() {
            }.getType());
        }
        return Collections.emptyList();
    }
}
