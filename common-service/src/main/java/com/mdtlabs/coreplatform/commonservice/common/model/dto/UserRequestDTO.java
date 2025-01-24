package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

@Data
public class UserRequestDTO {

    private Long id;

    private String firstName;

    private String middleName;

    private String lastName;

    private String gender;

    private String phoneNumber;

    private String username;

    private String countryCode;

    private List<Long> roleIds;

    private String fhirId;

    private Long tenantId;

    private List<Long> villageIds;

    private List<Village> villages;

    private Country country;

    private Timezone timezone;

    private Long supervisorId;

    private List<Long> insightUserOrganizationIds;

    private List<Long> reportUserOrganizationIds;

    private List<Organization> insightUserOrganization;

    private List<Organization> reportUserOrganization;

    private Long communityUnitId;

    private Culture culture;

    private Designation designation;

    private List<String> appTypes;

}
