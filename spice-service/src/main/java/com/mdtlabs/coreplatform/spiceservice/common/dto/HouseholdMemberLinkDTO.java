package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class HouseholdMemberLinkDTO {

    private Long id;

    private String memberId;

    private String patientId;

    private String householdId;

    private String name;

    private String villageId;

    private String status;

    private Boolean isShow;

    private List<CallRegisterDetailDTO> callRegisterDetail = new ArrayList<>();

    private ProvenanceDTO provenance;

}
