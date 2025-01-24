package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
public class HouseholdMemberLinkDTO implements Serializable {

    private String memberId;

    private String patientId;

    private String villageId;

    private String status;

    private Boolean isShow;

    private List<CallRegisterDetailDTO> callRegisterDetail = new ArrayList<>();

    private ProvenanceDTO provenance;
}
