package com.mdtlabs.coreplatform.offlineservice.common.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for a household.
 * </p>
 *
 */
@Data
public class HouseholdDTO implements Serializable {

    private String id;

    private String referenceId;

    private Long householdNo;

    private String name;

    private String village;

    private String villageId;

    private String landmark;

    private String headPhoneNumber;

    private String headPhoneNumberCategory;

    private int noOfPeople;

    private boolean ownedHandWashingFacilityWithSoap;

    private boolean ownedTreatedBedNet;

    private boolean ownedAnImprovedLatrine;

    private Integer bedNetCount;

    private List<HouseholdMemberDTO> householdMembers;

    private String type;

    private String version;

    private Date lastUpdated;

    private ProvenanceDTO provenance;

    private Double latitude;

    private Double longitude;
}