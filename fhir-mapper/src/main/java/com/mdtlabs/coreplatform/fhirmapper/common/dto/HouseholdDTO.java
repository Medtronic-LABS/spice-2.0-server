package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for Household entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on Feb 05, 2024.
 */
@Data
public class HouseholdDTO {

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

    private String version;

    private Date lastUpdated;

    private ProvenanceDTO provenance;

    private Double latitude;

    private Double longitude;

}