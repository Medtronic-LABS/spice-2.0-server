package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;

import lombok.Data;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

@Data
@Entity
@Table(name = TableConstants.HOUSEHOLD)
public class Household extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.VILLAGE_ID)
    private Integer villageId;

    @Column(name = FieldConstants.LANDMARK)
    private String landmark;

    @Column(name = FieldConstants.HEAD_PHONE_NUMBER)
    private String headPhoneNumber;

    @Column(name = FieldConstants.NO_OF_PEOPLE)
    private Integer noOfPeople;

    @Column(name = FieldConstants.OWNS_AN_IMPROVED_LATRINE)
    private Boolean ownedAnImprovedLatrine;

    @Column(name = FieldConstants.OWNS_HAND_WASHING_FACILITY_WITH_SOAP)
    private Boolean ownedHandWashingFacilityWithSoap;

    @Column(name = FieldConstants.OWNS_TREATED_BED_NET)
    private Boolean ownedTreatedBedNet;

    @Column(name = FieldConstants.BED_NET_COUNT)
    private Integer bedNetCount;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = FieldConstants.HOUSEHOLD_ID)
    private List<HouseholdMember> householdMembers;
}
