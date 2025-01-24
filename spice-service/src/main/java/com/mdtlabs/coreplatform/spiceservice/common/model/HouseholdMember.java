package com.mdtlabs.coreplatform.spiceservice.common.model;


import java.util.Date;

import lombok.Data;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Data
@Entity
@Table(name = TableConstants.HOUSEHOLD_MEMBER)
public class HouseholdMember extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.PHONE_NUMBER)
    private String phoneNumber;

    @Column(name = FieldConstants.PHONE_NUMBER_CATEGORY)
    private String phoneNumberCategory;

    @Column(name = FieldConstants.DATE_OF_BIRTH)
    private Date dateOfBirth;

    @Column(name = FieldConstants.NATIONAL_ID)
    private String nationalId;

    @Column(name = FieldConstants.GENDER)
    private String gender;

    @Column(name = FieldConstants.HOUSEHOLD_RELATIONSHIP)
    private String householdRelationship;

    @Column(name = FieldConstants.HOUSEHOLD_ID)
    private Long householdId;
}
