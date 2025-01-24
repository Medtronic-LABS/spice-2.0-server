package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This is a entity class for Household Member Link details.
 *
 * @author Denisha J created On 09 Oct 2024
 */
@Entity
@Data
@Table(name = TableConstants.TABLE_HOUSEHOLD_MEMBER_LINK)
public class HouseholdMemberLink extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.MEMBER_ID)
    private String memberId;

    @Column(name = FieldConstants.PATIENT_ID)
    private String patientId;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.VILLAGE_ID)
    private String villageId;

    @Column(name = FieldConstants.HOUSEHOLD_ID)
    private String householdId;

    @Column(name = FieldConstants.STATUS)
    private String status;

    @Column(name = FieldConstants.IS_SHOW)
    private Boolean isShow = true;
}
