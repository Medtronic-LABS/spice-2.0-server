package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.Data;
import lombok.NoArgsConstructor;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

/**
 * <p>
 * The Village class represents a village entity with various attributes such as name, code, and
 * associations with other entities like Country, District, Chiefdom, and HealthFacility.
 * </p>
 */
@Entity
@Data
@Table(name = TableConstants.TABLE_VILLAGE)
@NoArgsConstructor
public class Village extends BaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.CODE)
    private String code;

    @Column(name = FieldConstants.TYPE)
    private String type;
    
    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.DISTRICT_ID)
    private Long districtId;

    @Column(name = FieldConstants.CHIEFDOM_ID)
    private Long chiefdomId;

    @Column(name = FieldConstants.MEMBER_SEQUENCE)
    private long memberSequence = 0;

    @Column(name = FieldConstants.HOUSEHOLD_SEQUENCE)
    private long householdSequence = 0;

	public Village(String name, String code, String type, Long countryId, Long districtId, Long chiefdomId) {
		this.name = name;
		this.code = code;
		this.type = type;
		this.countryId = countryId;
		this.districtId = districtId;
		this.chiefdomId = chiefdomId;
	}

    public Village(String name, Long countryId, Long districtId, Long chiefdomId) {
        this.name = name;
        this.countryId = countryId;
        this.districtId = districtId;
        this.chiefdomId = chiefdomId;
    }
}
