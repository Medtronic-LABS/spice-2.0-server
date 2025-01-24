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
 * The Chiefdom class represents a chiefdom entity with properties such as name, code, country, and
 * district.
 * </p>
 */
@Entity
@Table(name = TableConstants.TABLE_CHIEFDOM)
@Data
@NoArgsConstructor
public class Chiefdom extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.CODE)
    private String code;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.DISTRICT_ID)
    private Long districtId;

    public Chiefdom(String name) {
        this.name = name;
    }

    public Chiefdom(String name, String code, Long countryId, Long districtId, Long tenantId) {
        this.name = name;
        this.code = code;
        this.countryId = countryId;
        this.districtId = districtId;
        this.tenantId = tenantId;
    }

    public Chiefdom(String name, Long countryId, Long districtId, Long tenantId) {
        this.name = name;
        this.countryId = countryId;
        this.districtId = districtId;
        this.tenantId = tenantId;
    }
}
