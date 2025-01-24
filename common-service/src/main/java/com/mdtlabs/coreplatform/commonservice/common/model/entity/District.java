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
 * The District class represents a district entity with properties such as name, code, and a reference
 * to a country entity.
 * </p>
 */
@Entity
@Table(name = TableConstants.TABLE_DISTRICT)
@Data
@NoArgsConstructor
public class District extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.CODE)
    private String code;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.STATUS)
    private String status;

    @Column(name = FieldConstants.REASON)
    private String reason;

    public District(String name) {
        this.name = name;
    }
    public District(String name, String code, Long countryId, Long tenantId) {
        this.name = name;
        this.code = code;
        this.countryId = countryId;
        this.tenantId = tenantId;
    }

    public District(String name, Long countryId, Long tenantId) {
        this.name = name;
        this.countryId = countryId;
        this.tenantId = tenantId;
    }
}
