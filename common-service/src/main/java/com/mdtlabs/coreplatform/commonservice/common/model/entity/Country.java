package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import org.hibernate.annotations.Type;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * The Country class represents a country entity with properties such as name, country code, unit
 * measurement, and region code.
 * </p>
 *
 * @author Karthick
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_COUNTRY)
@NoArgsConstructor
public class Country extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.PHONE_NUMBER_CODE)
    private String phoneNumberCode;

    @Column(name = FieldConstants.UNIT_MEASUREMENT)
    private String unitMeasurement;

    @Column(name = FieldConstants.REGION_CODE)
    private String regionCode;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;

    @Column(name = FieldConstants.DISPLAY_VALUES)
    @Type(value = JsonBinaryType.class)
    private Map<String, Object> displayValues;

    public Country(String name) {
        this.name = name;
    }
}
