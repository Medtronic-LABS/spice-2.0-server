package com.mdtlabs.coreplatform.adminservice.model.entity;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

import java.util.List;

/**
 * This is class for health facility types.
 */
@Data
@Entity
@Table(name = FieldConstants.TABLE_HEALTH_FACILITY_TYPE)
public class HealthFacilityTypes extends BaseEntity {

    private static final long serialVersionUID = 4174505913611242103L;

    @Id
    @Column(name = FieldConstants.ID)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name= FieldConstants.APP_TYPES)
    private List<String> appTypes;
}
