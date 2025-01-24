package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This class is an entity class for wgs data table.
 * </p>
 *
 * @author Premkalyan created on Nov 08 2024
 */
@Entity
@Data
@Table(name = TableConstants.TABLE_WGS_DATA)
public class WgsData {

    @Id
    @Column(name = FieldConstants.ID)
    private Long id;

    @Column(name = FieldConstants.INDICATOR)
    private String indicator;

    @Column(name = FieldConstants.SEX)
    private Integer sex;

    @Column(name = FieldConstants.GIVEN)
    private Double given;

    @Column(name = FieldConstants.L)
    private Double l;

    @Column(name = FieldConstants.M)
    private Double m;

    @Column(name = FieldConstants.S)
    private Double s;
}
