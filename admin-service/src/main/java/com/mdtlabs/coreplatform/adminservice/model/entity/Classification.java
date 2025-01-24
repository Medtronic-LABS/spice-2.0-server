package com.mdtlabs.coreplatform.adminservice.model.entity;


import java.util.List;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.adminservice.constants.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This class is an Entity represent Classification fields.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_CLASSIFICATION)
public class Classification extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;

    @OneToMany
    @JoinTable(name = TableConstants.MERGE_TABLE_CLASSIFICATION_BRAND, joinColumns = {
            @JoinColumn(name = FieldConstants.CLASSIFICATION_ID)}, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.BRAND_ID)
    }
    )
    private List<Brand> brands;

    @ManyToOne
    @JoinTable(name = TableConstants.MERGE_TABLE_COUNTRY_CLASSIFICATION, joinColumns = {
            @JoinColumn(name = FieldConstants.CLASSIFICATION_ID)}, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.COUNTRY_ID)
    }
    )
    private Country country;


    public Classification() {
    }

    public Classification(String name) {
        this.name = name;
    }
}
