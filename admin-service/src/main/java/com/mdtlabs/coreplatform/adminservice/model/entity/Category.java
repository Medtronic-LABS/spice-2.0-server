package com.mdtlabs.coreplatform.adminservice.model.entity;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.adminservice.constants.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

import java.io.Serializable;

@Entity
@Table(name = TableConstants.TABLE_CATEGORY)
@Data
public class Category extends BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    public Category() {
    }

    public Category(String name) {
        this.name = name;
    }

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;
}
