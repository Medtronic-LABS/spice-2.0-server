package com.mdtlabs.coreplatform.adminservice.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.adminservice.constants.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;

import lombok.Data;

/**
 * <p>
 * This class is an Entity represent DosageForm fields.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_DOSAGE_FORM)
public class DosageForm extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private int displayOrder;

    public DosageForm() {
    }

    public DosageForm(String name) {
        this.name = name;
    }

}
