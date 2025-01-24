package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This is a entity class for Presenting Complaints.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Entity
@Data
@Table(name = TableConstants.PRESENTING_COMPLAINTS)
public class PresentingComplaints extends BaseEntity {


    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Integer displayOrder;

    @Column(name = FieldConstants.VALUE)
    private String value;
}
