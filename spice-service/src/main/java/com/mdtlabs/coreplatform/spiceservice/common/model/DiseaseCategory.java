package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This is a entity class for Disease Category.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Entity
@Data
@Table(name = TableConstants.DISEASE_CATEGORY)
public class DiseaseCategory extends MetaBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Integer displayOrder;

    @Column(name = FieldConstants.VALUE)
    private String value;

    @OneToMany
    @JoinColumn(name = FieldConstants.DISEASE_ID)
    private List<DiseaseCondition> diseaseCondition;

    @Column(name = FieldConstants.TYPE)
    private String type;

}
