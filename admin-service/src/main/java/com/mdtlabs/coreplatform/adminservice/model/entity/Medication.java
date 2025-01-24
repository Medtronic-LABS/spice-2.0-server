package com.mdtlabs.coreplatform.adminservice.model.entity;

import java.io.Serializable;
import java.util.List;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.adminservice.constants.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;

/**
 * <p>
 * This class is an Entity represent Medication fields.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_MEDICATION)
public class Medication extends BaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.CLASSIFICATION_NAME)
    private String classificationName;

    @Column(name = FieldConstants.BRAND_NAME)
    private String brandName;

    @Column(name = FieldConstants.DOSAGE_FORM_NAME)
    private String dosageFormName;

    @Column(name = FieldConstants.CLASSIFICATION_ID)
    private Long classificationId;

    @Column(name = FieldConstants.DOSAGE_FORM_ID)
    private Long dosageFormId;

    @Column(name = FieldConstants.BRAND_ID)
    private Long brandId;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.CODES, columnDefinition = "jsonb")
    private List<Medication.Code> codes;

    @NotNull
    @JoinColumn(name = FieldConstants.CATEGORY)
    @ManyToOne(fetch = FetchType.LAZY)
    private Category category;

    @Data
    public static class Code implements Serializable {

        private String code;

        private String system;

        private String display;
    }
}
