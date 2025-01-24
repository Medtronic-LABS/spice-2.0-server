package com.mdtlabs.coreplatform.spiceservice.common.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import java.util.List;

/**
 * Represents metadata for a form used for interaction.
 * This entity stores information about forms and their associated metadata,
 * which are used in interactions within the application.
 * It facilitates the dynamic handling of form-related data.
 */
@Data
@Entity
@Table(name = TableConstants.FORM_META)
public class FormMeta extends BaseEntity {

    @Column(name = FieldConstants.FORM_INPUT)
    private String formInput;

    @Column(name = FieldConstants.FORM_TYPE)
    private String formType;

    @ManyToOne
    @JoinColumn(name = FieldConstants.CLINICAL_WORKFLOW_ID)
    private ClinicalWorkflow clinicalWorkflow;
    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;
}