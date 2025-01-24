package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.Map;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.Data;
import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

/**
 * <p>
 * This class represents the CustomizedModule Entity.
 * </p>
 *
 * @since Oct 24, 2022
 * @author Divya S
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_CUSTOMIZED_MODULES)
public class CustomizedModule extends TenantBaseEntity {

    private static final long serialVersionUID = 1L;

	@NotNull
    @Column(name = FieldConstants.MODULE_VALUE, columnDefinition = "jsonb")
    @Type(value = JsonBinaryType.class)
    private Map<String, Object> moduleValue;

    @NotBlank
    @Column(name = FieldConstants.SCREEN_TYPE)
    private String screenType;

    @NotNull
    @Column(name = FieldConstants.PATIENT_ID)
    private String patientId;

    @NotNull
    @Column(name = FieldConstants.MEMBER_ID)
    private String memberId;

    @Column(name = FieldConstants.CLINICAL_WORKFLOW_ID)
    private Long clinicalWorkflowId;

}
