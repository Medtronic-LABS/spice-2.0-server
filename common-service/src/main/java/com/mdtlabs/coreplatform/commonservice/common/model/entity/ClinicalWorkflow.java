package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.util.List;
import java.util.Map;

import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

@Data
@Entity
@Table(name = TableConstants.TABLE_CLINICAL_WORKFLOW)
public class ClinicalWorkflow extends BaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.MODULE_TYPE)
    private String moduleType;

    @Column(name = FieldConstants.GROUP_NAME)
    private String groupName;

    @Column(name = FieldConstants.WORKFLOW_NAME)
    private String workflowName;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.DISPLAY_ORDER)
    private Long displayOrder;

    @Type(value = JsonBinaryType.class)
	@Column(name = FieldConstants.CONDITIONS, columnDefinition = "jsonb")
	private List<Map<String, Object>> conditions;

    @Column(name = FieldConstants.VIEW_SCREENS, columnDefinition = "varchar[]")
    private List<String> viewScreens;

    @Column(name = FieldConstants.IS_DEFAULT)
    private boolean isDefault;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;

}
