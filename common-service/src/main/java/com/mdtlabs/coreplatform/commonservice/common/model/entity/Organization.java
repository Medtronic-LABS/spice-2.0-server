package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

@Data
@Entity
@Table(name = TableConstants.TABLE_ORGANIZATION)
public class Organization extends BaseEntity {

    private static final long serialVersionUID = 1L;

    public Organization() {
    }

    public Organization(String formName, String name, Long parentOrganizationId) {
		this.formName = formName;
		this.name = name;
		this.parentOrganizationId = parentOrganizationId;
	}

	public Organization(Long formDataId, String formName, String name, Long parentOrganizationId) {
		this.formDataId = formDataId;
		this.formName = formName;
		this.name = name;
		this.parentOrganizationId = parentOrganizationId;
	}

	public Organization(Long id, String name) {
		super(id);
		this.name = name;
	}

	@Column(name = FieldConstants.FORM_DATA_ID)
	private Long formDataId;

	@Column(name = FieldConstants.FORM_NAME)
	private String formName;

	@Column(name = FieldConstants.NAME)
	private String name;

	@Column(name = FieldConstants.SEQUENCE)
	private Long sequence;

	@Column(name = FieldConstants.PARENT_ORGANIZATION_ID)
	private Long parentOrganizationId;
    
}
