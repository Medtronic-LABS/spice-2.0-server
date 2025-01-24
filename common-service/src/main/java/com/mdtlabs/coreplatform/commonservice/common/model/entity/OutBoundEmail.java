package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import lombok.Data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.io.Serializable;

@Data
@Entity
@Table(name = TableConstants.TABLE_OUTBOUND_EMAIL)
public class OutBoundEmail extends BaseEntity implements Serializable {

	private static final long serialVersionUID = 4174505913611242103L;

	@Id
	@Column(name = FieldConstants.ID)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@Column(name = FieldConstants.TENANT_ID)
	private Long tenantId;

	@Column(name = FieldConstants.FORM_DATA_ID)
	private String formDataId;

	@Column(name = FieldConstants.IS_PROCESSED)
	private boolean isProcessed = false;

	@Column(name = FieldConstants.RETRY_ATTEMPTS)
	private int retryAttempts = 0;

	@Column(name = FieldConstants.FORM_NAME)
	private String formName;

	@Column(name = FieldConstants.TO)
	private String to;

	@Column(name = FieldConstants.TYPE)
	private String type;

	@Column(name = FieldConstants.SUBJECT)
	private String subject;

	@Column(name = FieldConstants.BODY)
	private String body;

	@Column(name = FieldConstants.CC)
	private String cc;

	@Column(name = FieldConstants.BCC)
	private String bcc;
}
