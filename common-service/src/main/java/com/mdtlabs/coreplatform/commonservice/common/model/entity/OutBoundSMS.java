package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.io.Serializable;

@Data
@Entity
@Table(name = TableConstants.TABLE_OUTBOUND_SMS)
public class OutBoundSMS extends BaseEntity implements Serializable {

	private static final long serialVersionUID = 4174505913611242103L;

	@Id
	@Column(name = FieldConstants.ID)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@Column(name = FieldConstants.TENANT_ID)
	private Long tenantId;

	@Column(name = FieldConstants.NOTIFICATION_ID)
	private Long notificationId;

	@Column(name = FieldConstants.FORM_DATA_ID)
	private String formDataId;

	@Column(name = FieldConstants.IS_PROCESSED)
	private boolean isProcessed = false;

	@Column(name = FieldConstants.RETRY_ATTEMPTS)
	private int retryAttempts = 0;

	@Column(name = FieldConstants.USERNAME)
	private String userName;

	@Column(name = FieldConstants.PHONE_NUMBER)
	private String phoneNumber;

	@Column(name = FieldConstants.BODY)
	private String body;

}
