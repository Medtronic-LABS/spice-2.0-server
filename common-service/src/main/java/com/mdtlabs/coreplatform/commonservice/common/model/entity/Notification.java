package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import lombok.Data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.io.Serializable;

/**
 * <p>
 * Notification class to get the notification entity.
 * </p>
 * 
 * @author VigneshKumar created on Jun 30, 2022
 */
@Entity
@Data
@Table(name = TableConstants.TABLE_NOTIFICATION)
public class Notification extends BaseEntity implements Serializable {

	private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.TO_EMAIL)
	private String toEmail;

	@Column(name = FieldConstants.CC_EMAILS)
	private String ccEmails;

	@Column(name = FieldConstants.SUBJECT)
	private String subject;

	@Column(name = FieldConstants.BODY)
	private String body;

	@Column(name = FieldConstants.STATUS)
	private String status;

	public Notification() {

	}

	public Notification(String subject, String body, String toEmail) {
		this.subject = subject;
		this.body = body;
		this.toEmail = toEmail;
	}

}
