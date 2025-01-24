package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import java.util.List;

@Data
@Table(name = TableConstants.TABLE_SMS_TEMPLATE)
@Entity
public class SMSTemplate extends BaseEntity {

	@Column(name = FieldConstants.BODY)
	private String body;

	@Column(name = FieldConstants.TYPE)
	private String type;

	@OneToMany
	@JoinColumn(name = FieldConstants.SMS_TEMPLATE_ID)
	private List<SMSTemplateValues> smsTemplateValues;

}
