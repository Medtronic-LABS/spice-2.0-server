package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

/**
 *
 * This is an DTO cotaining necessary fields to SMS entity.
 * 
 * @author Niraimathi S created on Feb 08, 2023
 *
 */
@Data
public class SmsDTO {
	private String body;
	
	private String subject;
	
	private String toPhoneNo;
	
	private String formName;
	
	private String userName;
	
	private Long tenantId;
	
	private String formDataId;
	
	private Long notificationId;

	public SmsDTO(String body, String toPhoneNo, String userName, long tenantId, String formDataId, Long notificationId) {
		this.body = body;
		this.toPhoneNo = toPhoneNo;
		this.userName = userName;
		this.tenantId = tenantId;
		this.formDataId = formDataId;
		this.notificationId = notificationId;
	}

	public SmsDTO() {
	}

	public SmsDTO(String body, String toPhoneNo, Long tenantId, String formDataId, String userName) {
		super();
		this.userName = userName;
		this.body = body;
		this.toPhoneNo = toPhoneNo;
		this.tenantId = tenantId;
		this.formDataId = formDataId;
	}

}
