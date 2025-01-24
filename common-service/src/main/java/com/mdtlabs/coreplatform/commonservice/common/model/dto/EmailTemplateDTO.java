package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;

import lombok.Data;

@Data
public class EmailTemplateDTO {
	
	private long id;

	private String type;

	private String vmContent;

	private String body;

	private String title;

	private String appUrl;

	private List<EmailTemplateValueDTO> emailTemplateValues;


}
