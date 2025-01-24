package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;

import lombok.Data;

@Data
public class NotificationDTO {
	
    private Long id;
    
    private Long tenantId;
    
    private Long createdBy = getUserValue();

    private Long updatedBy = getUserValue();

    @CreationTimestamp
    private Date createdAt;

    @UpdateTimestamp
    private Date updatedAt;

    private boolean isActive;

    private boolean isDeleted;
	
	private String toEmail;

	private String ccEmails;

	private String subject;

	private String body;

	private String status;
	
	/**
     * This method is used to get user value
     * 
     * @return String - user value
     */
    @JsonIgnore
    public long getUserValue() {
        if (null != UserContextHolder.getUserDto()) {
            return UserContextHolder.getUserDto().getId();
        }
        return Constants.ZERO;
    }


}
