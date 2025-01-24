package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This class is an entity class for api role permission table.
 * </p>
 * 
 * @author Prabu created on Oct 10 2022
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_API_ROLE_PERMISSION)
public class ApiRolePermission extends BaseEntity {
	
	@Column(name = FieldConstants.METHOD)
	private String method;
	
	@Column(name = FieldConstants.API)
	private String api;
	
	@Column(name = FieldConstants.ROLES)
	private List<String> roles;

	@Column(name = FieldConstants.SERVICE_NAME)
	private String serviceName;

	@Column(name = FieldConstants.TYPE)
	private String type;

}
