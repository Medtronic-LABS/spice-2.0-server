package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.CustomDateSerializer;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.audit.RestrictAudit;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;

import lombok.Data;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * auditing the records on every update
 * </p>
 * 
 * @author Prabu created on July 11, 2022
 *
 */
@Data
@Entity
@RestrictAudit
@Table(name = TableConstants.TABLE_AUDIT)
public class Audit implements Serializable {

	private static final long serialVersionUID = 4174505913611242103L;

	@Id
	@Column(name = FieldConstants.ID)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private long id;
	
	@Column(name = FieldConstants.ENTITY)
	private String entity;

	@Column(name = FieldConstants.ACTION)
	private String action;

	@Column(name = FieldConstants.ENTITY_ID)
	private long entityId;

	@Column(name = FieldConstants.COLUMN_NAME)
	private String columnName;

	@Column(name = FieldConstants.OLD_VALUE)
	private String oldValue;

	@Column(name = FieldConstants.NEW_VALUE)
	private String newValue;

	@Column(name = FieldConstants.CREATED_BY, updatable = false, nullable = false)
	private long createdBy = getUserValue();

	@Column(name = FieldConstants.UPDATED_BY, nullable = true)
	private long updatedBy = getUserValue();

	@Column(name = FieldConstants.CREATED_AT, columnDefinition = "TIMESTAMP", nullable = false, updatable = false)
	@CreationTimestamp
	@Temporal(TemporalType.TIMESTAMP)
	@JsonSerialize(using = CustomDateSerializer.class)
	private Date createdAt;

	@Column(name = FieldConstants.UPDATED_AT, columnDefinition = "TIMESTAMP")
	@UpdateTimestamp
	@Temporal(TemporalType.TIMESTAMP)
	@JsonSerialize(using = CustomDateSerializer.class)
	private Date updatedAt;

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