package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.Data;
import org.hibernate.annotations.Type;

/**
 * This class is for user preferences entity
 *
 * @author Nandhakumar Karthikeyan created on July 09, 2024
 */
@Entity
@Data
@Table(name = TableConstants.TABLE_USER_PREFERENCES)
public class UserPreferences extends BaseEntity {

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.USER_ID)
    private Long userId;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.PREFERENCE, columnDefinition = "jsonb")
    private Object preference;

}
