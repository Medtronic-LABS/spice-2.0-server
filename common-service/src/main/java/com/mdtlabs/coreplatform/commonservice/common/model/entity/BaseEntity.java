package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.io.Serializable;
import java.util.Date;
import java.util.Objects;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;

import jakarta.persistence.Column;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.Data;

@Data
@MappedSuperclass
public class BaseEntity implements Serializable {

    private static final long serialVersionUID = 4174505913611242103L;

    @Id
    @Column(name = FieldConstants.ID)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = FieldConstants.CREATED_BY, updatable = false, nullable = true)
    private Long createdBy = getUserValue();

    @Column(name = FieldConstants.UPDATED_BY, nullable = true)
    private Long updatedBy;

    @Column(name = FieldConstants.CREATED_AT, columnDefinition = "TIMESTAMP", nullable = false, updatable = false)
    @CreationTimestamp
    @Temporal(TemporalType.TIMESTAMP)
    private Date createdAt;

    @Column(name = FieldConstants.UPDATED_AT, columnDefinition = "TIMESTAMP")
    @UpdateTimestamp
    @Temporal(TemporalType.TIMESTAMP)
    private Date updatedAt;

    @Column(name = FieldConstants.IS_ACTIVE)
    private boolean isActive = true;

    @Column(name = FieldConstants.IS_DELETED)
    private boolean isDeleted = false;

    /**
     * Default Constructor
     */
    public BaseEntity() {

    }

    /**
     * Constructor to set Id
     */
    public BaseEntity(Long id) {
        this.id = id;
    }

    /**
     * This method is used to get user value
     *
     * @return String - user value
     */
    @JsonIgnore
    public long getUserValue() {
        Long resolvedCreatedBy;
        if (!StringUtils.isEmpty(CommonUtil.jobUserName) && Objects.nonNull(createdBy)) {
            resolvedCreatedBy = createdBy;
        } else if (Objects.nonNull(UserContextHolder.getUserDto())) {
            resolvedCreatedBy = UserContextHolder.getUserDto().getId();
        } else {
            resolvedCreatedBy = Constants.LONG_ZERO;
        }
        return resolvedCreatedBy;
    }

    @PrePersist
    @PreUpdate
    public void prePersistAndUpdate() {
        if (Objects.nonNull(UserContextHolder.getUserDto())) {
            this.updatedBy = UserContextHolder.getUserDto().getId();
        }
    }

}
