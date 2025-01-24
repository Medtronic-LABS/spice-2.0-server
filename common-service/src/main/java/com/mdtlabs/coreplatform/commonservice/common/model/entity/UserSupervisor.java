package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

@Data
@Entity
@Table(name = TableConstants.TABLE_USER_SUPERVISOR)
public class UserSupervisor extends BaseEntity {
    
    @Column(name = FieldConstants.USER_ID)
    private Long userId;

    @Column(name = FieldConstants.SUPERVISOR_ID)
    private Long supervisorId;

    public UserSupervisor(Long userId, Long supervisorId) {
        this.userId = userId;
        this.supervisorId = supervisorId;
    }

    public UserSupervisor() {
        
    }

    

}
