package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

@Entity
@Table(name = "suite_access")
@Data
public class SuiteAccess extends BaseEntity {
    
    @Column(name =  "name")
    private String name;

    @Column(name = "level")
    private String level;

    @Column(name = "is_all_roles")
    private boolean isAllRoles;

    @Column(name = "group_name")
    private String groupName;
    
}
