package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import lombok.Data;

@Data
public class ContextsDTO {
    
    private UserContextDTO userDetail;

    private Object tenants;

    public ContextsDTO(UserContextDTO userDetail, Object tenants) {
        this.userDetail = userDetail;
        this.tenants = tenants;
    }

    
}
