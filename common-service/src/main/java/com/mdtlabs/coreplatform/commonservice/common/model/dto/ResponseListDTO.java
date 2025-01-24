package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;

import lombok.Data;

/**
 * This class is an Data transfer Object for List of Objects.
 * 
 * @author Niraimathi S created on Feb 07, 2023
 *
 */
@Data
public class ResponseListDTO<T> {
    
    private List<T> data;
    
    private Long totalCount;
    
    public List<T> getData() {
        return data;
    }

    public ResponseListDTO(List<T> data, Long totalCount) {
        this.data = data;
        this.totalCount = totalCount;
    }

    public ResponseListDTO() {
        
    }

    public ResponseListDTO(List<T> data) {
        super();
        this.data = data;
    }
}

