package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;

import lombok.Data;

@Data
public class MetaFormDTO {

    private Long id;

    private String formName;

    private List<Map<String, Object>> components;

    public MetaFormDTO() {

    }

    public MetaFormDTO(Long id, String formName, List<Map<String, Object>> components) {
        this.id = id;
        this.formName = formName;
        this.components = components;
    }
}