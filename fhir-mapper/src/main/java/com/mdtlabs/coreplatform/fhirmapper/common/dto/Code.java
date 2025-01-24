package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

@Data
public class Code {

    private String code;

    private String url;

    public Code() {

    }

    public Code(String code, String url) {
        this.code = code;
        this.url = url;
    }
}
