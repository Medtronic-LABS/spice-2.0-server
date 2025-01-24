package com.mdtlabs.coreplatform.dwarfurl.common.dto;

import lombok.Data;

/**
 * Data Transfer Object (DTO) for handling URL shortening responses.
 */
@Data
public class ResponseDTO {

    private String shortUrl;

    public ResponseDTO(String shortUrl) {
        this.shortUrl = shortUrl;
    }
}
