package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

import java.io.InputStream;

/**
 * <p>
 * This is a DTO class for a file.
 * </p>
 */
@Data
public class FileDTO {

    private InputStream inputStream;

    private long contentLength;

    private String name;

    public FileDTO() {

    }

    public FileDTO(InputStream inputStream, long contentLength, String name) {
        this.inputStream = inputStream;
        this.contentLength = contentLength;
        this.name = name;
    }
}