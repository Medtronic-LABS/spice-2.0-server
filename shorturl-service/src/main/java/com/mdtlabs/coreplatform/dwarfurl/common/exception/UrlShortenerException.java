package com.mdtlabs.coreplatform.dwarfurl.common.exception;

import lombok.Data;
import org.springframework.http.HttpStatus;

@Data
public class UrlShortenerException  extends RuntimeException {

    public final String errorMessage;
    public final HttpStatus status;

    public UrlShortenerException(String errorMessage, HttpStatus status) {
        this.errorMessage = errorMessage;
        this.status = status;
    }
}
