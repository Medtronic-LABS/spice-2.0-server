package com.mdtlabs.coreplatform.commonservice.apiinterface;

import feign.FeignException;
import feign.Headers;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.ContextsDTO;

/**
 * <p>
 * This an interface class in which you can implemented this class in any
 * class.
 * </p>
 *
 * @author Maria Antony Praveen created on Mar 04, 2024
 */
@FeignClient(name = "auth-service", url = "${auth-service}")
public interface AuthServiceApiInterface {

    @PostMapping("/authenticate")
    ContextsDTO validateToken(@RequestHeader("Authorization") String token, @RequestHeader("client") String client,
            @RequestHeader(value = "auth-cookie", required = false) String cookie) throws FeignException;

    @PostMapping("/session")
    @Headers("Content-Type: application/x-www-form-urlencoded")
    ResponseEntity<String> login(@RequestBody MultiValueMap<String, String> formData, @RequestHeader String client);
}
