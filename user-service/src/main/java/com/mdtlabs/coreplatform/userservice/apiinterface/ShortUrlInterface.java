package com.mdtlabs.coreplatform.userservice.apiinterface;

import java.util.Map;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.mdtlabs.coreplatform.userservice.model.RequestDTO;

@FeignClient(name = "short-url-service", url = "${app.shorten-app-url}")
public interface ShortUrlInterface {

    @PostMapping("/")
    ResponseEntity<Map<String, Object>> shortenURL(@RequestBody RequestDTO requestDTO);
}
