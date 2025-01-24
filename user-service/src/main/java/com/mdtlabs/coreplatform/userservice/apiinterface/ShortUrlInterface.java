package com.mdtlabs.coreplatform.userservice.apiinterface;

import com.mdtlabs.coreplatform.userservice.model.RequestDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import java.util.Map;

@FeignClient(name = "short-url-service", url = "${app.shorten-app-url}")
public interface ShortUrlInterface {

    @PostMapping("/")
    ResponseEntity<Map<String, Object>> shortenURL(@RequestBody RequestDTO requestDTO);
}
