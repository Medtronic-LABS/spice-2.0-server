package com.mdtlabs.coreplatform.commonservice.common.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;

import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HealthCheckControllerTest {

    @InjectMocks
    HealthCheckController healthCheckController;

    @Test
    void healthCheck() {
        //then
        ResponseEntity<String> response = healthCheckController.healthCheck();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }
}