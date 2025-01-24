package com.mdtlabs.coreplatform.commonservice.common.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * This is a REST controller that handles health check requests.
 * It provides an endpoint to check the health status of the application.
 * </p>
 *
 * <p>
 * The health check endpoint is typically used for monitoring and alerting purposes,
 * to ensure that the application is running and responding to HTTP requests.
 * </p>
 *
 * <p>
 * The controller is mapped to the "/health-check" URL path.
 * </p>
 *
 * @author Sharveshkumar created on 02 Apr 2024
 */
@RestController
@RequestMapping(value = "/health-check")
public class HealthCheckController {

	/**
	 * <p>
	 * This method handles GET requests to the "/health-check" URL path.
	 * </p>
	 *
	 * <p>
	 * When called, it returns a success response with a status code of 200 (OK),
	 * a success code indicating a successful health check, and a payload of true.
	 * </p>
	 *
	 * @return a SuccessResponse object with a payload of true, indicating that the application is healthy
	 */
    @GetMapping
	public ResponseEntity<String> healthCheck() {
		return new ResponseEntity<>("Health Check succeeded.", HttpStatus.OK);
	}
}   
