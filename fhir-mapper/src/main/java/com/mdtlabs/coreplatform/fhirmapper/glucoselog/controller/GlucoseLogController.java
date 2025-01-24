package com.mdtlabs.coreplatform.fhirmapper.glucoselog.controller;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.glucoselog.service.GlucoseLogService;

/**
 * This class is a controller class to perform operation on GlucoseLog entity.
 *
 * @since Sep 05, 2024
 * @author Tamilmani
 */
@RestController
@RequestMapping(value = "/glucoselog")
@Validated
public class GlucoseLogController {

    private final GlucoseLogService glucoseLogService;

    @Autowired
    public GlucoseLogController(GlucoseLogService glucoseLogService) {
        this.glucoseLogService = glucoseLogService;
    }

    /**
     * This method is used to fetch bp logs using related person id.
     *
     * @param patientGlucoseLogsRequestDto {@link RequestDTO} entity
     * @return PatientGlucoseLogDTO entity
     */
    @PostMapping("/list")
    public PatientGlucoseLogDTO getBpLogsByPatientTrackId(@RequestBody RequestDTO patientGlucoseLogsRequestDto) {
        return glucoseLogService.getPatientGlucoseLogsWithSymptoms(patientGlucoseLogsRequestDto);
    }
}
