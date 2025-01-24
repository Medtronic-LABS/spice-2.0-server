package com.mdtlabs.coreplatform.spiceservice.glucoselog.controller;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.GlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientGlucoseLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.glucoselog.service.GlucoseLogService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * This class is a controller class to perform operation on GlucoseLog entity.
 *
 * @since Aug 28, 2024
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
     * This method is used to add a new Glucose log.
     *
     * @param glucoseLogDto  - entity
     * @return GlucoseLogDTO - entity.
     */
    @PostMapping("/create")
    @TenantValidation
    public SuccessResponse<GlucoseLogDTO> addGlucoseLog (@RequestBody GlucoseLogDTO glucoseLogDto) {
        if (Objects.isNull(glucoseLogDto.getType())) {
            glucoseLogDto.setType(Constants.MEDICAL_REVIEW);
        }
        return new SuccessResponse<>(SuccessCode.GLUCOSE_LOG_SAVE, glucoseLogService.addGlucoseLog(glucoseLogDto),
                HttpStatus.CREATED);
    }

    /**
     * This method is used to fetch bp logs using related person id.
     *
     * @param patientGlucoseLogsRequestDto PatientGlucoseLogsRequest object to fetch the list of patient's glucose logs.
     * @return PatientGlucoseLogDTO success entity.
     */
    @PostMapping("/list")
    @TenantValidation
    public SuccessResponse<PatientGlucoseLogDTO> getBpLogsByPatientTrackId(
            @RequestBody RequestDTO patientGlucoseLogsRequestDto) {
        return new SuccessResponse<>(SuccessCode.GET_GLUCOSE_LOG_LIST,
                glucoseLogService.getPatientGlucoseLogsWithSymptoms(patientGlucoseLogsRequestDto), HttpStatus.OK);
    }
}
