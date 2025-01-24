package com.mdtlabs.coreplatform.fhirmapper.bplog.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.fhirmapper.bplog.service.BpLogService;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;

/**
 * <p>
 * This class is a controller class to perform operation on BpLog entity.
 * </p>
 *
 */
@RestController
@RequestMapping(value = "/bplog")
@Validated
public class BpLogController {

    private final BpLogService bpLogService;

    @Autowired
    public BpLogController(BpLogService bpLogService) {
        this.bpLogService = bpLogService;
    }

    /**
     * Gets bp log information by patient id
     *
     * @param patientBpLogsRequestDto - request dto
     * @return PatientBpLogsDto
     */
    @PostMapping("/list")
    public PatientBpLogsDTO getBpLogsByPatientTrackId(@RequestBody RequestDTO patientBpLogsRequestDto) {
        Logger.logDebug("In BpLogController, fetching bp logs by patient track id information");
        return bpLogService.getPatientBpLogsWithSymptoms(patientBpLogsRequestDto);
    }
}
