package com.mdtlabs.coreplatform.spiceservice.bplog.controller;

import jakarta.validation.Valid;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.commonservice.common.annotations.TenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.spiceservice.bplog.service.BpLogService;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.BpLogDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientBpLogsDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

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
     * Creates new Bp log information
     *
     * @param bpLogDto - entity
     * @return BpLog Entity
     */
    @PostMapping("/create")
    @TenantValidation
    public SuccessResponse<BpLogDTO> addBpLog(@Valid @RequestBody BpLogDTO bpLogDto) {
        Logger.logDebug("In BpLogController, creating bp log information");
        if (Objects.isNull(bpLogDto.getType())) {
            bpLogDto.setType(Constants.MEDICAL_REVIEW);
        }
        return new SuccessResponse<>(SuccessCode.BP_LOG_SAVE, bpLogService.addBpLog(bpLogDto), HttpStatus.CREATED);
    }

    /**
     * Gets bp log information by patient id
     *
     * @param patientBpLogsRequestDto - request dto
     * @return PatientBpLogsDto - entity
     */
    @PostMapping("/list")
    @TenantValidation
    public SuccessResponse<PatientBpLogsDTO> getBpLogsByPatientTrackId(
            @RequestBody RequestDTO patientBpLogsRequestDto) {
        Logger.logDebug("In BpLogController, fetching bp logs by patient track id information");
        return new SuccessResponse<>(SuccessCode.GET_BP_LOG_LIST,
                bpLogService.getPatientBpLogsWithSymptoms(patientBpLogsRequestDto), HttpStatus.OK);
    }
}
