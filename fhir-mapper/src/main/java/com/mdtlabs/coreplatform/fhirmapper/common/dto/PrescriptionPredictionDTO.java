package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class PrescriptionPredictionDTO {

    List<GlucoseLogDTO> recentBGLogs;

    List<PrescriptionDTO> prescriptionResults;

    public PrescriptionPredictionDTO() {
    }

    public PrescriptionPredictionDTO(List<GlucoseLogDTO> recentBGLogs, List<PrescriptionDTO> prescriptionResults) {
        this.recentBGLogs = recentBGLogs;
        this.prescriptionResults = prescriptionResults;
    }
}

