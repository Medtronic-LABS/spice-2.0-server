package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class PrescriptionPredictionDTO {

    List<GlucoseLogDTO> recentBGLogs;

    List<PrescriptionDTO> prescriptionResults;

}