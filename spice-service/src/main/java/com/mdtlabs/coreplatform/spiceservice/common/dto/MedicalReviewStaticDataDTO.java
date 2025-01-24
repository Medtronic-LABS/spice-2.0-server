package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;

import lombok.Data;

/**
 * This is response DTO class for medical review static metas.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MedicalReviewStaticDataDTO {

	private List<MetaDataDTO> comorbidity;

	private List<MetaDataDTO> complications;

	private List<MetaDataDTO> currentMedication;

	private List<MetaDataDTO> physicalExamination;

	private List<MetaDataDTO> lifestyle;

	private List<MetaDataDTO> complaints;
	
	private Map<String, Object> treatmentPlanFormData;

    private List<MetaDataDTO> frequencies;

    private List<MetaDataDTO> frequencyTypes;

	private List<MetaDataDTO> nutritionLifestyles;
	
}
