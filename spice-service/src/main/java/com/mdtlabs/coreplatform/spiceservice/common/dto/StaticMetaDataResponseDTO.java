package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;

/**
 * <p>
 * This is a DTO class for static meta data of above and under 5Y response.
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan Created on Mar 13, 2024.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class StaticMetaDataResponseDTO {

    private List<MetaDataDTO> medicalSupplies;

    private List<MetaDataDTO> patientStatus;

    private List<MetaDataDTO> systemicExaminations;

    private List<MetaDataDTO> presentingComplaints;

    private List<MetaDataDTO> diseaseCategories;

    private List<MetaDataDTO> counselledOn;

    private List<MetaDataDTO> examinations;

    private List<MetaDataDTO> obstetricExaminations;

    private List<MetaDataDTO> pregnancyHistories;

    private List<MetaDataDTO> cost;

    private List<MetaDataDTO> bloodGroup;

    private List<MetaDataDTO> deliveryAt;

    private List<MetaDataDTO> deliveryBy;

    private List<MetaDataDTO> deliveryType;

    private List<MetaDataDTO> deliveryStatus;

    private List<MetaDataDTO> neonateOutcome;

    private List<MetaDataDTO> pncNeonateOutcome;

    private List<MetaDataDTO> riskFactors;

    private List<MetaDataDTO> conditionOfMother;

    private List<MetaDataDTO> motherDeliveryStatus;

    private List<MetaDataDTO> dosageFrequencies;

    private List<MetaDataDTO> immunisationStatus;

    private List<MetaDataDTO> muac;

    private List<MetaDataDTO> medicalCompliances;

    private List<MetaDataDTO> diagnosis;

    private List<MetaDataDTO> dosageForm;

    private List<MetaDataDTO> reasons;

    private List<MetaDataDTO> units;

    private List<MetaDataDTO> nutritionLifestyle;

    private List<MetaDataDTO> symptoms;

    private Map<String, Object> cvdRiskAlgorithms;

    private List<MetaDataDTO> stateOfPerineum;

}
