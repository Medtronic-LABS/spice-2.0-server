package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import lombok.Data;

/**
 * This is a Request DTO class clinical summary and signs.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class ClinicalSummaryAndSignsDTO {

    private Double weight;

    private String heightUnit;

    private Double height;

    private String weightUnit;

    private Double temperature;

    private String temperatureUnit;

    private Double waz;

    private Double whz;

    private String immunisationStatus;

    private List<Double> respirationRate;

    private Boolean vitAForMother;

    private Boolean albendazole;

    private Boolean breastFeeding;

    private Boolean exclusiveBreastFeeding;

    private String muacStatus;

    private Double muacInCentimeter;

}
