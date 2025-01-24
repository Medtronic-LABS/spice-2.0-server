package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This is a DTO class for Medication entity.
 *
 * @author Gokul A Created on 30 Sep 2024
 */
@Data
public class MedicationDTO {

    private Long id;

    private String name;

    private Long classificationId;

    private Long dosageFormId;

    private Long brandId;

    private String classificationName;

    private String brandName;

    private String dosageFormName;

    private Long countryId;

    private Long tenantId;

    private Code codeDetails;

    private CategoryDTO category;

}

