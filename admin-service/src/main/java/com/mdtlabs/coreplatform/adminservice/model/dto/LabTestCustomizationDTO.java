package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

/**
 * <p>
 * DTO class for LabTestCustomization.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jun 20, 2024
 */
@Data
public class LabTestCustomizationDTO {

    private Long id;

    private String uniqueName;

    private String testName;

    private String formInput;

    private Long countryId;

    private String tenantId;

    private Code codeDetails;

}
