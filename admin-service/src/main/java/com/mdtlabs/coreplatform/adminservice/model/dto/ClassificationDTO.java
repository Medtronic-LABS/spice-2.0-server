package com.mdtlabs.coreplatform.adminservice.model.dto;

import lombok.Data;

import java.util.List;

/**
 * <p>
 *     This class is an DTO represent Classification fields.
 * </p>
 *
 * @author Premkalyan
 * @since Dec 16, 2024
 */
@Data
public class ClassificationDTO {

    private Long id;

    private String name;

    private List<BrandDTO> brands;

}
