package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This class is used to encapsulate the data transfer object for country-related operations
 * </p>
 *
 * @author Divya S created on July 19, 2024
 */
@Data
public class CountryRequestDTO {

    private Long id;

    private String name;

    private String phoneNumberCode;

    private String unitMeasurement;

    private List<UserRequestDTO> users;

    private Long tenantId;

    private List<String> appTypes;

}
