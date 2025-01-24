package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Country;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import lombok.Data;
import org.modelmapper.ModelMapper;

import java.util.Objects;


/**
 * <p>
 * This class is an POJO for representing users under an Organization.
 * </p>
 * 
 * @author Gopinath R
 */
@Data
public class UserOrganizationDTO {
    private long id;

    private String username;

    private String firstName;

    private String lastName;

    private String gender;

    private Country country;

    private String countryCode;

    private String phoneNumber;

    private Timezone timezone;

    private Boolean isTermsAndConditionsAccepted;
    
    private Designation designation;

    public TimezoneDTO getTimezone() {
        ModelMapper modelMapper = new ModelMapper();
        return modelMapper.map(timezone, TimezoneDTO.class);
    }

    public CountryDTO getCountry() {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(country)) {
            return null;
        }
        return modelMapper.map(country, CountryDTO.class);
    }

    public CommonResponseDTO getDesignation() {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(designation)) {
            return new CommonResponseDTO();
        }
        return modelMapper.map(designation, CommonResponseDTO.class);
    }
}
