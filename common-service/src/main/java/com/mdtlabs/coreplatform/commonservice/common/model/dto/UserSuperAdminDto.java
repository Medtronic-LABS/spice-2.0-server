package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.util.Date;

import org.modelmapper.ModelMapper;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;

import lombok.Data;

@Data
public class UserSuperAdminDto {

    private Long id;

    private String firstName;

    private String lastName;

    private String gender;

    private long tenantId;

    private String email;

    private String phoneNumber;

    private Date createdAt;

    private Date updatedAt;

    private Timezone timezone;

    private String countryCode;

    private String username;

    public TimezoneDTO getTimezone() {
        ModelMapper modelMapper = new ModelMapper();
        return modelMapper.map(timezone, TimezoneDTO.class);
    }

    public String getEmail() {
        this.email = this.username;
        return this.email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

}
