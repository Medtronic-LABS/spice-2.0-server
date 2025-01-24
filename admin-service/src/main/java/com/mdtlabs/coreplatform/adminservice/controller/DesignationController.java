package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.DesignationService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DesignationListResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Designation;

/**
 * <p>
 * This controller class maintains CRUD operation for Designation data.
 * </p>
 *
 * @author Divya S created on Oct 18, 2024
 */
@RestController
@RequestMapping(value = "/designation")
public class DesignationController {

    private final DesignationService designationService;
    private final ModelMapper modelMapper;

    @Autowired
    public DesignationController(DesignationService designationService, ModelMapper modelMapper) {
        this.designationService = designationService;
        this.modelMapper = modelMapper;
    }

    /**
     * <p>
     * To Fetch all Designation for given countryId.
     * </p>
     *
     * @param countryId ID of the country to which the designation list to be retrieved is given
     * @return {@link SuccessResponse} List of designation entity is retrieved
     */
    @GetMapping("/list/{countryId}")
    public SuccessResponse<List<DesignationListResponseDTO>> getAllDesignation(@PathVariable(Constants.COUNTRY_ID) long countryId) {
        List<Designation> designationList = designationService.getAllDesignation(countryId);
        List<DesignationListResponseDTO> designationDTOList = designationList.stream()
                .map(designation -> modelMapper.map(designation, DesignationListResponseDTO.class))
                .toList();
        if (!designationList.isEmpty()) {
            return new SuccessResponse<>(SuccessCode.GET_DESIGNATIONS, designationDTOList, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.DESIGNATION_NOT_FOUND, designationDTOList, HttpStatus.OK);
    }
}
