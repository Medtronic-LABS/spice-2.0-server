package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;
import java.util.Objects;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramRequestDTO;
import com.mdtlabs.coreplatform.adminservice.service.ProgramService;
import com.mdtlabs.coreplatform.commonservice.common.annotations.UserTenantValidation;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;

import jakarta.validation.Valid;

/**
 * <p>
 * This class is a controller class to perform operation on Program entity.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2022
 * 
 */
@RestController
@RequestMapping("/program")
@Validated
public class ProgramController {

	private final ProgramService programService;

	@Autowired
	public ProgramController(ProgramService programService) {
		this.programService = programService;
	}

	/**
	 * <p>
	 * This method is used to add a new program.
	 * </p>
	 *
	 * @param program request data containing program details
	 * @return program Entity.
	 */
	@UserTenantValidation
	@PostMapping("/create")
	public SuccessResponse<Program> createProgram(@Valid @RequestBody ProgramRequestDTO program) {
		programService.createProgram(program);
		return new SuccessResponse<>(SuccessCode.PROGRAM_SAVE, HttpStatus.CREATED);
	}

	/**
	 * <p>
	 * This method is used to retrieve single program details using programId.
	 * </p>
	 *
	 * @param requestDto requestDto to get program entity.
	 * @return Program Entity
	 */
	@UserTenantValidation
	@PostMapping("/details")
	public SuccessResponse<Program> getProgramById(@RequestBody CommonRequestDTO requestDto) {
		return new SuccessResponse<>(SuccessCode.GET_PROGRAM, 
			programService.getProgramDetails(requestDto), HttpStatus.OK);
	}

	/**
	 * <p>
	 * This method is used to retrieve single program details using programId.
	 * </p>
	 *
	 * @param request request data
	 * @return Program Entity
	 */
	@UserTenantValidation
	@PostMapping("/list")
	public SuccessResponse<ProgramListDTO> getPrograms(@RequestBody SearchRequestDTO request) {
        ResponseListDTO<ProgramListDTO> response = programService.getAllPrograms(request);
        if (Objects.isNull(response.getTotalCount()) || 0L == response.getTotalCount()) {
            return new SuccessResponse<>(SuccessCode.GET_PROGRAM, response.getData(), null, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.GET_PROGRAM, response.getData(), response.getTotalCount(),
            HttpStatus.OK);
  
	}

	/**
	 * <p>
	 * Used to soft delete a program.
	 * </p>
	 *
	 * @param requestDto - CommonRequestDTO
	 * @return Boolean - true or false
	 */
	@UserTenantValidation
	@DeleteMapping("/remove")
	public SuccessResponse<Boolean> removeProgram(@RequestBody CommonRequestDTO requestDto) {
		programService.removeProgram(requestDto);
		return new SuccessResponse<>(SuccessCode.PROGRAM_DELETE, HttpStatus.OK);
	}

	/**
	 * <p>
	 * Used to update a program detail like name, etc.,
	 * </p>
	 *
	 * @param program - entity
	 * @return program Entity
	 */
	@UserTenantValidation
	@PatchMapping("/update")
	public SuccessResponse<Program> updateProgram(@RequestBody ProgramRequestDTO program) {
		programService.updateProgram(program);
		return new SuccessResponse<>(SuccessCode.PROGRAM_STATUS_UPDATE, HttpStatus.OK);
	}
	
	/**
    * <p>
    * Gets list of programs using list of site Ids.
    * </p>
    *
    * @param siteIds List of siteIds
    * @return List of Program Entities
    */
	@PostMapping("/get-by-healthfacility-ids")
	public List<Program> getProgramsByHealthFacilityIds(@RequestBody List<Long> siteIds) {
		return programService.getProgramsByHealthFacilityIds(siteIds);
	}

}
