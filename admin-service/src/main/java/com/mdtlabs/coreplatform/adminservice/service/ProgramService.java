package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramDetailsDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;


/**
 * <p>
 * This is an interface to perform any actions in Program entities.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2022
 */
public interface ProgramService {

    /**
     * <p>
     * Creates a new program.
     * </p>
     *
     * @param program entity
     * @return Program entity
     */
    public Program createProgram(ProgramRequestDTO program);

    /**
     * <p>
     * Gets a program details based on id.
     * </p>
     *
     * @param id - program id
     * @return Program entity
     */
    public Program getProgramById(Long id, Long tenantId, Boolean isDeleted);

    /**
     * <p>
     * Updates the program.
     * </p>
     *
     * @param program entity
     * @return Program entity
     */
    public Program updateProgram(ProgramRequestDTO program);

    /**
     * <p>
     * Soft delete the program.
     * </p>
     *
     * @param requestDto program requestDTO
     * @return boolean true or false
     */
    public boolean removeProgram(CommonRequestDTO requestDto);

    /**
     * <p>
     * Gets all the program based on request object.
     * </p>
     *
     * @param requestObject - request dto
     * @return Map(String, Object) - program list
     */
    public ResponseListDTO<ProgramListDTO> getAllPrograms(SearchRequestDTO requestObject);

    /**
     * <p>
     * Gets list of programs using list of site Ids.
     * </p>
     *
     * @param siteIds List of siteIds
     * @return List of Program Entities
     */
    public List<Program> getProgramsByHealthFacilityIds(List<Long> siteIds);
	
    /**
     * <p>
     * TO get a single program details using, Id and its tenantId.
     * </p>
     *
     * @param requestDto - request object
     * @return ProgramDetailsDTO - Program Entity
     */
    public ProgramDetailsDTO getProgramDetails(CommonRequestDTO requestDto);

}
