package com.mdtlabs.coreplatform.adminservice.service.impl;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramDetailsDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramListDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ProgramRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.ProgramRepository;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.adminservice.service.ProgramService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Program;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import org.modelmapper.TypeToken;


/**
 * This class implements the Program interface and contains actual business
 * logic to perform operations on Program entity.
 *
 * @author Karthick M created on Jun 30, 2022
 */
@Service
public class ProgramServiceImpl implements ProgramService {

	private final ProgramRepository programRepository;
    private final HealthFacilityService healthFacilityService;
	private ModelMapper mapper = new ModelMapper();;

    @Autowired
    public ProgramServiceImpl(ProgramRepository programRepository, HealthFacilityService healthFacilityService) {
        this.programRepository = programRepository;
        this.healthFacilityService = healthFacilityService;
    }

	/**
	* {@inheritDoc}
	*/
	public Program createProgram(ProgramRequestDTO programDto) {
        Program existingProgram = programRepository.findByNameAndTenantIdAndIsDeleted(programDto.getName(),
            programDto.getTenantId(), false);
        if (!Objects.isNull(existingProgram)) {
            throw new DataConflictException(13002);
        }
        Program program = new Program(programDto.getName(), programDto.getTenantId(), programDto.getCountry());
        if (!Objects.isNull(programDto.getHealthFacilities()) && !programDto.getHealthFacilities().isEmpty()) {
            Set<HealthFacility> healthFacilities = healthFacilityService.getHealthFacilitiesByIds(programDto.getHealthFacilities());
            if (!Objects.isNull(healthFacilities)) {
                program.setHealthFacilities(healthFacilities);
            }
        }
        return programRepository.save(program);
	}

	/**
	 * {@inheritDoc}
	 */
	public Program getProgramById(Long id, Long tenantId, Boolean isDeleted) {
		Program program = programRepository.findByIdAndIsDeletedAndTenantId(id, Boolean.FALSE, tenantId);
		if (Objects.isNull(program)) {
			throw new DataNotFoundException(13001);
		}
		return program;
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean removeProgram(CommonRequestDTO requestDto) {
		Program program = getProgramById(requestDto.getId(), requestDto.getTenantId(), Boolean.FALSE);
		program.setDeleted(true);
		return !Objects.isNull(programRepository.save(program));
	}

	/**
     * {@inheritDoc}
     */
	public ProgramDetailsDTO getProgramDetails(CommonRequestDTO requestDto) {
        if (Objects.isNull(requestDto.getId()) || Objects.isNull(requestDto.getTenantId())) {
            throw new DataNotAcceptableException(13005);
        }
        Program program = getProgramById(requestDto.getId(), requestDto.getTenantId(), Boolean.FALSE);      
		program.setHealthFacilities(
		program.getHealthFacilities()
			.stream()
			.filter(BaseEntity::isActive)
			.collect(Collectors.toSet())
        );
        return mapper.map(program, ProgramDetailsDTO.class);
    }
	
	/**
	 * {@inheritDoc}
	 */
	public Program updateProgram(ProgramRequestDTO updatedProgram) {
        if (Objects.isNull(updatedProgram)) {
            throw new BadRequestException(13009);
        }
        if (!Objects.isNull(updatedProgram.getName())) { // check name from request data
            throw new DataNotAcceptableException(13004);
        }
        Program existingProgram = programRepository.findByIdAndIsDeleted(updatedProgram.getId(), false);
        if (Objects.isNull(existingProgram)) {
            throw new DataNotFoundException(13001);
        }
        if (!Objects.isNull(updatedProgram.getHealthFacilities()) && !updatedProgram.getHealthFacilities().isEmpty()) {
            Set<HealthFacility> healthFacilities = healthFacilityService.getHealthFacilitiesByIds(updatedProgram.getHealthFacilities());
            if (!Objects.isNull(healthFacilities)) {
                existingProgram.setHealthFacilities(healthFacilities);
            }
        }
        if (!Objects.isNull(updatedProgram.getDeletedHealthFacilities())) {
            Set<HealthFacility> healthFacilities = healthFacilityService.getHealthFacilitiesByIds(updatedProgram.getDeletedHealthFacilities());
            if (!Objects.isNull(healthFacilities)) {
                existingProgram.setDeletedHealthFacilities(healthFacilities);
            }
        }
        existingProgram.setActive(updatedProgram.isActive());
        return programRepository.save(existingProgram);
    }

	/**
	 * {@inheritDoc}
	 */
    public ResponseListDTO<ProgramListDTO> getAllPrograms(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getTenantId())) {
            throw new DataNotAcceptableException(13008);
        }
        long totalCount = 0;
        String searchTerm = requestDTO.getSearchTerm();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            return new ResponseListDTO<>(new ArrayList<>(), Constants.LONG_ZERO);
        }
        Pageable pageable = Pagination.setPagination(requestDTO.getSkip(), requestDTO.getLimit());
        Page<Program> programs = programRepository.getAllProgram(searchTerm, requestDTO.getCountryId(),
            requestDTO.getTenantId(), pageable);
        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        List<ProgramListDTO> programDtos = !Objects.isNull(programs)
            ? mapper.map(programs.stream().toList(), new TypeToken<List<ProgramListDTO>>() {
            }.getType())
            : null;
        long totalElements = Objects.nonNull(programs)? programs.getTotalElements() : Constants.ZERO;   
        totalCount = requestDTO.getSearchTerm().isBlank()
            ? programRepository.countByCountryIdAndTenantId(requestDTO.getCountryId(), requestDTO.getTenantId())
            : totalElements;

        return new ResponseListDTO<>(programDtos, totalCount);
    }
	
	/**
	 * {@inheritDoc}
	 */
    public List<Program> getProgramsByHealthFacilityIds(List<Long> healthFacilityIds) {
        return programRepository.findProgramsByHealthFacilityIds(healthFacilityIds);
    }

}
