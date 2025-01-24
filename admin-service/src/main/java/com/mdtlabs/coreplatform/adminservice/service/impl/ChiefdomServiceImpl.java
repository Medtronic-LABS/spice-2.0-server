package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.ChiefdomRequestDTO;
import com.mdtlabs.coreplatform.adminservice.repository.ChiefdomRepository;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.adminservice.service.ChiefdomService;
import com.mdtlabs.coreplatform.adminservice.service.VillageService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * The ChiefdomServiceImpl class is a service implementation that handles CRUD related to to Chiefdom entity.
 * </p>
 *
 * @author Divya S
 */
@Service
public class ChiefdomServiceImpl implements ChiefdomService {

    public final ChiefdomRepository chiefdomRepository;
    public final UserServiceApiInterface userServiceApiInterface;
    public final ModelMapper modelMapper;
    public final HealthFacilityService healthFacilityService;
    public final VillageService villageService;

    public ChiefdomServiceImpl(ChiefdomRepository chiefdomRepository,
                                UserServiceApiInterface userServiceApiInterface,
                                ModelMapper modelMapper, HealthFacilityService healthFacilityService,
                                VillageService villageService) {
        this.chiefdomRepository = chiefdomRepository;
        this.userServiceApiInterface = userServiceApiInterface;
        this.modelMapper = modelMapper;
        this.healthFacilityService = healthFacilityService;
        this.villageService = villageService;
    }

    /**
     * {@inheritDoc}
     */
    public List<Map<String, Object>> getChiefdomCountByDistrictIds(List<Long> districtIds, Boolean isActive) {
        return chiefdomRepository.getChiefdomCountByDistrictIds(districtIds, isActive);
    }

    /**
     * {@inheritDoc}
     */
    public List<Long> activateOrDeactivateChiefdoms(Long countryId, Long accountId, boolean isActive) {
        List<Chiefdom> chiefdoms = chiefdomRepository.findByCountryIdAndDistrictIdAndIsActive(
                countryId, accountId, !isActive);
        List<Long> tenantIds = new ArrayList<>();
        if (!chiefdoms.isEmpty()) {
            chiefdoms.stream().forEach(chiefdom -> {
                chiefdom.setActive(isActive);
                tenantIds.add(chiefdom.getTenantId());
            });
            chiefdomRepository.saveAll(chiefdoms);
        }
        return tenantIds;
    }

    /**
     * {@inheritDoc}
     */
    public List<Map<String, Object>> getChiefdomCountByCountryIds(List<Long> countryIds, Boolean isActive) {
        List<Map<String, Object>> chiefdoms = chiefdomRepository.getChiefdomCountByCountryIds(countryIds, isActive);
        if (Objects.nonNull(chiefdoms)) {
            return chiefdoms;
        }
        return new ArrayList<>();
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<ChiefdomDTO> getAllChiefdoms(SearchRequestDTO requestDto) {
        String searchTerm = requestDto.getSearchTerm();
        ResponseListDTO<ChiefdomDTO> response = new ResponseListDTO<>();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        searchTerm = !Objects.isNull(searchTerm) ? searchTerm.strip() : null;
        Pageable pageable = null;
        if (0 < requestDto.getLimit()) {
            pageable = Pagination.setPagination(requestDto.getSkip(), requestDto.getLimit(), Constants.UPDATED_AT,
                    Constants.BOOLEAN_FALSE);
        }
        Organization organization = userServiceApiInterface.getOrganizationById(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), requestDto.getTenantId(), CommonUtil.getClient()).getBody();
        if (!Objects.isNull(organization)) {
            Long countryId = organization.getFormName().equals(Constants.COUNTRY) ? organization.getFormDataId() : null;
            Long chiefdomId = Objects.isNull(countryId) ? organization.getFormDataId() : null;
            Page<Map<String, Object>> chiefdoms = chiefdomRepository.getChiefdoms(
                    searchTerm, countryId, chiefdomId, pageable);
            if (!Objects.isNull(chiefdoms) && !chiefdoms.isEmpty()) {
                modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
                response.setData(modelMapper.map(chiefdoms.stream().toList(),
                        new TypeToken<List<ChiefdomDTO>>() {}.getType()));
                response.setTotalCount(chiefdoms.getTotalElements());
            }
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<ChiefdomDTO> getChiefdomList(ChiefdomRequestDTO requestDto) {
        Logger.logInfo("Fetching chiefdom List");
        String searchTerm = requestDto.getSearchTerm();
        Pageable pageable = Pagination.setPagination(requestDto.getSkip(), requestDto.getLimit());
        if (StringUtils.isNotEmpty(searchTerm)) {
            searchTerm = searchTerm.replaceAll(Constants.SEARCH_TERM, Constants.EMPTY);
        }
        Organization organization = userServiceApiInterface.getOrganizationById(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), requestDto.getTenantId(), CommonUtil.getClient()).getBody();
        ResponseListDTO<ChiefdomDTO> response = new ResponseListDTO<>();
        if (!Objects.isNull(organization)) {
            Long countryId = organization.getFormName().equals(Constants.COUNTRY) ? organization.getFormDataId() : null;
            Long districtId = Objects.isNull(countryId) ? organization.getFormDataId() : null;
            Page<Chiefdom> chiefdoms = chiefdomRepository.findChiefdoms(
                    searchTerm, countryId, districtId, pageable);
            response.setTotalCount(chiefdoms.getTotalElements());
            List<ChiefdomDTO> chiefdomDTOS = new ArrayList<>();
            if (!chiefdoms.isEmpty()) {
                constructChiefdomListResponse(chiefdomDTOS, response, chiefdoms);
            }
        }
        return response;
    }

    /**
     * <p>
     * Constructs the response object for chiefdom List.
     * </p>
     *
     * @param chiefdomDTOS - chiefdom DTO list
     * @param response              - response object with chiefdom list and its count.
     * @param chiefdoms        - chiefdoms
     */
    private void constructChiefdomListResponse(List<ChiefdomDTO> chiefdomDTOS,
                                                ResponseListDTO<ChiefdomDTO> response, Page<Chiefdom> chiefdoms) {
        List<Map<String, Object>> noOfHealthFacilities = healthFacilityService.getHealthFacilityCountByChiefdomIds(
                chiefdoms.stream().map(BaseEntity::getId).toList(), Boolean.TRUE);
        for (Chiefdom chiefdom : chiefdoms) {
            ChiefdomDTO chiefdomDTO = new ChiefdomDTO(chiefdom.getId(),
                    chiefdom.getName(), chiefdom.getTenantId());
            noOfHealthFacilities.forEach(healthFacility -> {
                Long chiefdomId = (Long) healthFacility.get(Constants.CHIEFDOM_ID);
                if (chiefdom.getId().equals(chiefdomId)) {
                    chiefdomDTO.setHealthFacilityCount(((Long) healthFacility.get(Constants.COUNT)));
                }
            });
            chiefdomDTOS.add(chiefdomDTO);
        }
        response.setData(chiefdomDTOS);
    }

    /**
     * {@inheritDoc}
     */
    @Transactional
    public Chiefdom updateChiefdom(Chiefdom chiefdom) {
        Chiefdom existingChiefdom = chiefdomRepository
                .findByIdAndIsDeletedFalseAndIsActive(chiefdom.getId(), Constants.BOOLEAN_TRUE);

        if (Objects.isNull(existingChiefdom)) {
            Logger.logError(ErrorConstants.CHIEFDOM_NOT_FOUND + chiefdom.getId());
            throw new DataNotFoundException(29010, chiefdom.getId().toString());
        }
        if (!Objects.isNull(chiefdom.getName())) {
            boolean exists = chiefdomRepository.existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(
                    chiefdom.getName(), chiefdom.getId());
            if(exists) {
                Logger.logError(chiefdom.getName() + " already exist(s) in the regional database");
                throw new DataConflictException(29011, chiefdom.getName());
            }
            existingChiefdom.setName(chiefdom.getName());
            Organization organization = new Organization(chiefdom.getTenantId(), chiefdom.getName());
            userServiceApiInterface.updateOrganization(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                    modelMapper.map(organization, OrganizationDTO.class), CommonUtil.getClient());
        }
        return chiefdomRepository.save(existingChiefdom);
    }

    /**
     * {@inheritDoc}
     */
    @Transactional
    public Chiefdom createChiefdom(Chiefdom chiefdom, List<Village> villages) {
        Chiefdom existingChiefdom = chiefdomRepository
                .findByNameIgnoreCaseAndIsDeletedFalse(chiefdom.getName().strip());
        if (!Objects.isNull(existingChiefdom)) {
            Logger.logError(chiefdom.getName() + " already exist(s) in the regional database");
            throw new DataConflictException(29011, chiefdom.getName());
        }
        chiefdom.setName(chiefdom.getName().strip());
        chiefdom = chiefdomRepository.save(chiefdom);
        villageService.addVillagesToChiefdom(chiefdom, villages);
        return chiefdom;
    }

    /**
     * {@inheritDoc}
     */
    public ChiefdomDTO getChiefdomDetails(SearchRequestDTO requestDto) {
        if (Objects.isNull(requestDto.getId()) || Objects.isNull(requestDto.getTenantId())) {
            Logger.logError("ID and Tenant ID should not be empty.");
            throw new DataNotAcceptableException(20004);
        }
        Map<String, Object> chiefdom = chiefdomRepository.getChiefdomDetails(requestDto.getId());
        if (chiefdom.isEmpty()) {
            Logger.logError(ErrorConstants.CHIEFDOM_NOT_FOUND + requestDto.getId());
            throw new DataNotFoundException(29010, requestDto.getId().toString());
        }
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        ChiefdomDTO chiefdomDTO = modelMapper.map(chiefdom, ChiefdomDTO.class);
        chiefdomDTO.setUsers(userServiceApiInterface.getUsersByTenantIds(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDto));
        return chiefdomDTO;
    }
}
