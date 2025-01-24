package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import jakarta.transaction.Transactional;

import org.apache.commons.lang3.StringUtils;
import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.DistrictListDTO;
import com.mdtlabs.coreplatform.adminservice.repository.DistrictRepository;
import com.mdtlabs.coreplatform.adminservice.service.DistrictService;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.adminservice.service.ChiefdomService;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataConflictException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.DistrictRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.OrganizationDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * The DistrictServiceImpl class is a service implementation that handles CRUD related to to district entity.
 * </p>
 *
 * @author Divya S
 */
@Service
public class DistrictServiceImpl implements DistrictService {

    public final UserServiceApiInterface userServiceApiInterface;
    public final DistrictRepository districtRepository;
    public final ModelMapper modelMapper;
    public final ChiefdomService chiefdomService;
    public final HealthFacilityService healthFacilityService;
    private final RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;


    public DistrictServiceImpl(UserServiceApiInterface userServiceApiInterface,
                             DistrictRepository districtRepository, ModelMapper modelMapper,
                             ChiefdomService chiefdomService, HealthFacilityService healthFacilityService,
                             RedisTemplate<String, Map<Long, List<Long>>> redisTemplate) {
        this.userServiceApiInterface = userServiceApiInterface;
        this.districtRepository = districtRepository;
        this.modelMapper = modelMapper;
        this.chiefdomService = chiefdomService;
        this.healthFacilityService = healthFacilityService;
        this.redisTemplate = redisTemplate;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<DistrictDTO> getDistricts(SearchRequestDTO searchRequestDto) {
        if (CommonUtil.isCommunityApp(searchRequestDto.getAppTypes())) {
            Logger.logInfo(("Get district list by Country ID - ").concat(searchRequestDto.getCountryId().toString()));
            List<District> districts = districtRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(
                    searchRequestDto.getCountryId());
            List<DistrictDTO> districtResponse = new ArrayList<>();
            if (!Objects.isNull(districts) && !districts.isEmpty()) {
                districtResponse = modelMapper.map(districts, new TypeToken<List<DistrictDTO>>() {
                }.getType());
            }
            return new ResponseListDTO<>(districtResponse);
        }

        Logger.logDebug(("Get district list by passing tenant_id - ").concat(searchRequestDto.getTenantId().toString()));
        String searchTerm = searchRequestDto.getSearchTerm();
        Pageable pageable = null;
        ResponseListDTO<DistrictDTO> response = new ResponseListDTO<>();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        if (0 < searchRequestDto.getLimit()) {
            pageable = Pagination.setPagination(searchRequestDto.getSkip(), searchRequestDto.getLimit(),
                    Constants.UPDATED_AT, Constants.BOOLEAN_FALSE);
        }
        Organization organization = userServiceApiInterface.getOrganizationById(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), searchRequestDto.getTenantId(), CommonUtil.getClient()).getBody();
        if (!Objects.isNull(organization)) {
            Page<District> districts = districtRepository.findDistrictList(searchTerm, organization.getFormDataId(),
                    pageable);
            response.setTotalCount(districts.getTotalElements());
            if (!districts.stream().toList().isEmpty()) {
                response.setData(modelMapper.map(districts.stream().toList(), new TypeToken<List<DistrictDTO>>() {
                }.getType()));
            }
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<DistrictListDTO> getDistrictList(SearchRequestDTO searchRequestDTO) {
        Logger.logInfo("Fetch district details with given data");
        Pageable pageable = Pagination.setPagination(searchRequestDTO.getSkip(), searchRequestDTO.getLimit());
        String searchTerm = (!Objects.isNull(searchRequestDTO.getSearchTerm()))
                ? searchRequestDTO.getSearchTerm().replaceAll(Constants.SEARCH_TERM, Constants.EMPTY) : Constants.EMPTY;
        Organization organization = userServiceApiInterface.getOrganizationById(CommonUtil.getAuthToken(),
                UserSelectedTenantContextHolder.get(), searchRequestDTO.getTenantId(), CommonUtil.getClient()).getBody();
        List<DistrictListDTO> districtListDtos = new ArrayList<>();
        ResponseListDTO<DistrictListDTO> response = new ResponseListDTO<>();
        if (!Objects.isNull(organization)) {
            Page<District> pagedDistricts = districtRepository.findDistrictList(searchTerm, organization.getFormDataId(),
                    pageable);
            List<District> districts = pagedDistricts.stream().toList();

            response.setTotalCount(pagedDistricts.getTotalElements());
            constructDistrictListResponse(districtListDtos, districts);
        }
        response.setData(districtListDtos);
        return response;
    }

    /**
     * <p>
     * This method is used to construct a response list of district DTOs using the list of districts.
     * </p>
     *
     * @param districtListDtos {@link List<DistrictListDTO>} A list of district list DTOs to add
     *                       the constructed district list DTO is given
     * @param districts       {@link List<District>} A list of districts that provide the data like
     *                       ID, tenant ID, name etc., is given
     */
    private void constructDistrictListResponse(List<DistrictListDTO> districtListDtos, List<District> districts) {
        if (!districts.isEmpty()) {
            List<Long> districtIds = districts.stream().map(BaseEntity::getId).toList();
            List<Map<String, Object>> noOfChiefdoms = chiefdomService.
                    getChiefdomCountByDistrictIds(districtIds, Constants.BOOLEAN_TRUE);
            List<Map<String, Object>> noOfSites = healthFacilityService.getCountByDistrictIds(districtIds);
            for (District district : districts) {
                DistrictListDTO districtListDto = new DistrictListDTO(district.getId(), district.getName(),
                        district.getTenantId());
                noOfChiefdoms.forEach(operatingUnit -> {
                    if (district.getId() == operatingUnit.get(Constants.DISTRICT_ID)) {
                        districtListDto.setChiefdomCount(((Long) operatingUnit.get(Constants.COUNT)).intValue());
                    }
                });
                noOfSites.forEach(site -> {
                    Long districtId = (Long) site.get(Constants.DISTRICT_ID);
                    if (district.getId().equals(districtId)) {
                        districtListDto.setHealthFacilityCount(((Long) site.get(Constants.COUNT)).intValue());
                    }
                });
                districtListDtos.add(districtListDto);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public DistrictDTO getDistrictDetails(SearchRequestDTO searchRequestDTO) {
        if (Objects.isNull(searchRequestDTO.getId()) || Objects.isNull(searchRequestDTO.getTenantId())) {
            Logger.logError("Tenant id  should not be empty or null.");
            throw new DataNotAcceptableException(1005);
        }
        Logger.logDebug(("Getting district information by passing id - ").concat(searchRequestDTO.getId().toString()));
        District district = districtRepository.findByIdAndTenantIdAndIsActiveAndIsDeleted(searchRequestDTO.getId(),
                searchRequestDTO.getTenantId(), true, false);
        if (Objects.isNull(district)) {
            Logger.logError(("No District found for this ID - ").concat(searchRequestDTO.getId().toString()));
            throw new DataNotFoundException(2102);
        }
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        DistrictDTO districtDTO = modelMapper.map(district, DistrictDTO.class);
        districtDTO.setUsers(userServiceApiInterface.getUsersByTenantIds(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), searchRequestDTO));
        return districtDTO;
    }

    /**
     * {@inheritDoc}
     */
    public District createDistrict(DistrictRequestDTO districtRequestDTO) {
        Logger.logInfo("Creating district details with given data");
        if (Objects.isNull(districtRequestDTO)) {
            Logger.logError("Request should not be empty.");
            throw new BadRequestException(1003);
        }
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        District district = modelMapper.map(districtRequestDTO, District.class);
        district.setCountryId(districtRequestDTO.getCountryId());
        return districtRepository.save(district);
    }

    /**
     * {@inheritDoc}
     */
    public District updateDistrict(DistrictRequestDTO districtRequestDTO) {
        Logger.logInfo("Updating district details with given data");
        if (Objects.isNull(districtRequestDTO)) {
            Logger.logError("Request should not be empty.");
            throw new BadRequestException(1003);
        }
        District existingDistrict = validateDistrict(districtRequestDTO);
        modelMapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        modelMapper.map(districtRequestDTO, existingDistrict);
        Organization organization = new Organization();
        organization.setId(districtRequestDTO.getTenantId());
        organization.setName(districtRequestDTO.getName());
        userServiceApiInterface.updateOrganization(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                modelMapper.map(organization, OrganizationDTO.class), CommonUtil.getClient());

        existingDistrict.setName(districtRequestDTO.getName());
        return districtRepository.save(existingDistrict);
    }

    /**
     * This method is used to validate an district by checking if it exists, if its name is unique,
     * and if its clinical workflows are not empty or null.
     *
     * @param districtRequestDTO {@link DistrictRequestDTO} The information about an district and its
     *                         associated workflows is given
     * @return {@link District} The validated district is returned
     */
    private District validateDistrict(DistrictRequestDTO districtRequestDTO) {
        District existingDistrict = districtRepository.findByIdAndIsDeleted(districtRequestDTO.getId(), false);
        if (Objects.isNull(existingDistrict)) {
            Logger.logError(("No District found for this ID - ").concat(districtRequestDTO.getId().toString()));
            throw new DataNotFoundException(2102);
        }
        boolean isExist = districtRepository.existsByNameIgnoreCaseAndIsDeletedFalseAndIdNot(districtRequestDTO.getName(),
                districtRequestDTO.getId());
        if (isExist) {
            Logger.logError(("District with same name already exists - ").concat(districtRequestDTO.getName()));
            throw new DataConflictException(2101, districtRequestDTO.getName().strip());
        }
        return existingDistrict;
    }

    /**
     * {@inheritDoc}
     */
    @Transactional
    public Boolean activateOrDeactivateDistrict(DistrictRequestDTO districtRequestDTO) {
        String status = Boolean.TRUE.equals(districtRequestDTO.getIsActive()) ? Constants.ACTIVATE : Constants.DEACTIVATE;
        Logger.logDebug(status.concat(" the district org id: ").concat(districtRequestDTO.getTenantId().toString()));
        Boolean isActive = districtRequestDTO.getIsActive();
        District district = districtRepository.findByTenantIdAndIsDeletedFalseAndIsActive(districtRequestDTO.getTenantId(),
                !isActive);
        if (Objects.isNull(district)) {
            Logger.logError("No District found for this ID");
            throw new DataNotFoundException(2102);
        }
        district.setReason(districtRequestDTO.getReason());
        district.setStatus(districtRequestDTO.getStatus());
        district.setActive(isActive);
        district = districtRepository.save(district);
        List<Long> tenantIds = new ArrayList<>();
        tenantIds.addAll(chiefdomService.activateOrDeactivateChiefdoms(null, district.getId(), isActive));
        List<HealthFacility> healthFacilities = healthFacilityService.activateOrDeactivateHealthFacility(null, district.getId(), null, isActive);
        List<Long> healthFacilitiesTenantIds = new ArrayList<>();
        List<String> healthFacilityFhirIds = new ArrayList<>();
        healthFacilities.forEach(healthFacility -> {
            healthFacilitiesTenantIds.add(healthFacility.getTenantId());
            healthFacilityFhirIds.add(healthFacility.getFhirId());
        });
        tenantIds.addAll(healthFacilitiesTenantIds);
        tenantIds.add(districtRequestDTO.getTenantId());
        userServiceApiInterface.activateOrDeactivateOrg(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                tenantIds, isActive, healthFacilityFhirIds, CommonUtil.getClient());
        userServiceApiInterface.activateOrDeactivateUser(CommonUtil.getAuthToken(), UserSelectedTenantContextHolder.get(),
                tenantIds, isActive, CommonUtil.getClient());
        try {
            redisTemplate.delete(Constants.ORGANIZATION_REDIS_KEY);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        return !Objects.isNull(district);
    }

    /**
     * {@inheritDoc}
     */
    @Transactional
    public ResponseListDTO<DistrictDTO> getDeactivatedDistricts(SearchRequestDTO searchRequestDto) {
        Logger.logInfo("Get inactive districts list");
        ResponseListDTO<DistrictDTO> response = new ResponseListDTO<>();
        String searchTerm = searchRequestDto.getSearchTerm();
        if (!CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        if (StringUtils.isNotBlank(searchTerm)) {
            searchTerm = searchTerm.strip();
        }
        Long countryId = null;
        if (!Objects.isNull(searchRequestDto.getTenantId()) && Constants.ZERO != searchRequestDto.getTenantId()) {
            Organization organization = userServiceApiInterface.getOrganizationById(CommonUtil.getAuthToken(),
                    UserSelectedTenantContextHolder.get(), searchRequestDto.getTenantId(), CommonUtil.getClient()).getBody();
            countryId = !Objects.isNull(organization) ? organization.getFormDataId() : null;
        }
        Pageable pageable = Pagination.setPagination(searchRequestDto.getSkip(), searchRequestDto.getLimit(),
                Constants.UPDATED_AT, false);
        Page<District> districts = districtRepository.getDeactivatedDistricts(searchTerm, countryId, pageable);
        if (!Objects.isNull(districts) && !districts.isEmpty()) {
            response.setTotalCount(districts.getTotalElements());
            response.setData(modelMapper.map(districts.stream().toList(), new TypeToken<List<DistrictDTO>>() {
            }.getType()));
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Map<String, Object>> getDistrictCountByCountryIds(List<Long> ids, Boolean isActive) {
        List<Map<String, Object>> districts = districtRepository.getDistrictCountByCountryIds(ids, isActive);
        if (Objects.nonNull(districts)) {
            return districts;
        }
        return new ArrayList<>();
    }
}
