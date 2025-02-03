package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.adminservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.apiinterface.UserServiceApiInterface;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.RequestDTO;
import com.mdtlabs.coreplatform.adminservice.model.dto.HealthFacilityFilterDTO;
import com.mdtlabs.coreplatform.adminservice.repository.HealthFacilityRepository;
import com.mdtlabs.coreplatform.adminservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.adminservice.service.ClinicalWorkflowService;
import com.mdtlabs.coreplatform.adminservice.service.HealthFacilityService;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.OrganizationUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.CommonRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.ResponseListDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserResponseDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.HealthFacility;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.commonservice.common.util.Pagination;

/**
 * <p>
 * This service class contain all the business logic for health facility module and
 * perform all the health facility operation here.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
@Service
public class HealthFacilityServiceImpl implements HealthFacilityService {

    private final VillageRepository villageRepository;
    private final HealthFacilityRepository healthFacilityRepository;
    private final UserServiceApiInterface userServiceApiInterface;
    private final ClinicalWorkflowService clinicalWorkflowService;
    private final FhirServiceApiInterface fhirServiceApiInterface;
    private final OrganizationUtil organizationUtil;
    private final ModelMapper modelMapper;

    @Autowired
    public HealthFacilityServiceImpl(VillageRepository villageRepository, HealthFacilityRepository healthFacilityRepository,
                                     UserServiceApiInterface userServiceApiInterface, ClinicalWorkflowService clinicalWorkflowService,
                                     FhirServiceApiInterface fhirServiceApiInterface, OrganizationUtil organizationUtil,
                                     ModelMapper modelMapper) {
        this.villageRepository = villageRepository;
        this.healthFacilityRepository = healthFacilityRepository;
        this.userServiceApiInterface = userServiceApiInterface;
        this.clinicalWorkflowService = clinicalWorkflowService;
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.organizationUtil = organizationUtil;
        this.modelMapper = modelMapper;
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacility createHealthFacility(HealthFacilityRequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        HealthFacility healthFacility = mapper.map(request, HealthFacility.class);
        List<Village> villages = villageRepository.findByIdInAndIsDeletedAndIsActive(request.getLinkedVillageIds(), false, true);
        List<Long> workflowIds = new ArrayList<>();
        workflowIds.addAll(request.getClinicalWorkflowIds());
        if (!Objects.isNull(request.getCustomizedWorkflowIds()) && !request.getCustomizedWorkflowIds().isEmpty()) {
            workflowIds.addAll(request.getCustomizedWorkflowIds());
        }

        List<ClinicalWorkflow> clinicalWorkflows = clinicalWorkflowService.getWorkflowsByIds(workflowIds);
        List<ClinicalWorkflow> healthFacilityclinicalWorkflows = clinicalWorkflows.stream().filter(workflow ->
            workflow.getModuleType().equals(Constants.CLINICAL)).toList();
        if (!Objects.isNull(request.getCustomizedWorkflowIds()) && !request.getCustomizedWorkflowIds().isEmpty()) {
            List<ClinicalWorkflow> healthFacilitycustomizedWorkflows = clinicalWorkflows.stream().filter(workflow ->
                    workflow.getModuleType().equals(Constants.CUSTOMIZED)).toList();
            healthFacility.setCustomizedWorkflows(healthFacilitycustomizedWorkflows);
        }
        healthFacility.setClinicalWorkflows(healthFacilityclinicalWorkflows);
        healthFacility.setLinkedVillages(villages);
        healthFacilityRepository.save(healthFacility);
        return healthFacility;
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO addHealthFacilityUser(UserRequestDTO request) {
        return userServiceApiInterface.addAdmin(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), request).getBody();
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO updateHealthFacilityUser(UserRequestDTO request) {
        return userServiceApiInterface.updateAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(),
                request).getBody();
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacilityDTO getHealthFacilityDetails(SearchRequestDTO request) {
        if (Objects.isNull(request.getId()) || Objects.isNull(request.getTenantId())) {
            throw new BadRequestException(20004);
        }
        HealthFacility healthFacility = healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(request.getId(), request.getTenantId(), false, true);
        if (Objects.isNull(healthFacility)) {
            throw new DataNotFoundException(27007);
        }
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setAppTypes(request.getAppTypes());
        requestDTO.setTenantIds(List.of(request.getTenantId()));
        List<UserResponseDTO> linkedPeerUsers = userServiceApiInterface.getPeerSupervisors(CommonUtil.getAuthToken(),
                CommonUtil.getAuthCookie(), UserContextHolder.getUserDto().getClient(), requestDTO).getBody();
        HealthFacilityDTO healthFacilityDTO = new ModelMapper().map(healthFacility, HealthFacilityDTO.class);
        healthFacilityDTO.setPeerSupervisors(linkedPeerUsers);
        return healthFacilityDTO;
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacility updateHealthFacility(HealthFacilityRequestDTO request) {
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
        HealthFacility healthFacility = healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(request.getId(), request.getTenantId(), false, true);
        if (Objects.isNull(healthFacility)) {
            throw new DataNotFoundException(27007);
        }
        mapper.map(request, healthFacility);
        if (!Objects.isNull(request.getLinkedVillageIds()) && !request.getLinkedVillageIds().isEmpty()) {
            List<Village> villages = villageRepository.findByIdInAndIsDeletedAndIsActive(request.getLinkedVillageIds(), false, true);
            healthFacility.setLinkedVillages(villages);
        }
        if (!Objects.isNull(request.getClinicalWorkflowIds()) && !request.getClinicalWorkflowIds().isEmpty()) {
            List<Long> workflowIds = new ArrayList<>();
            workflowIds.addAll(request.getClinicalWorkflowIds());
            if (!Objects.isNull(request.getCustomizedWorkflowIds()) && !request.getCustomizedWorkflowIds().isEmpty()) {
                workflowIds.addAll(request.getCustomizedWorkflowIds());
            }
            List<ClinicalWorkflow> clinicalWorkflows = clinicalWorkflowService.getWorkflowsByIds(workflowIds);
            List<ClinicalWorkflow> healthFacilityclinicalWorkflows = clinicalWorkflows.stream().filter(workflow ->
                    workflow.getModuleType().equals(Constants.CLINICAL)).collect(Collectors.toList());
            List<ClinicalWorkflow> healthFacilitycustomizedWorkflows = clinicalWorkflows.stream().filter(workflow ->
                    workflow.getModuleType().equals(Constants.CUSTOMIZED)).collect(Collectors.toList());
            healthFacility.setClinicalWorkflows(healthFacilityclinicalWorkflows);
            if (!healthFacilitycustomizedWorkflows.isEmpty()) {
                healthFacility.setCustomizedWorkflows(healthFacilitycustomizedWorkflows);
            }
        }
        userServiceApiInterface.updateHealthFacilityOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(), request);
        request.setFhirId(healthFacility.getFhirId());
        fhirServiceApiInterface.updateOrganization(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
                UserContextHolder.getUserDto().getClient(), request);

        return healthFacilityRepository.save(healthFacility);
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<HealthFacilityDTO> getHealthFacilities(SearchRequestDTO request) {
        ResponseListDTO<HealthFacilityDTO> response = new ResponseListDTO<>();
        if (!CommonUtil.isValidSearchData(request.getSearchTerm(), Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        Page<HealthFacility> healthFacilities = getHealthFacilitiesPage(request);

        if (!Objects.isNull(healthFacilities) && !healthFacilities.isEmpty()) {
            response.setData(mapper.map(healthFacilities.toList(), new TypeToken<List<HealthFacilityDTO>>() {
            }.getType()));
            response.setTotalCount(healthFacilities.getTotalElements());
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public UserResponseDTO deleteHealthFacilityAdmin(SearchRequestDTO request) {
        List<HealthFacility> healthFacilities = null;
        if (!Objects.isNull(request.getTenantIds()) && !request.getTenantIds().isEmpty()) {
            healthFacilities = healthFacilityRepository.findByTenantIdInAndIsDeleted(
                request.getTenantIds(), Boolean.FALSE);
                request.setHealthFacilityLinkedVillages(healthFacilities.stream().flatMap( facility -> facility.getLinkedVillages().stream().map(BaseEntity::getId)).toList());
        }
        return userServiceApiInterface.removeAdmin(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(),
            UserContextHolder.getUserDto().getClient(), request).getBody();
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacilityDTO> getHealthFacilitiesByCountryId(Long countryId) {
        List<HealthFacility> healthFacilities = healthFacilityRepository.findByCountryIdAndIsDeleted(countryId, false);
        return new ModelMapper().map(healthFacilities, new TypeToken<List<HealthFacilityDTO>>() {
            }.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacilityDTO> getHealthFacilitiesByTenants(List<Long> tenantIds) {
        List<HealthFacility> healthFacilities = healthFacilityRepository.findByTenantIdInAndIsDeleted(tenantIds, false);
        return new ModelMapper().map(healthFacilities, new TypeToken<List<HealthFacilityDTO>>() {
        }.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacilityDTO> getHealthFacilitiesByChiefdomId(Long chiefdomId) {
        List<HealthFacility> healthFacilities = healthFacilityRepository.findByChiefdomIdAndIsDeleted(chiefdomId, false);
        return new ModelMapper().map(healthFacilities, new TypeToken<List<HealthFacilityDTO>>() {
        }.getType());
    }

    /**
     * {@inheritDoc}
     */
    public List<VillageDTO> getVillagesByFacility(List<Long> tenantIds) {
        List<HealthFacility> healthFacilities = healthFacilityRepository.findByTenantIdInAndIsDeleted(tenantIds, false);
        Set<Village> villages = new HashSet<>();
        if (!Objects.isNull(healthFacilities)) {
            healthFacilities.stream().forEach(healthFacilitity -> villages.addAll(healthFacilitity.getLinkedVillages()));
        }
        return new ModelMapper().map(villages, new TypeToken<List<VillageDTO>>() {
        }.getType());
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacilityRequestDTO getHealthFacilityByFhirId(RequestDTO requestDTO) {
        HealthFacility healthFacility = healthFacilityRepository.findByFhirIdAndIsDeletedFalseAndIsActiveTrue(requestDTO.getFhirId());
        if (Objects.isNull(healthFacility)) {
            throw new DataNotFoundException(27007);
        }
        return new ModelMapper().map(healthFacility, HealthFacilityRequestDTO.class);
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacilityDTO> getAllHealthFacilities() {
        return mapHealthFacilityToDTO(healthFacilityRepository.findAllByIsDeletedFalseAndIsActiveTrue());
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacilityDTO> getAllHealthFacilitiesByDistrictId(SearchRequestDTO requestDTO) {
        if (Objects.isNull(requestDTO.getDistrictId())) {
            throw new DataNotFoundException(1010);
        }
        return mapHealthFacilityToDTO(healthFacilityRepository.findAllByDistrictIdAndIsDeletedFalseAndIsActiveTrue(
                requestDTO.getDistrictId()));
    }

    /**
     * Converts a list of {@link HealthFacility} entities into a list of {@link HealthFacilityDTO} objects.
     * <p>
     * This method iterates over each {@link HealthFacility} in the provided list, creating a new {@link HealthFacilityDTO}
     * for each, and populating it with the corresponding properties from the {@link HealthFacility} entity. The resulting
     * list of {@link HealthFacilityDTO}s is then returned. This is useful for converting database entities into a form
     * that can be easily transferred over the network or processed by the front-end.
     * </p>
     *
     * @param healthFacilities A list of {@link HealthFacility} entities to be converted.
     * @return A list of {@link HealthFacilityDTO}s corresponding to the input list of {@link HealthFacility} entities.
     */
    private List<HealthFacilityDTO> mapHealthFacilityToDTO(List<HealthFacility> healthFacilities) {
        List<HealthFacilityDTO> healthFacilityDTOS = new ArrayList<>();
        for (HealthFacility healthFacility : healthFacilities) {
            HealthFacilityDTO healthFacilityDTO = new HealthFacilityDTO();
            healthFacilityDTO.setId(healthFacility.getId());
            healthFacilityDTO.setName(healthFacility.getName());
            healthFacilityDTO.setTenantId(healthFacility.getTenantId());
            healthFacilityDTO.setFhirId(healthFacility.getFhirId());
            healthFacilityDTOS.add(healthFacilityDTO);
        }
        return healthFacilityDTOS;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String deleteHealthFacility(SearchRequestDTO request) {
        HealthFacility healthFacility = healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(request.getId(), request.getTenantId(), false, true);
        healthFacility.setActive(false);
        healthFacility.setDeleted(true);
        healthFacilityRepository.save(healthFacility);
        return healthFacility.getFhirId();
    }

    /**
     * {@inheritDoc}
     */
    public void validateHealthFacility(SearchRequestDTO request) {
        HealthFacility healthFacility = healthFacilityRepository.findByIdAndTenantIdAndIsDeletedAndIsActive(request.getHealthFacilityId(),
                request.getTenantId(), false, true);
        if (Objects.isNull(healthFacility)) {
            throw new DataNotFoundException(27007);
        }
        List<Long> deletedVillages = new ArrayList<>(
                healthFacility.getLinkedVillages().stream().map(BaseEntity::getId).toList());
        deletedVillages.removeAll(request.getLinkedVillageIds());
        List<Village> villages = villageRepository.getUserLinkedVillage(deletedVillages);
        if (!Objects.isNull(villages) && !villages.isEmpty()) {
            throw new BadRequestException(27012);
        }
        userServiceApiInterface.validatePeerSupervisors(CommonUtil.getAuthToken(), CommonUtil.getAuthCookie(), CommonUtil.getClient(),request);
    }

    /**
     * {@inheritDoc}
     */
    public List<Map<String, Object>> getCountByDistrictIds(List<Long> countyIds) {
        return healthFacilityRepository.getCountByDistrictIds(countyIds);
    }

    /**
     * {@inheritDoc}
     */
    public List<Long> getFacilityVillageIdsByTenantId() {
        HealthFacility healthFacility = healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(UserSelectedTenantContextHolder.get());
        if (Objects.isNull(healthFacility)) {
            throw new DataNotFoundException(27007);
        }
        return Objects.isNull(healthFacility.getLinkedVillages()) || healthFacility.getLinkedVillages().isEmpty() ? new ArrayList<>() :
            healthFacility.getLinkedVillages().stream().map(BaseEntity::getId).toList();
    }

    /**
     * {@inheritDoc}
     */
    public List<Map<String, Object>> getHealthFacilityCountByCountryIds(List<Long> list, Boolean booleanTrue) {
        return healthFacilityRepository.getHealthFacilityCountByCountryIds(list, booleanTrue);
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacility> activateOrDeactivateHealthFacility(Long countryId, Long districtId, Long chiefdomId,
                                                boolean isActive) {
        List<HealthFacility> healthFacilities = healthFacilityRepository.findHealthFacilityDistrict(countryId, districtId, chiefdomId, !isActive);
        if (!healthFacilities.isEmpty()) {
            healthFacilities.stream().forEach(healthFacility -> healthFacility.setActive(isActive));
            healthFacilityRepository.saveAll(healthFacilities);
        }
        return healthFacilities;
    }

    /**
     * {@inheritDoc}
     */
    public ResponseListDTO<HealthFacilityFilterDTO> getHealthFacilitiesFilter(SearchRequestDTO request) {
        ResponseListDTO<HealthFacilityFilterDTO> response = new ResponseListDTO<>();
        if (!CommonUtil.isValidSearchData(request.getSearchTerm(), Constants.SEARCH_TERM)) {
            response.setTotalCount(0L);
            return response;
        }
        Page<HealthFacility> healthFacilities = getHealthFacilitiesPage(request);

        if (!Objects.isNull(healthFacilities) && !healthFacilities.isEmpty()) {
            response.setData(new ModelMapper().map(healthFacilities.stream().toList(), new TypeToken<List<HealthFacilityFilterDTO>>() {
            }.getType()));
            response.setTotalCount(healthFacilities.getTotalElements());
        }
        return response;
    }

    private Page<HealthFacility> getHealthFacilitiesPage(SearchRequestDTO request) {
        Pageable pageable = null;
        if (0 < request.getLimit()) {
            pageable = Pagination.setPagination(request.getSkip(), request.getLimit(), Constants.UPDATED_AT,
                    Constants.BOOLEAN_FALSE);
        }
        Set<Long> tenantIds = new HashSet<>();

        if (Objects.nonNull(request.getTenantId())) {
            tenantIds.addAll(organizationUtil.getParentChildTenantMap().get(request.getTenantId()));
        } else if (Objects.nonNull(request.getTenantIds())) {
            for (Long tenantId : request.getTenantIds()) {
                List<Long> ids = organizationUtil.getParentChildTenantMap().get(tenantId);
                if (Objects.nonNull(ids)) {
                    tenantIds.addAll(organizationUtil.getParentChildTenantMap().get(tenantId));
                }
            }
        }
        tenantIds = tenantIds.isEmpty() ? null : tenantIds;
        return healthFacilityRepository.getHealthFacilities(request.getSearchTerm(), request.getCountryId(), tenantIds, pageable);
    }

    /**
     * {@inheritDoc}
     */
    public List<Map<String, Object>> getHealthFacilityCountByChiefdomIds(List<Long> chiefdomIds, boolean isActive) {
        return healthFacilityRepository.getHealthFacilityCountByChiefdomIds(chiefdomIds, isActive);
    }
    
    /**
     * {@inheritDoc}
     */
    public Set<HealthFacility> getHealthFacilitiesByIds(Set<Long> ids) {
        return healthFacilityRepository.findByIdInAndIsDeletedAndIsActive(ids, false, true);
    }

    /**
     * {@inheritDoc}
     */
    public HealthFacility getHealthFacilityByTenantId(Long tenantId) {
        return healthFacilityRepository.findByTenantIdAndIsDeletedFalseAndIsActiveTrue(tenantId);
    }

    /**
     * @{inheritDoc}
     */
    @Override
    public List<HealthFacilityDTO> getDistrictHealthFacilitiesByTenantId(Long tenantId) {
        List<HealthFacility> healthFacilities = healthFacilityRepository.findDistrictFacilitiesByTenantId(tenantId);
        return healthFacilities.stream().map(
                healthFacility -> modelMapper.map(healthFacility, HealthFacilityDTO.class)).toList();
    }

    /**
     * {@inheritDoc}
     */
    public List<HealthFacility> getHealthFacilityByCountry(CommonRequestDTO requestDTO) {
        List<HealthFacility> sites = null;
        sites = healthFacilityRepository.getHealthFacilityByCountryAndTenant(requestDTO.getSearchTerm(), requestDTO.getTenantId(),
                requestDTO.getCountryId());
        return Objects.isNull(sites) ? new ArrayList<>() : sites;
    }
}
