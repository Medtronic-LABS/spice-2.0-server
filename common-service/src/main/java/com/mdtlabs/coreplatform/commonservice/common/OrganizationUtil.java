package com.mdtlabs.coreplatform.commonservice.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;

/**
 * <p>
 * Utility class for handling operations related to Organization entities.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
@Component
public class OrganizationUtil {

    private final RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;

    public final OrganizationRepository organizationRepository;

    private List<Organization> organizationList = new ArrayList<>();

    private Map<Long, List<Long>> parentChildTenantMap = new HashMap<>();

    public OrganizationUtil(RedisTemplate<String, Map<Long, List<Long>>> redisTemplate, OrganizationRepository organizationRepository) {
        this.redisTemplate = redisTemplate;
        this.organizationRepository = organizationRepository;
    }

    /**
     * <p>
     * Initializes a map where each key is an organization ID and the value is a list of tenant IDs associated with that organization.
     * The method retrieves all active and non-deleted organizations, gets the tenant IDs for each organization, and adds the organization's own ID to the list.
     * The resulting map is then returned.
     * </p>
     *
     * @return A map where the key is an organization ID and the value is a list of tenant IDs associated with that organization.
     */
    public Map<Long, List<Long>> initiateMap() {
        Map<Long, List<Long>> childTenantMap = new HashMap<>();
        organizationList = organizationRepository.findByIsDeletedFalseAndIsActiveTrue();
        for (Organization organization : organizationList) {
            List<Long> childids = getTenantIds(organization.getId());
            childids.add(organization.getId());
            childTenantMap.put(organization.getId(), childids);
        }
        return childTenantMap;
    }

    /**
     * <p>
     * Retrieves the tenant IDs associated with a given parent ID.
     * The method first filters the tenants based on the parent ID, then iteratively filters the tenants of the resulting tenants until no more tenants are found.
     * The resulting list of tenant IDs is then returned.
     * </p>
     *
     * @param parentId The ID of the parent for which to retrieve the tenant IDs.
     * @return A list of tenant IDs associated with the given parent ID.
     */
    public List<Long> getTenantIds(Long parentId) {
        List<Long> childTenants = filterTenants(parentId);
        List<Long> tempTenantIds = childTenants;
        while (!tempTenantIds.isEmpty()) {
            tempTenantIds = filterTenants(tempTenantIds);
            childTenants.addAll(tempTenantIds);
        }
        return childTenants;
    }

    /**
     * <p>
     * Retrieves the tenant IDs associated with a given parent ID.
     * The method first filters the tenants based on the parent ID, then iteratively filters the tenants of the resulting tenants until no more tenants are found.
     * The resulting list of tenant IDs is then returned.
     * </p>
     *
     * @param parentIds The ID of the parent for which to retrieve the tenant IDs.
     * @return A list of tenant IDs associated with the given parent ID.
     */
    public List<Long> filterTenants(List<Long> parentIds) {
        List<Long> childs = new ArrayList<>();
        for (Long parentId : parentIds) {
            List<Long> filteredTenants = organizationList.stream()
                    .filter(org -> !Objects.isNull(org.getParentOrganizationId())
                            && org.getParentOrganizationId().equals(parentId))
                    .map(BaseEntity::getId).toList();
            childs.addAll(filteredTenants);
        }
        return childs;
    }

    /**
     * <p>
     * Filters tenants based on a parent ID.
     * The method filters the tenants whose parent organization ID matches the provided parent ID,
     * and returns a list of the IDs of the resulting tenants.
     * </p>
     *
     * @param parentId The ID of the parent for which to filter the tenants.
     * @return A list of tenant IDs whose parent organization ID matches the provided parent ID.
     */
    public List<Long> filterTenants(Long parentId) {
        List<Organization> filteredTenants = organizationList.stream()
                .filter(org -> !Objects.isNull(org.getParentOrganizationId())
                        && org.getParentOrganizationId().equals(parentId))
                .toList();
        return new ArrayList<>(filteredTenants.stream().map(BaseEntity::getId).toList());
    }

    /**
     * <p>
     * Retrieves the map of parent-child tenant relationships.
     * The method first initializes the roles map, then returns the map of parent-child tenant relationships.
     * </p>
     *
     * @return A map where the key is a parent tenant ID and the value is a list of child tenant IDs.
     */
    public Map<Long, List<Long>> getParentChildTenantMap() {
        initiateRolesMap();
        return parentChildTenantMap;
    }

    /**
     * <p>
     * Initializes the roles map from Redis.
     * The method retrieves the roles map from Redis. If the roles map is null or empty, it reloads the data into Redis.
     * The resulting roles map is then returned.
     * </p>
     */
    private void initiateRolesMap() {
        List<Map<Long, List<Long>>> roleListRedis = new ArrayList<>();
        try {
            roleListRedis = redisTemplate.opsForList()
                    .range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, Constants.ONE);

        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        if (!Objects.isNull(roleListRedis)  && roleListRedis.isEmpty()) {
            roleListRedis = loadRedisData(roleListRedis);
        }
        if (!Objects.isNull(roleListRedis) && !roleListRedis.isEmpty()) {
            parentChildTenantMap = roleListRedis.getFirst();
        } else {
            Logger.logWarn("No data available in roleListRedis to initialize parentChildTenantMap.");
            parentChildTenantMap = null;
        }
    }

    /**
     * <p>
     * Loads organization data into Redis.
     * The method first clears the provided list of roles, then initializes a map of application roles.
     * The application roles map is then pushed to Redis under the key "organization".
     * The method then retrieves the list of roles from Redis and assigns it to the provided list of roles.
     * If an exception occurs during this process, it is caught and logged, and the method continues to return the list of roles.
     * </p>
     *
     * @param roleListRedis The list of roles to clear and then populate with the roles retrieved from Redis.
     * @return The list of roles retrieved from Redis.
     */
    private List<Map<Long, List<Long>>> loadRedisData(List<Map<Long, List<Long>>> roleListRedis) {
        try {
            roleListRedis.clear();
            Map<Long, List<Long>> applicationRoles = initiateMap();
            redisTemplate.opsForList().leftPush("organization", applicationRoles);
            roleListRedis = redisTemplate.opsForList()
                    .range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, Constants.ONE);
        } catch (Exception exception) {
            Logger.logError(exception.getMessage(), exception);
        }
        return roleListRedis;
    }

}
