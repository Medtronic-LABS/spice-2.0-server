package com.mdtlabs.coreplatform.commonservice.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Role;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class OrganizationUtilTest {

    @InjectMocks
    private OrganizationUtil organizationUtil;

    @Mock
    private OrganizationRepository organizationRepository;

    @Mock
    private RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;

    @Test
    void initiateMap() {
        //given
        List<Organization> organizations = TestDataProvider.getOrganizationList();

        //when
        when(organizationRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(organizations);

        //then
        Map<Long, List<Long>> response = organizationUtil.initiateMap();
        assertNotNull(response);
    }

    @Test
    void getParentChildTenantMapWithException() {
        //then
        Map<Long, List<Long>> response = organizationUtil.getParentChildTenantMap();
        assertNull(response);
    }

    @Test
    void getParentChildTenantMap() {
        //given
        List<Organization> organizations = TestDataProvider.getOrganizationList();

        ListOperations operations = mock(ListOperations.class);
        List<Map<Long, List<Long>>> roleListRedis = new ArrayList<>();
        Map<Long, List<Long>> applicationRoles = new HashMap<>();
        applicationRoles.put(TestConstants.ONE, new ArrayList<>());
        applicationRoles.get(TestConstants.ONE).add(TestConstants.TWO);
        applicationRoles.put(TestConstants.FOUR, List.of(TestConstants.FIVE));
        List<Role> roles = List.of(TestDataProvider.getRole());
        roleListRedis.add(applicationRoles);

        //when
        when(redisTemplate.opsForList()).thenReturn(operations);
        when(organizationRepository.findByIsDeletedFalseAndIsActiveTrue()).thenReturn(organizations);
        when(operations.range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(new ArrayList<>());
        when(operations.leftPush(Constants.ROLE_REDIS_KEY, roles)).thenReturn(TestConstants.ONE);
        when(operations.range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(roleListRedis);

        //then
        Map<Long, List<Long>> response = organizationUtil.getParentChildTenantMap();
        assertNotNull(response);
    }

    @Test
    void testInitiateRolesMapNullDataInRedis() {
        //given
        ListOperations operations = mock(ListOperations.class);

        //when
        when(redisTemplate.opsForList()).thenReturn(operations);
        when(operations.range(Constants.ORGANIZATION_REDIS_KEY, Constants.ZERO, Constants.ONE)).thenReturn(new ArrayList<>());

        //then
        Map<Long, List<Long>> response = organizationUtil.getParentChildTenantMap();
        assertNull(response);
    }
}