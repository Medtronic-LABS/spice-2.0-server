package com.mdtlabs.coreplatform.commonservice.common;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.autoconfigure.cache.RedisCacheManagerBuilderCustomizer;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RedisConfigTest {

    @InjectMocks
    private RedisConfig redisConfig;

    @Test
    void connectionFactory() {
        //given
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_HOST, TestConstants.LOCALHOST);
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_PORT, 6782);

        //then
        JedisConnectionFactory response = redisConfig.connectionFactory();
        Assertions.assertNotNull(response);
        Assertions.assertEquals(6782, response.getPort());
    }

    @Test
    void redisCacheManagerBuilderCustomizer() {
        //then
        RedisCacheManagerBuilderCustomizer response = redisConfig.redisCacheManagerBuilderCustomizer();
        Assertions.assertNotNull(response);
    }

    @Test
    void cacheConfiguration() {
        //then
        RedisCacheConfiguration response = redisConfig.cacheConfiguration();
        Assertions.assertNotNull(response);
    }

    @Test
    void redisTemplate() {
        //given
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_HOST, TestConstants.LOCALHOST);
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_PORT, 6782);

        //then
        RedisTemplate<String, Map<String, List<String>>> response = redisConfig.redisTemplate();
        Assertions.assertNotNull(response);
    }

    @Test
    void redisApiPermissionTemplate() {
        //given
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_HOST, TestConstants.LOCALHOST);
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_PORT, 6782);

        //then
        RedisTemplate<String, Map<String, Map<String, List<String>>>> response = redisConfig.redisApiPermissionTemplate();
        Assertions.assertNotNull(response);
    }

    @Test
    void redisTemplateForOrganization() {
        //given
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_HOST, TestConstants.LOCALHOST);
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_PORT, 6782);

        //then
        RedisTemplate<String, Map<Long, List<Long>>> response = redisConfig.redisTemplateForOrganization();
        Assertions.assertNotNull(response);
    }

    @Test
    void redisTemplateForMetaCode() {
        //given
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_HOST, TestConstants.LOCALHOST);
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_PORT, 6782);

        //then
        RedisTemplate<String, Map<String, MetaCodeDetails>> response = redisConfig.redisTemplateForMetaCode();
        Assertions.assertNotNull(response);
    }

    @Test
    void redisTemplateForMetaData() {
        //given
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_HOST, TestConstants.LOCALHOST);
        ReflectionTestUtils.setField(redisConfig, TestConstants.REDIS_PORT, 6782);

        //then
        RedisTemplate<String, Map<String, List<MetaDataDTO>>> response = redisConfig.redisTemplateForMetaData();
        Assertions.assertNotNull(response);
    }
}