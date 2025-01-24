package com.mdtlabs.coreplatform.commonservice.common;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.cache.RedisCacheManagerBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.jedis.JedisClientConfiguration;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.repository.configuration.EnableRedisRepositories;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext.SerializationPair;
import redis.clients.jedis.JedisPoolConfig;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.MetaCodeDetails;

/**
 * <p>
 * Redis configuration on user token.
 * </p>
 *
 * @author Maria Antony Praveen Created on 04 Mar 2024
 *
 */
@Configuration
@EnableRedisRepositories
public class RedisConfig {

    @Value("${spring.redis.host}")
    private String redisHost;

    @Value("${spring.redis.port}")
    private int redisPort;

    /**
     * <p>
     * Connection factory bean to register jedis connection factory.
     * </p>
     *
     * @return JedisConnectionFactory - bean
     */
    @Bean
    public JedisConnectionFactory connectionFactory() {
        RedisStandaloneConfiguration redisStandaloneconfiguration = new RedisStandaloneConfiguration();
        redisStandaloneconfiguration.setHostName(redisHost);
        redisStandaloneconfiguration.setPort(redisPort);
        JedisPoolConfig poolConfig = new JedisPoolConfig();
        poolConfig.setMaxIdle(Constants.ONE);
        poolConfig.setBlockWhenExhausted(Constants.BOOLEAN_TRUE);
        JedisClientConfiguration.JedisPoolingClientConfigurationBuilder jedisConfig =
                (JedisClientConfiguration.JedisPoolingClientConfigurationBuilder) JedisClientConfiguration.builder();
        jedisConfig.poolConfig(poolConfig);
        JedisClientConfiguration jedisClientConfig = jedisConfig.build();
        return new JedisConnectionFactory(redisStandaloneconfiguration, jedisClientConfig);
    }
    
    /**
     * <p>
     * Redis cache manager builder customizer bean is to register builder.
     * </p>
     *
     * @return RedisCacheManagerBuilderCustomizer - bean
     */
    @Bean
    public RedisCacheManagerBuilderCustomizer redisCacheManagerBuilderCustomizer() {
        return builder -> builder.withCacheConfiguration(Constants.TOKENS, RedisCacheConfiguration.defaultCacheConfig());
    }

    /**
     * <p>
     * Cache configuration bean is to register redis cache configuration.
     * </p>
     *
     * @return RedisCacheConfiguration - bean
     */
    @Bean
    public RedisCacheConfiguration cacheConfiguration() {
        return RedisCacheConfiguration.defaultCacheConfig().disableCachingNullValues()
                .serializeValuesWith(SerializationPair.fromSerializer(new GenericJackson2JsonRedisSerializer()));
    }
    
    /**
     * <p>
     * Redis template bean is to register redis template.
     * </p>
     *
     * @return RedisTemplate - bean
     */
    @Bean
    RedisTemplate<String, Map<String, List<String>>> redisTemplate() {
        final RedisTemplate<String, Map<String, List<String>>> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory());
        template.afterPropertiesSet();
        return template;
    }

    /**
     * <p>
     * Creates a RedisTemplate bean for API role permissions.
     * This RedisTemplate is configured to handle a Map where the key is a String and the value is a Map.
     * The inner Map's key is a String and its value is a List of Strings.
     * The method sets the connection factory for the RedisTemplate and initializes it.
     * </p>
     *
     * @return A RedisTemplate configured for handling API role permissions.
     */
    @Bean
    RedisTemplate<String, Map<String, Map<String, List<String>>>> redisApiPermissionTemplate() {
        final RedisTemplate<String, Map<String, Map<String, List<String>>>> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory());
        template.afterPropertiesSet();
        return template;
    }

    /**
     * <p>
     * Creates a RedisTemplate bean for organization data.
     * This RedisTemplate is configured to handle a Map where the key is a String and the value is a Map.
     * The inner Map's key is a Long and its value is a List of Longs.
     * The method sets the connection factory for the RedisTemplate and initializes it.
     * </p>
     *
     * @return A RedisTemplate configured for handling organization data.
     */
    @Bean
    RedisTemplate<String, Map<Long, List<Long>>> redisTemplateForOrganization() {
        final RedisTemplate<String, Map<Long, List<Long>>> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory());
        template.afterPropertiesSet();
        return template;
    }

    /**
     * <p>
     * Creates a RedisTemplate bean for MetaCodeDetails data.
     * This RedisTemplate is configured to handle a Map where the key is a String and the value is MetaCodeDetails.
     * The method sets the connection factory for the RedisTemplate and initializes it.
     * </p>
     *
     * @return A RedisTemplate configured for handling MetaCodeDetails data.
     */
    @Bean
    RedisTemplate<String, Map<String, MetaCodeDetails>> redisTemplateForMetaCode() {
        final RedisTemplate<String, Map<String, MetaCodeDetails>> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory());
        template.afterPropertiesSet();
        return template;
    }

    /**
     * <p>
     * Creates a RedisTemplate bean for MetaDataDTO data.
     * This RedisTemplate is configured to handle a Map where the key is a String and the value is a List of MetaDataDTO.
     * The method sets the connection factory for the RedisTemplate and initializes it.
     * </p>
     *
     * @return A RedisTemplate configured for handling MetaDataDTO data.
     */
    @Bean
    RedisTemplate<String, Map<String, List<MetaDataDTO>>> redisTemplateForMetaData() {
        final RedisTemplate<String, Map<String, List<MetaDataDTO>>> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory());
        template.afterPropertiesSet();
        return template;
    }
    
}
