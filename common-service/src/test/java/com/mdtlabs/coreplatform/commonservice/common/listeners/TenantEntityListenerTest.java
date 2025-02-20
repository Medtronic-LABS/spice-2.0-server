package com.mdtlabs.coreplatform.commonservice.common.listeners;

import jakarta.persistence.EntityNotFoundException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import com.mdtlabs.coreplatform.commonservice.common.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;

@ExtendWith(MockitoExtension.class)
class TenantEntityListenerTest {

    @InjectMocks
    private TenantEntityListener tenantEntityListener;

    @Test
    void prePersistAndUpdate() {
        //given
        TestDataProvider.init();
        TenantBaseEntity tenantBaseEntity = TestDataProvider.getTenantBaseEntity();

        //when
        TestDataProvider.getStaticMock();

        //then
        tenantEntityListener.prePersistAndUpdate(tenantBaseEntity);
        TestDataProvider.cleanUp();
        Assertions.assertEquals(TestConstants.ONE, tenantBaseEntity.getTenantId());
        Assertions.assertNotNull(tenantBaseEntity);

    }

    @Test
    void testPrePersistAndUpdate() {
        //given
        BaseEntity baseEntity = TestDataProvider.getBaseEntity();

        //then
        tenantEntityListener.prePersistAndUpdate(baseEntity);
        Assertions.assertNotNull(baseEntity);
    }

    @Test
    void toVerifyPrePersistAndUpdateWithNull() {
        //given
        TenantBaseEntity tenantBaseEntity = TestDataProvider.getTenantBaseEntity();
        tenantBaseEntity.setTenantId(TestConstants.ONE);

        //then
        tenantEntityListener.prePersistAndUpdate(tenantBaseEntity);
        Assertions.assertEquals(TestConstants.ONE, tenantBaseEntity.getTenantId());
        Assertions.assertNull(tenantBaseEntity.getId());
    }

    @Test
    void preRemove() {
        //given
        TestDataProvider.init();
        TenantBaseEntity tenantBaseEntity = TestDataProvider.getTenantBaseEntity();
        tenantBaseEntity.setTenantId(TestConstants.ONE);

        //when
        TestDataProvider.getStaticMock();

        //then
        tenantEntityListener.preRemove(tenantBaseEntity);
        TestDataProvider.cleanUp();
        Assertions.assertNotNull(tenantBaseEntity);
    }

    @Test
    void testPreRemove() {
        //given
        BaseEntity tenantBaseEntity = TestDataProvider.getBaseEntity();

        //then
        tenantEntityListener.preRemove(tenantBaseEntity);
        Assertions.assertNotNull(tenantBaseEntity);
    }

    @Test
    void toVerifyPreRemoveWithException() {
        //given
        TenantBaseEntity tenantBaseEntity = TestDataProvider.getTenantBaseEntity();
        tenantBaseEntity.setTenantId(TestConstants.FIVE);
        TestDataProvider.init();

        //when
        TestDataProvider.getStaticMock();

        //then
        Assertions.assertThrows(EntityNotFoundException.class, () -> tenantEntityListener.preRemove(tenantBaseEntity));
        TestDataProvider.cleanUp();
    }
}