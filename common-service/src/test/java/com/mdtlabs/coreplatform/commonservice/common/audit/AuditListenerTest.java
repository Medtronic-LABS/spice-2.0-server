package com.mdtlabs.coreplatform.commonservice.common.audit;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.repository.AuditLogRepository;
import org.hibernate.event.spi.PostInsertEvent;
import org.hibernate.event.spi.PostUpdateEvent;
import org.hibernate.persister.entity.EntityPersister;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class AuditListenerTest {

    AuditLogRepository auditLogRepository = mock(AuditLogRepository.class);
    AuditListener auditListener = new AuditListener(auditLogRepository);

    @Test
    void testOnPostInsert() {
        UserDTO userDTO = new UserDTO();
        EntityPersister entityPersister = mock(EntityPersister.class);
        String[] s = {"a", "b", "c"};
        PostInsertEvent postInsertEvent = new PostInsertEvent(userDTO, "1", s, entityPersister, null);
        when(entityPersister.getPropertyNames()).thenReturn(s);
        auditListener.onPostInsert(postInsertEvent);
        verify(auditLogRepository, times(1)).saveAll(any());
    }

    @Test
    void testOnPostInsertWhenExceptionOccurs() {
        UserDTO userDTO = new UserDTO();
        EntityPersister entityPersister = mock(EntityPersister.class);
        String[] s = {"a", "b", "c"};
        PostInsertEvent postInsertEvent = new PostInsertEvent(userDTO, "1", s, entityPersister, null);
        doThrow(RuntimeException.class).when(entityPersister).getPropertyNames();
        auditListener.onPostInsert(postInsertEvent);
        verify(auditLogRepository, times(0)).saveAll(any());
    }

    @Test
    void testOnPostUpdate() {
        UserDTO userDTO = new UserDTO();
        EntityPersister entityPersister = mock(EntityPersister.class);
        String[] sampleStrings = {"a", "b", "c"};
        String[] testStrings = {"d", "e", "f"};
        PostUpdateEvent postInsertEvent = new PostUpdateEvent(userDTO, "1", sampleStrings, testStrings, null, entityPersister, null);
        when(entityPersister.getPropertyNames()).thenReturn(sampleStrings);
        auditListener.onPostUpdate(postInsertEvent);
        verify(auditLogRepository, times(1)).saveAll(any());
    }

    @Test
    void testOnPostUpdateWhenExceptionOccurs() {
        UserDTO userDTO = new UserDTO();
        EntityPersister entityPersister = mock(EntityPersister.class);
        String[] sampleStrings = {"a", "b", "c"};
        String[] testStrings = {"d", "e", "f"};
        PostUpdateEvent postInsertEvent = new PostUpdateEvent(userDTO, "1", sampleStrings, testStrings, null, entityPersister, null);
        doThrow(RuntimeException.class).when(entityPersister).getPropertyNames();
        auditListener.onPostUpdate(postInsertEvent);
        verify(auditLogRepository, times(0)).saveAll(any());
    }

    @Test
    void requiresPostCommitHandling() {
        //given
        EntityPersister entityPersister = mock(EntityPersister.class);

        //then
        boolean response = auditListener.requiresPostCommitHandling(entityPersister);
        assertFalse(response);
    }

}