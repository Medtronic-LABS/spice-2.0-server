package com.mdtlabs.coreplatform.commonservice.common.audit;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Audit;
import com.mdtlabs.coreplatform.commonservice.common.repository.AuditLogRepository;
import org.hibernate.event.spi.PostDeleteEvent;
import org.hibernate.event.spi.PostDeleteEventListener;
import org.hibernate.event.spi.PostInsertEvent;
import org.hibernate.event.spi.PostInsertEventListener;
import org.hibernate.event.spi.PostUpdateEvent;
import org.hibernate.event.spi.PostUpdateEventListener;
import org.hibernate.persister.entity.EntityPersister;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * This class is responsible for listening to Hibernate post-insert, post-update,
 * and post-delete events to perform audit-related tasks.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram
 */
@Configuration
public class AuditListener implements PostInsertEventListener, PostUpdateEventListener, PostDeleteEventListener {

    private final AuditLogRepository auditLogRepository;

    @Autowired
    public AuditListener(AuditLogRepository auditLogRepository) {
        this.auditLogRepository = auditLogRepository;
    }

    @Override
    public void onPostInsert(PostInsertEvent event) {
        try {
            if (!event.getEntity().getClass().isAnnotationPresent(RestrictAudit.class)) {
                List<Audit> auditListToBeSaved = new ArrayList<>();
                String[] propertyNames = event.getPersister().getPropertyNames();
                Object[] states = event.getState();
                for (int i = 0; i < propertyNames.length; i++) {
                    if (null != states[i]) {
                        Audit audit = new Audit();
                        audit.setEntityId(Long.parseLong(event.getId().toString()));
                        audit.setAction(Constants.CREATE.toUpperCase());
                        audit.setEntity(event.getEntity().getClass().getSimpleName());
                        audit.setColumnName(propertyNames[i]);
                        audit.setNewValue(String.valueOf(states[i]));
                        auditListToBeSaved.add(audit);

                    }
                }
                auditLogRepository.saveAll(auditListToBeSaved);
                auditListToBeSaved.clear();
            }
        } catch (Exception e) {
            Logger.logInfo(e.getMessage());
        }
    }

    @Override
    public void onPostUpdate(PostUpdateEvent event) {
        try {
            if (!event.getEntity().getClass().isAnnotationPresent(RestrictAudit.class)) {
                List<Audit> auditListToBeSaved = new ArrayList<>();
                String[] propertyNames = event.getPersister().getPropertyNames();
                Object[] currentState = event.getState();
                Object[] previousState = event.getOldState();
                for (int i = 0; i < currentState.length; i++) {
                    if (!Objects.deepEquals(currentState[i], previousState[i])
                            && Objects.nonNull(currentState[i])
                            && (Objects.nonNull(currentState[i]) && Objects.nonNull(previousState[i]))) {
                        Audit audit = new Audit();
                        audit.setEntityId(Long.parseLong(event.getId().toString()));
                        audit.setAction(Constants.UPDATE.toUpperCase());
                        audit.setEntity(event.getEntity().getClass().getSimpleName());
                        audit.setColumnName(propertyNames[i]);
                        audit.setOldValue(String.valueOf(previousState[i]));
                        audit.setNewValue(String.valueOf(currentState[i]));
                        auditListToBeSaved.add(audit);
                    }
                }
                if (!auditListToBeSaved.isEmpty()) {
                    auditLogRepository.saveAll(auditListToBeSaved);
                    auditListToBeSaved.clear();
                }
            }
        } catch (Exception e) {
            Logger.logInfo(e.getMessage());
        }
    }

    @Override
    public void onPostDelete(PostDeleteEvent postDeleteEvent) {
        // This method intentionally left empty as there are currently no actions to be performed
        // Deleting an entity does not require any additional actions in this context
    }

    @Override
    public boolean requiresPostCommitHandling(EntityPersister entityPersister) {
        return false;
    }
}