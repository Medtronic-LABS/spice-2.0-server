package com.mdtlabs.coreplatform.commonservice.common.audit;

import jakarta.annotation.PostConstruct;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.PersistenceUnit;
import org.hibernate.event.service.spi.EventListenerRegistry;
import org.hibernate.event.spi.EventType;
import org.hibernate.internal.SessionFactoryImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


/**
 * <p>
 * This class is responsible for configuring Hibernate event listeners
 * to handle audit-related tasks such as tracking entity insertions and updates.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram
 */
@Component
public class HibernateListenerConfig {
    @PersistenceUnit
    private EntityManagerFactory emf;

    private final AuditListener auditListener;


    @Autowired
    public HibernateListenerConfig(AuditListener auditListener) {
        this.auditListener = auditListener;
    }

    @PostConstruct
    protected void init() {
        SessionFactoryImpl sessionFactory = emf.unwrap(SessionFactoryImpl.class);
        EventListenerRegistry registry = sessionFactory.getServiceRegistry().getService(EventListenerRegistry.class);
        assert registry != null;
        registry.getEventListenerGroup(EventType.POST_INSERT).appendListener(auditListener);
        registry.getEventListenerGroup(EventType.POST_UPDATE).appendListener(auditListener);
    }
}
