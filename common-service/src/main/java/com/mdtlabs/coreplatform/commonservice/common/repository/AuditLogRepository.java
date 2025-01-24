package com.mdtlabs.coreplatform.commonservice.common.repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Audit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * <p>
 *   This repository contains the needed customized functions for audit.
 * </p>it
 *
 * @author Tamilarasi Shanmugasundaram
 */
@Repository
public interface AuditLogRepository extends JpaRepository<Audit,Long> {

}