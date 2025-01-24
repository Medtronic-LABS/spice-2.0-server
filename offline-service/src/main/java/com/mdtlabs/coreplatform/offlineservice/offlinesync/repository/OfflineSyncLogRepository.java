package com.mdtlabs.coreplatform.offlineservice.offlinesync.repository;

import com.mdtlabs.coreplatform.offlineservice.common.model.OfflineSyncLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * This class is a repository class to perform operation on offline sync log repositories
 * </p>
 *
 * @author Praveen Created on July 03, 2024.
 */
@Repository
public interface OfflineSyncLogRepository extends JpaRepository<OfflineSyncLog, Long> {

}
