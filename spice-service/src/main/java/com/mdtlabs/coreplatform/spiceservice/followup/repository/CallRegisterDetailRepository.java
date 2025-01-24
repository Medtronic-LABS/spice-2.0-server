package com.mdtlabs.coreplatform.spiceservice.followup.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegisterDetail;

/**
 * Repository interface for {@link CallRegisterDetail} entities.
 * <p>
 * This interface extends {@link JpaRepository}, providing CRUD operations for {@link CallRegisterDetail} entities.
 * It is used to interact with the database layer to perform operations on the Call Register Detail table.
 * </p>
 * <p>
 * Utilizing Spring Data JPA's repository abstraction, it simplifies the data access layer by eliminating boilerplate
 * code and providing a mechanism to implement repository interfaces automatically at runtime.
 * </p>
 *
 * @author Yogeshwaran Mohan
 * Created on Mar 26, 2024.
 */
@Repository
public interface CallRegisterDetailRepository extends JpaRepository<CallRegisterDetail, Long> {
}
