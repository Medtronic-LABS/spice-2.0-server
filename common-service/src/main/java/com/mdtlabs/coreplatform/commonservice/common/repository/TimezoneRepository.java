package com.mdtlabs.coreplatform.commonservice.common.repository;

import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;

/**
 * <p>
 * This interface extends {@link GenericRepository}, inheriting its CRUD operations. It is specifically
 * tailored for {@link Timezone} entities, providing an abstraction layer over the underlying data access technology.
 * Use this repository to perform database operations related to timezones.
 * </p>
 */
@Repository
public interface TimezoneRepository extends GenericRepository<Timezone> {
}
