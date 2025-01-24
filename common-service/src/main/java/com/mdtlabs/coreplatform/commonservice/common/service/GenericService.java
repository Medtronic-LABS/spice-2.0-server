package com.mdtlabs.coreplatform.commonservice.common.service;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;

/**
 * <p>
 * Provides generic service operations for entities extending {@link BaseEntity}.
 * This interface defines basic CRUD operations such as find all entities, save an entity, and find an entity by ID.
 * Implementations of this interface can be used to provide common service operations for any entity type.
 * </p>
 *
 * @author Divya created on Jul 19, 2023
 */
public interface GenericService<T extends BaseEntity> {

    /**
     * <p>
     * Retrieves all instances of the entity type {@code T}.
     * This method should be used to fetch all records of {@code T} from the database.
     * </p>
     *
     * @return a {@link List} of entities of type {@code T}; if no entities are found, an empty list is returned
     */
    List<T> findAll();

    /**
     * <p>
     * Saves a given entity of type {@code T}.
     * Use this method to insert a new entity or update an existing entity in the database.
     * </p>
     *
     * @param entity the entity to save; must not be {@code null}
     * @return the saved entity; will never be {@code null}
     */
    T save(T entity);

    /**
     * <p>
     * Retrieves an entity by its id.
     * Use this method to fetch a single entity of type {@code T} from the database by its identifier.
     * </p>
     *
     * @param id the id of the entity to retrieve; must not be {@code null}
     * @return the entity with the given id or {@code null} if none found
     */
    T findById(Long id);
}
