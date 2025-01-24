package com.mdtlabs.coreplatform.commonservice.common.service.impl;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.repository.GenericRepository;
import com.mdtlabs.coreplatform.commonservice.common.service.GenericService;


/**
 * <p>
 * This service class contain all the business logic and perform all the
 * operation here.
 * </p>
 *
 * @author Divya created on July 20, 2024
 */
@Service
public class GenericServiceImpl<T extends BaseEntity> implements GenericService<T> {

    private final GenericRepository<T> genericRepository;

    @Autowired
    public GenericServiceImpl(GenericRepository<T> genericRepository) {
        this.genericRepository = genericRepository;
    }

	/**
	 * {@inheritDoc}
	 */
    @Override
    public List<T> findAll() {
        try {
            return genericRepository.findAll();
        } catch (Exception exception) {
            Logger.logError(exception);
            throw exception;
        }
    }

	/**
	 * {@inheritDoc}
	 */
    @Override
    public T findById(Long id) {
        try {
            Optional<T> entity = genericRepository.findById(id);
            if (!entity.isEmpty()) {
                return entity.get();
            } else {
                return null;
            }
        } catch (Exception exception) {
            Logger.logError(exception);
            throw exception;
        }
    }

	/**
	 * {@inheritDoc}
	 */
    @Override
    public T save(T entity) {
        try {
            return genericRepository.save(entity);
        } catch (Exception exception) {
            Logger.logError(exception);
            throw exception;
        }
    }

	/**
	 * <p>
	 * Retrieves all instances of the entity type.
	 * </p>
	 *
	 * @param sort the {@link Sort} object specifying the properties to sort by and the direction for each property.
	 * @return a {@link List} of entities of type {@code T} sorted as per the {@link Sort} object; if no entities are found,
	 *         an empty list is returned.
     */
    public List<T> findAll(Sort sort) {
        try {
            return genericRepository.findAll(sort);
        } catch (Exception exception) {
            Logger.logError(exception);
            throw exception;
        }
    }
}
