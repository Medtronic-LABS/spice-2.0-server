package com.mdtlabs.coreplatform.commonservice.common.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.commonservice.common.service.GenericService;


/**
 * <p>
 * Generic Controller used to perform any action like read and write.
 * </p>
 * 
 * @author Divya created on July 22, 2024
 */
@ResponseBody
public class GenericController<T extends BaseEntity> {

	private final GenericService<T> genericService;

	@Autowired
	public GenericController(GenericService<T> genericService) {
        this.genericService = genericService;
    }

	/**
	 * <p>
	 * Saves a given entity of type {@code T}.
	 * Use this method to insert a new entity or update an existing entity in the database.
	 * </p>
	 *
	 * @param entity the entity to save; must not be {@code null}
	 * @return {@link ResponseEntity} the saved entity; will never be {@code null}
	 */
	@PostMapping
	public ResponseEntity<Object> save(@RequestBody T entity) {
		try {
			return new ResponseEntity<>(genericService.save(entity), HttpStatus.OK);
		} catch (Exception e) {
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	/**
	 * <p>
	 * Retrieves all instances of the entity type {@code T}.
	 * This method should be used to fetch all records of {@code T} from the database.
	 * </p>
	 *
	 * @return a {@link ResponseEntity} list of entities of type {@code T}; if no entities are found, an empty list is returned
	 */
	@GetMapping
	public ResponseEntity<List<T>> findAll() {
		try {
			return new ResponseEntity<>(genericService.findAll(), HttpStatus.OK);
		} catch (Exception e) {
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	/**
	 * <p>
	 * Retrieves an entity by its id.
	 * Use this method to fetch a single entity of type {@code T} from the database by its identifier.
	 * </p>
	 *
	 * @param id the id of the entity to retrieve; must not be {@code null}
	 * @return {@link ResponseEntity} the entity with the given id or {@code null} if none found
	 */
	@GetMapping("/{id}")
	public ResponseEntity<T> findById(@PathVariable(FieldConstants.ID) Long id) {
		try {
			T entity = genericService.findById(id);
			if (entity != null) {
				return new ResponseEntity<>(entity, HttpStatus.OK);
			} else {
				return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
			}
		} catch (Exception e) {
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

}
