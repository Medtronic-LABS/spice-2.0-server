package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;

/**
 * Repository interface for {@link Symptom} entities.
 * <p>
 * This interface extends {@link JpaRepository} to provide CRUD operations and additional query methods
 * for {@link Symptom} entities. It facilitates communication between the application and the database
 * for operations related to symptoms. Custom query methods can be defined here for specific symptom-related
 * queries.
 * </p>
 */
@Repository
public interface SymptomRepository extends JpaRepository<Symptom, Long> {

    /**
     * Finds a list of {@link Symptom} entities by their category.
     * <p>
     * This method queries the database for symptoms that match the specified category. It returns a list
     * of symptoms that belong to the given category. This can be useful for filtering symptoms based on
     * their categorization.
     * </p>
     *
     * @param category The category of symptoms to search for.
     * @return A list of {@link Symptom} entities that belong to the specified category.
     */
    List<Symptom> findByCategoryAndIsDeletedFalseAndIsActiveTrue(String category);

    /**
     * Finds symptoms by id
     * 
     * @return A list of {@link Symptom} entities
     */
    public List<Symptom> findByIsDeletedFalseAndIsActiveTrue();  

    /**
     * Finds all symptoms
     * 
     * @return A list of {@link Symptom} entities
     */
    List<Symptom> findAllByIsDeletedFalseAndIsActiveTrue();

    List<Symptom> findByNameInAndType(List<String> symptoms, String type);
}
