package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.adminservice.model.entity.Classification;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the Classification module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 *
 * @author Karthick M
 */
@Repository
public interface ClassificationRepository extends JpaRepository<Classification, Long> {

    String GET_CLASSIFICATIONS = "From Classification as classification where classification.country.id=:countryId"
            + " AND classification.isDeleted = false AND classification.isActive = true";

    /**
     * Retrieves a list of Classifications for a given country that are not marked as deleted.
     * <p>
     * This method utilizes a custom query defined by the {@code GET_CLASSIFICATIONS} constant to fetch
     * classifications from the database. It filters the classifications by the provided country ID and
     * excludes any entries that have been marked as deleted. This ensures that the results are relevant
     * and up-to-date with the current active classifications.
     * </p>
     *
     * @param countryId The ID of the country for which classifications are being retrieved.
     * @return A list of {@link Classification} entities that match the given country ID and are not marked as deleted.
     */
    @Query(value = GET_CLASSIFICATIONS)
    List<Classification> getClassifications(@Param("countryId") Long countryId);
}
