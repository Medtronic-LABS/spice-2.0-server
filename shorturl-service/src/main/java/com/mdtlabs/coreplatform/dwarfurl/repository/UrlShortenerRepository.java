package com.mdtlabs.coreplatform.dwarfurl.repository;

import com.mdtlabs.coreplatform.dwarfurl.common.model.UrlShortener;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * The UrlShortenerRepository interface provides the contract for CRUD operations on UrlShortener entities.
 * </p>
 *
 * @author JohnKennedy Created on 04 sep 2024
 */
@Repository
public interface UrlShortenerRepository extends JpaRepository<UrlShortener, Long> {

    /**
     * Finds a UrlShortener entity by its token and active status.
     *
     * @param shortKey the short key representing the URL
     * @param isActive the active status of the URL
     * @return UrlShortener the URL shortener entity
     */
    UrlShortener findByTokenAndIsActive(String shortKey, boolean isActive);

    /**
     * Finds a UrlShortener entity by its token.
     *
     * @param shortKey the short key representing the URL
     * @return UrlShortener the URL shortener entity
     */
    UrlShortener findByToken(String shortKey);
}
