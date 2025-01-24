package com.mdtlabs.coreplatform.spiceservice.patient.repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.WgsData;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * This is the repository class for communicate link between server side and database for WGS data.
 *
 * @author Premkalyan created on Nov 08, 2024
 */
@Repository
public interface WgsDataRepository extends JpaRepository<WgsData, Long> {

    // Method to find WgsData by sex, given values, and indicators
    List<WgsData> findBySexAndGivenAndIndicator(Integer sex, Double given, String indicator);
}
