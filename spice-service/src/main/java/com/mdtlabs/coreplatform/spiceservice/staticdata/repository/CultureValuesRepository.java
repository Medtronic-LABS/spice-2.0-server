package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.CultureValues;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CultureValuesRepository extends JpaRepository<CultureValues, Long>{

    List<CultureValues> findByCultureId(long cultureId);

}
