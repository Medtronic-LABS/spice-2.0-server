package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.NutritionLifestyle;


/**
 * <p>
 * This is the repository which acts as link between server side and database. 
 * This class is used to perform all the complaints module action in database.
 * In query annotation (nativeQuery = true), the below query perform like SQL. 
 * Otherwise it performs like HQL.
 *  </p>
 *
 * @since Jun 30, 2022
 * @author Karthick Murugesan
 */
@Repository
public interface NutritionLifestyleRepository extends JpaRepository<NutritionLifestyle, Long> {
   
    /**
	 * <p>
	 * This method used to get the list of lifestyle.
	 * </p>
	 *
	 * @return List of NutritionLifestyle
	 */
	public List<NutritionLifestyle> findByIsDeletedFalseAndIsActiveTrue();

	/**
	 * <p>
	 * This method used to get the list of lifestyle.
	 * </p>
	 *
	 * @Param ids
	 * @return Set of NutritionLifestyle
	 */
	public Set<NutritionLifestyle> findByIdInAndIsDeletedFalseAndIsActiveTrue(Set<Long> ids);

}
