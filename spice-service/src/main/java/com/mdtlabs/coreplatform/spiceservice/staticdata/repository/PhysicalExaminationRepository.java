package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.PhysicalExamination;


/**
 * <p>
 * This is the repository which acts as link between server side and database. 
 * This class is used to perform all the complaints module action in database.
 * In query annotation (nativeQuery = true), the below query perform like SQL. 
 * Otherwise it performs like HQL.
 * </p>
 *
 * @since Jun 30, 2022
 * @author Karthick Murugesan
 */
@Repository
public interface PhysicalExaminationRepository extends JpaRepository<PhysicalExamination, Long> {

	/**
	 * <p>
	 * This method is used to get list of Physical Examination entities.
	 * </p>
	 *
	 * @return Set of Physical Examination
	 */
	public List<PhysicalExamination> findByIsDeletedFalseAndIsActiveTrue();
	
	/**
	 * <p>
	 * This method is used to get list of physical examination entities by id's.
	 * </p>
	 *
	 * @param ids
	 * @return Set of Physical Examination
	 */
    public Set<PhysicalExamination> findByIdInAndIsDeletedFalseAndIsActiveTrue(Set<Long> ids);
}
