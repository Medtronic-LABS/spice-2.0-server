package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.mdtlabs.coreplatform.spiceservice.common.model.ModelQuestions;

/**
 * This interface is responsible for performing database operations between
 * server and ModelQuestions entity.
 *
 * @author Niraimathi S
 *
 */
public interface ModelQuestionsRepository extends JpaRepository<ModelQuestions, Long> {

	public static final String GET_MODEL_QUESTIONS_BY_WORKFLOW = "SELECT modelQuestion from ModelQuestions as modelQuestion where "
	+ "modelQuestion.countryId = :countryId AND ((:workflow) IS null OR modelQuestion.workflow in (:workflow))"
	+ "AND modelQuestion.isDeleted=false AND modelQuestion.isActive=true order by displayOrder asc" ;
	
	/**
	 * <p>
	 * Gets list of ModelQuestions entities based on countryId and isDeleted fields.
	 * </p>
	 *
	 * @param countryId country Id
	 * @param isDeleted isDeleted value
	 * @return List of ModelQuestions entities
	 */
	public List<ModelQuestions> findByCountryIdAndIsDeleted(Long countryId, boolean isDeleted);

	/**
	 * <p>
	 * Gets list of ModelQuestions entities based on isDefault and isDeleted Fields.
	 * </p>
	 *
	 * @param isDefault isDefault value
	 * @param isDeleted isDeleted value
	 * @return List of ModelQuestions entities
	 */
	public List<ModelQuestions> findByIsDefaultAndIsDeleted(boolean isDefault, boolean isDeleted);
	
	/**
	 * <p>
	 * Gets list of active model questions.
	 * </p>
	 * 
	 * @return List of ModelQuestions entities
	 */
    public List<ModelQuestions> findByIsDeletedFalseAndIsActiveTrue();
    
    /**
     * <p>
     * Gets list of model questions by country id.
     * </p>
     * 
     * @param countryId
     * @param false
     * @param true
     * @return List of ModelQuestions entities.
     */
	@Query(value = GET_MODEL_QUESTIONS_BY_WORKFLOW)
    public List<ModelQuestions> getModelQuestionByWorkflow(@Param("countryId") Long countryId,
        @Param("workflow") Set<String> workflow
	);

}
