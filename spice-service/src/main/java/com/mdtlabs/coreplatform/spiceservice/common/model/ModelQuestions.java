package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * <p>
 * This class is an entity used to represent questions from the application.
 * </p>
 *
 * @author Niraimathi S created on Jun 30, 2022 
 *
 */
@Entity
@Data
@Table(name = TableConstants.MODEL_QUESTIONS)
public class ModelQuestions extends MetaBaseEntity {

    private static final long serialVersionUID = 1L;

	@Column(name =  FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name =  FieldConstants.DISPLAY_ORDER)
    private int displayOrder;

    @Column(name = FieldConstants.IS_DEFAULT)
    private boolean isDefault;

    @Column(name =  FieldConstants.TYPE)
    private String type;

    @Column(name =  FieldConstants.WORKFLOW)
    private String workflow;

    @Column(name = FieldConstants.IS_MANDATORY)
    private boolean isMandatory;
    
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	    @JoinColumn(name = FieldConstants.QUESTION_ID)
    private List<ModelAnswers> modelAnswers;

}
