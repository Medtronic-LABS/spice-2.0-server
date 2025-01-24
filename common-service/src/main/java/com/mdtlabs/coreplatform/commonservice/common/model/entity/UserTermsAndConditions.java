package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotBlank;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;


/**
 * <p>
 * This class is an Entity for user terms and conditions which contains necessary fields.
 * </p>
 *
 * @author Divya S created on Oct 22, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_USER_TERMS_AND_CONDITION)
public class UserTermsAndConditions extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @NotBlank(message = ErrorConstants.FORM_INPUT_NOT_NULL)
    @Column(name = FieldConstants.FORM_INPUT)
    private String formInput;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;
}
