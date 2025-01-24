package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

/**
 * <p>
 * This class is an Entity for Role Designation details which contains necessary fields.
 * </p>
 *
 * @author Divya S created on Oct 18, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_DESIGNATION)
public class Designation extends BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotNull(message = ErrorConstants.COUNTRY_ID_NOT_NULL)
    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @ManyToOne
    @JoinColumn(name = FieldConstants.ROLE_ID)
    private Role role;

    @Column(name = FieldConstants.NAME)
    private String name;

    public Designation(Long id) {
        super(id);
    }

    public Designation() {
        super();
    }

}
