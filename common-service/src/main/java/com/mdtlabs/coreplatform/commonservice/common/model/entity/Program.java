package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import lombok.Data;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import java.util.Set;

/**
 * This is entity class for program.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_PROGRAM)
public class Program extends TenantBaseEntity {

    private static final long serialVersionUID = 1L;

    // @NotEmpty(message = ErrorConstants.PROGRAM_NAME_NOT_EMPTY)
    @Column(name = FieldConstants.NAME)
    private String name;

    // @NotNull(message = ErrorConstants.COUNTRY_ID_NOT_NULL)
    @JoinColumn(name = FieldConstants.COUNTRY_ID)
    @ManyToOne
    private Country country;

    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_HEALTH_FACILITY_PROGRAM, joinColumns = {
        @JoinColumn(name = FieldConstants.PROGRAM_ID) }, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.HEALTH_FACILITY_ID) })
    private Set<HealthFacility> healthFacilities;
    
    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_DELETED_HEALTH_FACILITY_PROGRAM, joinColumns = {
        @JoinColumn(name = FieldConstants.PROGRAM_ID)}, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.HEALTH_FACILITY_ID)})
    private Set<HealthFacility> deletedHealthFacilities;
    
    /**
     * Default constructor.
     */
    public Program() {
        
    }
    
    /**
     * Parametrized constructor.
     */
    public Program(String name, Long tenantId, Country country) {
        this.name = name;
        this.tenantId = tenantId;
        this.country = country;
    }

}
