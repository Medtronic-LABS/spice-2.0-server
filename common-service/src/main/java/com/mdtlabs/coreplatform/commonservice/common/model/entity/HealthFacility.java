package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import lombok.Data;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

/**
 * <p>
 * The HealthFacility class represents a health facility entity with various attributes such as name,
 * type, code, address, and linked villages.
 * </p>
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_HEALTH_FACILITY)
public class HealthFacility extends TenantBaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.CODE)
    private String code;

    @Column(name = FieldConstants.PHU_FOCAL_PERSON_NAME)
    private String phuFocalPersonName;

    @Column(name = FieldConstants.PHU_FOCAL_PERSON_NUMBER)
    private String phuFocalPersonNumber;

    @Column(name = FieldConstants.ADDRESS)
    private String address;

    @ManyToOne
    @JoinColumn(name = FieldConstants.COUNTRY_ID)
    private Country country;

    @ManyToOne(cascade = CascadeType.DETACH)
    @JoinColumn(name = FieldConstants.DISTRICT_ID)
    private District district;

    @ManyToOne(cascade = CascadeType.DETACH)
    @JoinColumn(name = FieldConstants.CHIEFDOM_ID)
    private Chiefdom chiefdom;

    @Column(name = FieldConstants.CITY_NAME)
    private String cityName;

    @Column(name = FieldConstants.FHIR_ID)
    private String fhirId;

    @Column(name = FieldConstants.LATITUDE)
    private String latitude;

    @Column(name = FieldConstants.LONGITUDE)
    private String longitude;
    
    @Column(name = FieldConstants.POSTAL_CODE)
    private String postalCode;

    @Column(name = FieldConstants.LANGUAGE)
    private String language;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = FieldConstants.CULTURE_ID)
    private Culture culture;

    @ManyToMany 
    @JoinTable(name = TableConstants.MERGE_TABLE_HF_VILLAGE, joinColumns = {
        @JoinColumn(name= FieldConstants.HEALTH_FACILITY_ID)}, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.VILLAGE_ID)
        })
    private List<Village> linkedVillages;

    @OneToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = TableConstants.MERGE_TABLE_HF_WORKFLOW, joinColumns = {
            @JoinColumn(name = FieldConstants.HEALTH_FACILITY_ID) }, inverseJoinColumns = {
                    @JoinColumn(name = FieldConstants.CLINICAL_WORFKLOW_ID) })
    private List<ClinicalWorkflow> clinicalWorkflows;

    @OneToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = TableConstants.MERGE_TABLE_HF_CUSTOMIZED_WORKFLOW, joinColumns = {
            @JoinColumn(name = FieldConstants.HEALTH_FACILITY_ID) }, inverseJoinColumns = {
                    @JoinColumn(name = FieldConstants.CUSTOMIZED_WORFKLOW_ID) })
    private List<ClinicalWorkflow> customizedWorkflows;

    @Column(name = FieldConstants.ORG_UNIT_ID)
    private String orgUnitId;

    public HealthFacility(String name, String code, Country country, District district, Chiefdom chiefdom) {
        this.name = name;
        this.code = code;
        this.country = country;
        this.district = district;
        this.chiefdom = chiefdom;
    }

    public HealthFacility() {

    }

    public HealthFacility(Long id) {
        super(id);
    }
}
