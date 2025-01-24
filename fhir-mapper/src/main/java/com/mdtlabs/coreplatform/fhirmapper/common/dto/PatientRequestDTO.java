package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for PatientRequest entity.
 * </p>
 *
 * @author Nanthinee sugumar Created on Feb 28, 2024.
 */
@Data
public class PatientRequestDTO {

    private String name;

    private String phoneNumber;

    private String id;

    private String siteId;

    private String status;

    private String villageId;

    private int page;

    private int count;

    private List<String> villageIds;

    private int skip;

    private int limit;

    private String idSystem;

    private String idCode;

    private String searchText;

    private String referencePatientId;

    private String type;

    private PatientFilterDTO filter;

    private PatientSortDTO sort;

    private String countryId;

    private String memberReference;

    private Boolean isDiagnosisRequired;

    private boolean isReferredReasonsRequired;

    private String searchType;
}
