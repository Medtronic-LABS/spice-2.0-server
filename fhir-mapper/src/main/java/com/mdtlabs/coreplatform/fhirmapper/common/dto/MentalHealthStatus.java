package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.List;

import lombok.Data;

/**
 * <p>
 *      This class used to get mental health status.
 * </p>
 *
 * @author - Gopinath
 */
@Data
public class MentalHealthStatus {

    private String id;

    private String status;

    private List<String> mentalHealthDisorder;

    private String comments;

    private String yearOfDiagnosis;
}
