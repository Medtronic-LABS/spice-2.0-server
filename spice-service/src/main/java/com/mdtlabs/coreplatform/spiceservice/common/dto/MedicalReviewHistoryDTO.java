package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.Date;
import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * <p>
 * DTO class for Medical Review History Details.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since May 09, 2024
 */

@Data
public class MedicalReviewHistoryDTO {

    private String id;

    private String patientId;

    private String patientReference;

    private String patientStatus;

    private Date dateOfReview;

    private Object reviewDetails;

    private List<Map<String, Object>> history;

    private Date nextVisitDate;

    private String type;

    private String visitNumber;

}
