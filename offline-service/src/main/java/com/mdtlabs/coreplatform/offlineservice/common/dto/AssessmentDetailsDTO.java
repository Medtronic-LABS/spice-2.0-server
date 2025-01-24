package com.mdtlabs.coreplatform.offlineservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for AssessmentDTO Details entity.
 * </p>
 *
 * @author Praveen created on Mar 26, 2024
 */
@Data
public class AssessmentDetailsDTO {

    private ICCMDTO iccm;

    private OtherSymptomsDTO otherSymptoms;

    private AncDTO anc;

    private PncMotherDTO pncMother;

    private PncNeonatalDTO pncNeonatal;

    private PncChildDTO pncChild;

}
