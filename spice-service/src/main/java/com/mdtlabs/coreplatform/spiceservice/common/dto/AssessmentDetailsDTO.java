package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class for AssessmentDTO Details entity.
 * </p>
 *
 * @author Nandhakumar karthikeyan Created on MAr 08, 2024.
 */
@Data
public class AssessmentDetailsDTO {

    private ICCMDTO iccm;

    private OtherSymptomsDTO otherSymptoms;

    private TbDTO tb;

    private AncDTO anc;

    private PncMotherDTO pncMother;

    private PncNeonatalDTO pncNeonatal;

    private PncChildDTO pncChild;

}
