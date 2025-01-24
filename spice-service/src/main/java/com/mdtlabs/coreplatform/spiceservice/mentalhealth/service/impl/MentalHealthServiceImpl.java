package com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MentalHealthDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.MentalHealthService;

/**
 * <p>
 * This class implements the mental health service class and contains business logic for the operations of mental health
 * entity.
 * </p>
 *
 * @author Niraimathi S created on Sept 06 07, 2024
 */
@Service
public class MentalHealthServiceImpl implements MentalHealthService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    public MentalHealthServiceImpl(FhirServiceApiInterface fhirServiceApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public void createMentalHealth(AssessmentDTO mentalHealth) {
        fhirServiceApiInterface.createMentalHealth(CommonUtil.getAuthToken(), CommonUtil.getClient(), mentalHealth);
    }

    /**
     * {@inheritDoc}
     */
    public MentalHealthDTO getMentalHealthDetails(RequestDTO request) {
        return fhirServiceApiInterface.getMentalHealthDetails(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    public void createMentalHealthCondition(AssessmentDTO mentalHealth) {
        fhirServiceApiInterface.createMentalHealthCondition(CommonUtil.getAuthToken(), CommonUtil.getClient(), mentalHealth);
    }

    /**
     * {@inheritDoc}
     */
    public AssessmentDTO getMentalHealthCondition(AssessmentDTO request) {
        return fhirServiceApiInterface.getMentalHealthCondition(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }
}
