package com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.patienttreatmentplan.service.PatientTreatmentPlanService;

/**
 * <p>
 * This class is a service implementation class to perform operation on Patient treatmentplan.
 * </p>
 *
 * @author Karthick M created on Aug 14, 2024
 */
@Service
public class PatientTreatmentPlanServiceImpl implements PatientTreatmentPlanService {


    private final FhirServiceApiInterface fhirServiceApiInterface;

    @Autowired
    public PatientTreatmentPlanServiceImpl(FhirServiceApiInterface fhirServiceApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public TreatmentPlanResponseDTO getPatientTreatmentPlanDetails(RequestDTO request) {
        return fhirServiceApiInterface.getPatientTreatmentPlan(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    public void updateTreatmentPlanData(TreatmentPlanDTO treatmentPlan) {
        fhirServiceApiInterface.updateTreatmentPlanData(CommonUtil.getAuthToken(), CommonUtil.getClient(), treatmentPlan);

    }
}
