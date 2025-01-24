package com.mdtlabs.coreplatform.spiceservice.patientvisit.service.impl;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PatientVisitDTO;
import com.mdtlabs.coreplatform.spiceservice.patientvisit.service.PatientVisitService;


/**
 * <p>
 * This class implements the patient visit service interface and contains actual
 * business logic to perform operations on patient visit entity.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 09, 2023
 */
@Service
public class PatientVisitServiceImpl implements PatientVisitService {


    private FhirServiceApiInterface fhirServiceApiInterface;

    @Autowired
    public PatientVisitServiceImpl(FhirServiceApiInterface fhirServiceApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> createPatientVisit(PatientVisitDTO patientVisitDto) {
        return fhirServiceApiInterface.createPatientVisit(CommonUtil.getAuthToken(), CommonUtil.getClient(), patientVisitDto);
    }
}
