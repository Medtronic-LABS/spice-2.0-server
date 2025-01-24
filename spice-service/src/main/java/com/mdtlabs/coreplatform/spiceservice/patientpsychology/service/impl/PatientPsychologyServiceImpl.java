package com.mdtlabs.coreplatform.spiceservice.patientpsychology.service.impl;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.PsychologyDTO;
import com.mdtlabs.coreplatform.spiceservice.patientpsychology.service.PatientPsychologyService;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Service interface for managing psychology notes.
 * <p>
 * Defines operations for creating and retrieving patient psychology,
 * including specialized summaries for mother and neonate. This interface abstracts the
 * underlying data access and business logic for general medical review management.
 * </p>
 *
 * @author Bala Ashwanth N created on Nov 15, 2024
 */
@Service
public class PatientPsychologyServiceImpl implements PatientPsychologyService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    public PatientPsychologyServiceImpl(FhirServiceApiInterface fhirServiceApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PsychologyDTO removePsychologyDataById(PsychologyDTO request) {
        return fhirServiceApiInterface.removePsychologyData(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PsychologyDTO updatePsychologyData(PsychologyDTO request) {
        return fhirServiceApiInterface.updatePsychology(CommonUtil.getAuthToken(), CommonUtil.getClient(),request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PsychologyDTO savePsychologyData(PsychologyDTO request) {
        return fhirServiceApiInterface.savePatientPsychology(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<PsychologyDTO> getPsychologyDataByUserIdAndRelatedPersonId(PsychologyDTO request) {
        return fhirServiceApiInterface.getPatientPsychology(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
    }
}
