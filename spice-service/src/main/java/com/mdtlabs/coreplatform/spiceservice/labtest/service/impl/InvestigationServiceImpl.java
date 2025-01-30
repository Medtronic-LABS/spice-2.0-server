package com.mdtlabs.coreplatform.spiceservice.labtest.service.impl;

import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.AdminServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestCustomizationDTO;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestHistoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.LabTestRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.labtest.service.InvestigationService;

@Service
public class InvestigationServiceImpl implements InvestigationService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final AdminServiceApiInterface adminServiceApiInterface;

    public InvestigationServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, AdminServiceApiInterface adminServiceApiInterface) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.adminServiceApiInterface = adminServiceApiInterface;
    }

    /**
     *
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> createOrUpdateInvestigation(LabTestRequestDTO requestDTO) {
        return fhirServiceApiInterface.createOrUpdateInvestigation(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), CommonUtil.getTenantId(), requestDTO);
    }

    /**
     *
     * {@inheritDoc}
     */
    @Override
    public LabTestDTO getInvestigatedDetails(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getInvestigatedDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     *
     * {@inheritDoc}
     */
    @Override
    public List<LabTestDTO> getInvestigationsByEncounter(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getInvestigationsByEncounter(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     *
     * {@inheritDoc}
     */
    @Override
    public Map<String,String> removeInvestigation(RequestDTO requestDTO) {
        return fhirServiceApiInterface.removeInvestigation(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO);
    }

    /**
     *
     * {@inheritDoc}
     */
    @Override
    public LabTestHistoryDTO getHistoryInvestigatedDetails(RequestDTO requestDTO) {
        return fhirServiceApiInterface.getHistoryInvestigatedDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO, CommonUtil.getTenantId());
    }

    /**
     *
     * {@inheritDoc}
     */
    public LabTestCustomizationDTO getLabTestCustomization(SearchRequestDTO requestDTO) {
        return adminServiceApiInterface.getLabTestCustomization(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), requestDTO);
    }

    /**
     *
     * {@inheritDoc}
     */
    public void updateInvestigationResult(LabTestRequestDTO labTestRequestDTO) {
        fhirServiceApiInterface.updateInvestigationResult(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), labTestRequestDTO);
    }

    /**
     *
     * {@inheritDoc}
     */
    public void reviewInvestigation(RequestDTO request) {
        fhirServiceApiInterface.reviewInvestigation(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request);
    }

    /**
     *
     * {@inheritDoc}
     */
    public Map<String, List<LabTestDTO>> getIntensificationDetails(RequestDTO request) {
        return fhirServiceApiInterface.getIntensificationDetails(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request);
    }
}
