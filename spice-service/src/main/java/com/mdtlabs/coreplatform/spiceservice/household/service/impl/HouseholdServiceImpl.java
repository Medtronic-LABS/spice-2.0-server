package com.mdtlabs.coreplatform.spiceservice.household.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberSequenceDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdSequenceDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.HouseholdMemberLinkService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.household.service.HouseholdService;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * This class is a service class to perform operation on Household
 * operations.
 * </p>
 *
 * @author Nandhakumar karthikeyan created on Jan 04, 2024
 */
@Service
public class HouseholdServiceImpl implements HouseholdService {

    private final FhirServiceApiInterface fhirServiceApiInterface;

    private final HouseholdMemberLinkService householdMemberLinkService;

    public HouseholdServiceImpl(FhirServiceApiInterface fhirServiceApiInterface, HouseholdMemberLinkService householdMemberLinkService) {
        this.fhirServiceApiInterface = fhirServiceApiInterface;
        this.householdMemberLinkService = householdMemberLinkService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdDTO createHousehold(HouseholdDTO householdDTO) {
        return fhirServiceApiInterface.createHouseHold(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdDTO updateHousehold(HouseholdDTO householdDTO) {
        return fhirServiceApiInterface.updateHouseHold(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdDTO getHousehold(String householdId) {
        return fhirServiceApiInterface.getHouseHold(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdMemberDTO createHouseholdMember(HouseholdMemberDTO householdMemberDTO) {
        HouseholdMemberDTO householdMemberDTOResponse = fhirServiceApiInterface.createHouseHoldMember(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdMemberDTO);
        if (Objects.isNull(householdMemberDTO.getHouseholdId())) {
            householdMemberLinkService.createHouseholdMemberLink(householdMemberDTOResponse);
        }
        return householdMemberDTOResponse;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HouseholdMemberDTO updateHouseholdMember(HouseholdMemberDTO householdMemberDTO) {
        return fhirServiceApiInterface.updateHouseHoldMember(CommonUtil.getAuthToken(), CommonUtil.getClient(), householdMemberDTO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<HouseholdDTO> getHouseholdList(RequestDTO request) {
        List<HouseholdDTO> householdList = new ArrayList<>();
        List<HouseholdDTO> householdResponseList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            request.setSkip(skip);
            request.setLimit(limit);
            householdResponseList = fhirServiceApiInterface.getHouseholdList(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
            householdList.addAll(householdResponseList);
            skip += limit;
        } while (!householdResponseList.isEmpty());
        return householdList;
    }

    /**
     * {@inheritDoc}
     */
    public List<HouseholdMemberDTO> getHouseholdMemberList(RequestDTO request) {
        List<HouseholdMemberDTO> householdMemberList = new ArrayList<>();
        List<HouseholdMemberDTO> householdMemberResponseList;
        int skip = Constants.OFFLINE_RESOURCE_LIST_SKIP;
        int limit = Constants.OFFLINE_RESOURCE_LIST_LIMIT;
        do {
            request.setSkip(skip);
            request.setLimit(limit);
            householdMemberResponseList = fhirServiceApiInterface.getHouseholdMemberList(CommonUtil.getAuthToken(), CommonUtil.getClient(), request);
            householdMemberList.addAll(householdMemberResponseList);
            skip += limit;
        } while (!householdMemberResponseList.isEmpty());
        return householdMemberList;
    }

    /**
     * {@inheritDoc}
     */
    public HouseholdMemberDTO updateSignature(RequestDTO request) {
        return fhirServiceApiInterface.updateSignature(CommonUtil.getAuthToken(),
                CommonUtil.getClient(), request);
    }

}
