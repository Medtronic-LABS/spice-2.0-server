package com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.impl;

import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDetailDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegisterDetail;
import com.mdtlabs.coreplatform.spiceservice.common.model.HouseholdMemberLink;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterDetailRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.respository.HouseholdMemberLinkRepository;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.HouseholdMemberLinkService;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
public class HouseholdMemberLinkServiceImpl implements HouseholdMemberLinkService {

    private final HouseholdMemberLinkRepository householdMemberLinkRepository;

    private final CallRegisterRepository callRegisterRepository;

    private final CallRegisterDetailRepository callRegisterDetailRepository;

    @Autowired
    public HouseholdMemberLinkServiceImpl(HouseholdMemberLinkRepository householdMemberLinkRepository, CallRegisterRepository callRegisterRepository, CallRegisterDetailRepository callRegisterDetailRepository) {
        this.householdMemberLinkRepository = householdMemberLinkRepository;
        this.callRegisterRepository = callRegisterRepository;
        this.callRegisterDetailRepository = callRegisterDetailRepository;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void createHouseholdMemberLink(HouseholdMemberDTO householdMemberDTO) {
        HouseholdMemberLink householdMemberLink = new HouseholdMemberLink();
        householdMemberLink.setMemberId(householdMemberDTO.getId());
        householdMemberLink.setPatientId(householdMemberDTO.getPatientId());
        householdMemberLink.setVillageId(householdMemberDTO.getVillageId());
        householdMemberLink.setStatus(Constants.UNASSIGNED);
        householdMemberLink.setName(householdMemberDTO.getName());
        householdMemberLinkRepository.save(householdMemberLink);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<HouseholdMemberLinkDTO> getHouseholdMemberLinkList(RequestDTO requestDTO) {
        ModelMapper mapper = new ModelMapper();
        List<String> villageIds = requestDTO.getVillageIds();
        List<HouseholdMemberLink> householdMemberLinks = householdMemberLinkRepository.findByVillageIds(villageIds, Constants.UNASSIGNED, requestDTO.getLastSyncTime(),
                requestDTO.getCurrentSyncTime());
        return householdMemberLinks != null ? householdMemberLinks.stream().map(householdMemberLink -> mapper.map(householdMemberLink, HouseholdMemberLinkDTO.class)).toList()
                : new ArrayList<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateHouseholdMemberLink(HouseholdMemberLinkDTO householdMemberLinkDTO) {
        ModelMapper mapper = new ModelMapper();
        if (Objects.nonNull(householdMemberLinkDTO.getCallRegisterDetail())) {
            CallRegister existingCallRegister = callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalse(householdMemberLinkDTO.getMemberId(), AppointmentType.HH_MAPPING);
            CallRegister callRegister = Objects.nonNull(existingCallRegister) ? existingCallRegister : mapper.map(householdMemberLinkDTO, CallRegister.class);
            int existingAttempt = callRegister.getAttempts();
            callRegister.setAttempts(existingAttempt + householdMemberLinkDTO.getCallRegisterDetail().size());
            callRegister.setCreatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
            callRegister.setUpdatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
            callRegister.setType(AppointmentType.HH_MAPPING);
            callRegister = callRegisterRepository.save(callRegister);
            for (CallRegisterDetailDTO householdMemberLinkDetail : householdMemberLinkDTO.getCallRegisterDetail()) {
                CallRegisterDetail callRegisterDetail = new CallRegisterDetail();
                callRegisterDetail.setCallRegisterId(callRegister.getId());
                callRegisterDetail.setCallDate(householdMemberLinkDetail.getCallDate());
                callRegisterDetail.setAttempts(++existingAttempt);
                callRegisterDetail.setCreatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
                callRegisterDetail.setUpdatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
                callRegisterDetail.setDuration(householdMemberLinkDetail.getDuration());
                callRegisterDetailRepository.save(callRegisterDetail);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateMemberLink(List<HouseholdMemberDTO> householdMemberDto) {
        List<String> memberIds = householdMemberDto.stream()
                .map(HouseholdMemberDTO::getId)
                .toList();
        List<HouseholdMemberLink> householdMemberLinks = householdMemberLinkRepository.findByMemberIdInAndIsDeletedFalseAndIsActiveTrue(memberIds);
        List<HouseholdMemberLink> householdMemberLinkResponse = new ArrayList<>();
        householdMemberDto.forEach(member -> {
            HouseholdMemberLink houseMember = householdMemberLinks.stream().filter(m -> m.getMemberId().equals(member.getId())).findFirst().orElse(null);
            if (Objects.nonNull(houseMember)) {
                houseMember.setStatus(Constants.ASSIGNED);
                houseMember.setHouseholdId(member.getHouseholdId());
                householdMemberLinkResponse.add(houseMember);
            }
        });
        householdMemberLinkRepository.saveAll(householdMemberLinkResponse);
    }
}
