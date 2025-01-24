package com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service;

import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.CallRegisterDetailDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;
import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegisterDetail;
import com.mdtlabs.coreplatform.spiceservice.common.model.HouseholdMemberLink;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterDetailRepository;
import com.mdtlabs.coreplatform.spiceservice.followup.repository.CallRegisterRepository;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.respository.HouseholdMemberLinkRepository;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.impl.HouseholdMemberLinkServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.modelmapper.ModelMapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class HouseHoldMemberLinkServiceTest {

    @InjectMocks
    private HouseholdMemberLinkServiceImpl householdMemberLinkService;

    @Mock
    private HouseholdMemberLinkRepository householdMemberLinkRepository;

    @Mock
    private CallRegisterRepository callRegisterRepository;

    @Mock
    private CallRegisterDetailRepository callRegisterDetailRepository;

    @Test
    void createHouseholdMemberLink() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        HouseholdMemberLink householdMemberLink = new HouseholdMemberLink();
        householdMemberLink.setMemberId(householdMemberDTO.getId());
        householdMemberLink.setPatientId(householdMemberDTO.getPatientId());
        householdMemberLink.setVillageId(householdMemberDTO.getVillageId());
        householdMemberLink.setStatus(Constants.UNASSIGNED);
        householdMemberLink.setName(householdMemberDTO.getName());
        when(householdMemberLinkRepository.save(householdMemberLink)).thenReturn(householdMemberLink);
        householdMemberLinkService.createHouseholdMemberLink(householdMemberDTO);
    }

    @Test
    void getHouseholdMemberLinkList() {
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<String> villageIds = requestDTO.getVillageIds();
        List<HouseholdMemberLink> householdMemberLinks = new ArrayList<>();
        when(householdMemberLinkRepository.findByVillageIds(villageIds, Constants.UNASSIGNED, requestDTO.getLastSyncTime(),
                requestDTO.getCurrentSyncTime())).thenReturn(householdMemberLinks);
        householdMemberLinkService.getHouseholdMemberLinkList(requestDTO);
    }

    @Test
    void updateHouseholdMemberLink() {
        HouseholdMemberLinkDTO householdMemberLinkDTO = new HouseholdMemberLinkDTO();
        householdMemberLinkDTO.setProvenance(TestDataProvider.getProvenance());
        householdMemberLinkDTO.getProvenance().setSpiceUserId(1L);
        CallRegister existingCallRegister = TestDataProvider.getCallRegister();
        CallRegisterDetailDTO callRegisterDetailDTO = new CallRegisterDetailDTO();
        householdMemberLinkDTO.setCallRegisterDetail(List.of(callRegisterDetailDTO));
        CallRegister callRegister = TestDataProvider.getCallRegister();
        callRegister.setId(1L);
        callRegister.setAttempts(TestDataProvider.getCallRegister().getAttempts());
        int existingAttempt = callRegister.getAttempts();
        callRegisterDetailDTO.setCallDate(new Date());
        callRegisterDetailDTO.setDuration(TestConstants.INT_ONE);
        callRegister.setAttempts(existingAttempt + householdMemberLinkDTO.getCallRegisterDetail().size());
        callRegister.setCreatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
        callRegister.setUpdatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
        callRegister.setType(AppointmentType.HH_MAPPING);
        CallRegisterDetail callRegisterDetail = TestDataProvider.getCallRegisterDetail();
        callRegisterDetail.setCallRegisterId(callRegister.getId());
        callRegisterDetail.setCallDate(callRegisterDetailDTO.getCallDate());
        callRegisterDetail.setAttempts(++existingAttempt);
        callRegisterDetail.setCreatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
        callRegisterDetail.setUpdatedBy(householdMemberLinkDTO.getProvenance().getSpiceUserId());
        callRegisterDetail.setDuration(callRegisterDetailDTO.getDuration());
        //when
        when(callRegisterRepository.findByMemberIdAndTypeAndIsDeletedFalse(householdMemberLinkDTO.getMemberId(), AppointmentType.HH_MAPPING)).thenReturn(existingCallRegister);
        when(callRegisterRepository.save(callRegister)).thenReturn(existingCallRegister);
        when(callRegisterDetailRepository.save(callRegisterDetail)).thenReturn(callRegisterDetail);
        householdMemberLinkService.updateHouseholdMemberLink(householdMemberLinkDTO);
    }

    @Test
    void updateMemberLink() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        List<HouseholdMemberDTO> householdMemberDto = List.of(householdMemberDTO);
        List<HouseholdMemberLink> householdMemberLinks = new ArrayList<>();
        List<String> memberIds = householdMemberDto.stream()
                .map(HouseholdMemberDTO::getId)
                .toList();
        HouseholdMemberLink houseMember = new HouseholdMemberLink();
        houseMember.setStatus(Constants.ASSIGNED);
        houseMember.setHouseholdId(TestDataProvider.getHouseHoldMember().getId());
        List<HouseholdMemberLink> householdMemberLinkResponse = new ArrayList<>();
        householdMemberLinkResponse.add(houseMember);
        //when
        when(householdMemberLinkRepository.findByMemberIdInAndIsDeletedFalseAndIsActiveTrue(memberIds)).thenReturn(householdMemberLinks);
        when(householdMemberLinkRepository.saveAll(householdMemberLinkResponse)).thenReturn(householdMemberLinks);
        householdMemberLinkService.updateMemberLink(householdMemberDto);
    }
}
