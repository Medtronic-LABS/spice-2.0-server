package com.mdtlabs.coreplatform.spiceservice.householdmemberlink.controller;

import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberLinkDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.impl.HouseholdMemberLinkServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HouseHoldMemberLinkControllerTest {
    @InjectMocks
    private HouseholdMemberLinkController householdMemberLinkController;

    @Mock
    private HouseholdMemberLinkServiceImpl householdMemberlinkService;

    @Test
    void createHouseholdMemberLink() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        doNothing().when(householdMemberlinkService).createHouseholdMemberLink(householdMemberDTO);
        householdMemberLinkController.createHouseholdMemberLink(householdMemberDTO);
        verify(householdMemberlinkService,atLeastOnce()).createHouseholdMemberLink(householdMemberDTO);
    }

    @Test
    void getHouseholdMemberLinkList() {
        RequestDTO request = TestDataProvider.getRequestDTO();
        HouseholdMemberLinkDTO householdMemberDTO = new HouseholdMemberLinkDTO();
        List<HouseholdMemberLinkDTO> householdMemberLinkDTOS = List.of(householdMemberDTO);

        //when
        when(householdMemberlinkService.getHouseholdMemberLinkList(request)).thenReturn(householdMemberLinkDTOS);
        List<HouseholdMemberLinkDTO> householdMemberLinkList = householdMemberLinkController.getHouseholdMemberLinkList(request);
        verify(householdMemberlinkService, times(1)).getHouseholdMemberLinkList(request);

        assertEquals(1, householdMemberLinkList.size());
        assertSame(householdMemberDTO, householdMemberLinkList.get(0));
    }

    @Test
    void updateHouseholdMemberLink() {
        HouseholdMemberLinkDTO householdMemberLinkDTO = new HouseholdMemberLinkDTO();

        //when
        doNothing().when(householdMemberlinkService).updateHouseholdMemberLink(householdMemberLinkDTO);
        householdMemberLinkController.updateHouseholdMemberLink(householdMemberLinkDTO);
        verify(householdMemberlinkService, times(1)).updateHouseholdMemberLink(householdMemberLinkDTO);
    }

    @Test
    void updateMemberLink() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        List<HouseholdMemberDTO> householdMemberDto = List.of(householdMemberDTO);

        //when
        doNothing().when(householdMemberlinkService).updateMemberLink(householdMemberDto);
        householdMemberLinkController.updateMemberLink(householdMemberDto);
        verify(householdMemberlinkService, times(1)).updateMemberLink(householdMemberDto);
    }
}
