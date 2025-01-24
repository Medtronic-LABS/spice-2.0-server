package com.mdtlabs.coreplatform.spiceservice.household.service;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.household.service.impl.HouseholdServiceImpl;
import com.mdtlabs.coreplatform.spiceservice.householdmemberlink.service.HouseholdMemberLinkService;

/**
 * <p>
 * HouseholdServiceTest class used to test all possible positive
 * and negative cases for all methods and conditions used in HouseholdService class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HouseholdServiceTest {
    @InjectMocks
    HouseholdServiceImpl householdService;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Mock
    private HouseholdMemberLinkService householdMemberLinkService;

    @Test
    void createHousehold() {
        //given
        TestDataProvider.init();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();

        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.createHouseHold("BearerTest", "mob", householdDTO)).thenReturn(householdDTO);
        //then
        HouseholdDTO response = householdService.createHousehold(householdDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateHouseHold() {
        //given
        TestDataProvider.init();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.updateHouseHold("BearerTest", "mob", householdDTO)).thenReturn(householdDTO);
        //then
        HouseholdDTO response = householdService.updateHousehold(householdDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getHouseHold() {
        //given
        TestDataProvider.init();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getHouseHold("BearerTest", "mob", TestConstants.STRING_ONE)).thenReturn(householdDTO);
        //then
        HouseholdDTO response = householdService.getHousehold(TestConstants.STRING_ONE);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void createHouseHoldMember() {
        //given
        TestDataProvider.init();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.createHouseHoldMember("BearerTest", "mob", householdMemberDTO)).thenReturn(householdMemberDTO);
        doNothing().when(householdMemberLinkService).createHouseholdMemberLink(householdMemberDTO);
        //then
        HouseholdMemberDTO response = householdService.createHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void updateHouseholdMember() {
        //given
        TestDataProvider.init();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.updateHouseHoldMember("BearerTest", "mob", householdMemberDTO)).thenReturn(householdMemberDTO);
        //then
        HouseholdMemberDTO response = householdService.updateHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getHouseholdList() {
        //given
        TestDataProvider.init();
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setSkip(TestConstants.INT_ZERO);
        requestDTO.setLimit(TestConstants.INT_ONE);

        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getHouseholdList("BearerTest", "mob", requestDTO)).thenReturn(List.of(householdDTO)).thenReturn(new ArrayList<>());

        //then
        List<HouseholdDTO> response = householdService.getHouseholdList(requestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }

    @Test
    void getHouseholdMemberList() {
        //given
        TestDataProvider.init();
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setSkip(TestConstants.INT_ZERO);
        requestDTO.setLimit(TestConstants.INT_ONE);

        //when
        TestDataProvider.getStaticMock();
        when(fhirServiceApiInterface.getHouseholdMemberList(CommonUtil.getAuthToken(), CommonUtil.getClient(), requestDTO)).thenReturn(List.of(householdMemberDTO)).thenReturn(new ArrayList<>());

        //then
        List<HouseholdMemberDTO> response = householdService.getHouseholdMemberList(requestDTO);
        Assertions.assertNotNull(response);
        TestDataProvider.cleanUp();
    }
}
