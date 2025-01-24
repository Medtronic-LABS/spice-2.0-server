package com.mdtlabs.coreplatform.fhirmapper.household.controller;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.household.service.HouseholdService;

/**
 * <p>
 * HouseholdControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in HouseholdController class.
 * </p>
 *
 * @author Nandhakumar
 * @since Feb 8, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HouseholdControllerTest {

    @InjectMocks
    HouseholdController householdController;

    @Mock
    private HouseholdService householdService;

    /**
     * Creates new household  Unit Test case
     */
    @Test
    void createHousehold() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        when(householdService.createHousehold(householdDTO)).thenReturn(householdDTO);
        HouseholdDTO response = householdController.createHousehold(householdDTO);
        Assertions.assertNotNull(response);
    }

    /**
     * get household Unit Test case
     */
    @Test
    void getHouseHold() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        when(householdService.getHousehold(null)).thenReturn(householdDTO);
        HouseholdDTO response = householdController.getHouseHold(null);
        Assertions.assertNotNull(response);
    }

    /**
     * Creates new household Member Unit Test case
     */
    @Test
    void createHouseholdMember() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        when(householdService.createHouseholdMember(householdMemberDTO)).thenReturn(householdMemberDTO);
        HouseholdMemberDTO response = householdController.createHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
    }

    /**
     * Update new household Unit Test case
     */
    @Test
    void updateHousehold() {
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        when(householdService.updateHousehold(householdDTO)).thenReturn(householdDTO);
        HouseholdDTO response = householdController.updateHousehold(householdDTO);
        Assertions.assertNotNull(response);
    }

    /**
     * update new household Member Unit Test case
     */
    @Test
    void updateHouseholdMember() {
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        when(householdService.updateHouseholdMember(householdMemberDTO)).thenReturn(householdMemberDTO);
        HouseholdMemberDTO response = householdController.updateHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
    }

    /**
     * update get household Unit Test case
     */
    @Test
    void getHouseholdList() {
        RequestDTO requestDTO = new RequestDTO();
        when(householdService.getHouseholdList(requestDTO)).thenReturn(List.of());
        List<HouseholdDTO> response = householdController.getHouseholdList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdMemberList() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        List<HouseholdMemberDTO> householdMemberDTOS = List.of(TestDataProvider.getHouseHoldMember());

        //when
        when(householdService.getHouseholdMemberList(requestDTO)).thenReturn(householdMemberDTOS);

        //then
        List<HouseholdMemberDTO>  response = householdController.getHouseholdMemberList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdMemberById() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setMemberId(TestConstants.TWO_STR);
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        when(householdService.getHouseholdMemberById(requestDTO.getMemberId())).thenReturn(householdMemberDTO);

        //then
        HouseholdMemberDTO response = householdController.getHouseholdMemberById(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdMemberByPatientId() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setPatientId(TestConstants.TWO_STR);
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        when(householdService.getHouseholdMemberByPatientId(requestDTO.getPatientId())).thenReturn(householdMemberDTO);

        //then
        HouseholdMemberDTO response = householdController.getHouseholdMemberByPatientId(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateSignature() {
        //given
        RequestDTO requestDTO = TestDataProvider.getRequestDTO();
        requestDTO.setMemberId(TestConstants.TWO_STR);
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();

        //when
        when(householdService.updateSignature(requestDTO)).thenReturn(householdMemberDTO);

        //then
        HouseholdMemberDTO response = householdController.updateSignature(requestDTO);
        Assertions.assertNotNull(response);

    }

}
