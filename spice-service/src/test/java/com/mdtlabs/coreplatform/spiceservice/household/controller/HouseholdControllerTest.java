package com.mdtlabs.coreplatform.spiceservice.household.controller;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.HouseholdMemberDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.spiceservice.household.service.HouseholdService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * <p>
 * HouseholdControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in HouseholdController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class HouseholdControllerTest {

    @InjectMocks
    HouseholdController householdController;

    @Mock
    HouseholdService householdService;

    @Test
    void createHousehold() {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        ResponseEntity<HouseholdDTO> responseEntity = new ResponseEntity<>(householdDTO, HttpStatus.OK);

        //when
        when(householdService.createHousehold(householdDTO)).thenReturn(responseEntity.getBody());

        //then
        ResponseEntity<HouseholdDTO> response = householdController.createHousehold(householdDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseHold() {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();

        //when
        when(householdService.getHousehold(TestConstants.STRING_ONE)).thenReturn(householdDTO);

        //then
        SuccessResponse<HouseholdDTO> response = householdController.getHouseHold(TestConstants.STRING_ONE);
        Assertions.assertNotNull(response);
    }

    @Test
    void createHouseholdMember() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        ResponseEntity<HouseholdMemberDTO> responseEntity = new ResponseEntity<>(householdMemberDTO, HttpStatus.OK);

        //when
        when(householdService.createHouseholdMember(householdMemberDTO)).thenReturn(responseEntity.getBody());

        //then
        ResponseEntity<HouseholdMemberDTO> response = householdController.createHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateHousehold() {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        ResponseEntity<HouseholdDTO> responseEntity = new ResponseEntity<>(householdDTO, HttpStatus.OK);

        //when
        when(householdService.updateHousehold(householdDTO)).thenReturn(responseEntity.getBody());

        //then
        ResponseEntity<HouseholdDTO> response = householdController.updateHousehold(householdDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateHouseholdMember() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        ResponseEntity<HouseholdMemberDTO> responseEntity = new ResponseEntity<>(householdMemberDTO, HttpStatus.OK);

        //when
        when(householdService.updateHouseholdMember(householdMemberDTO)).thenReturn(responseEntity.getBody());

        //then
        ResponseEntity<HouseholdMemberDTO> response = householdController.updateHouseholdMember(householdMemberDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdList() {
        //given
        HouseholdDTO householdDTO = TestDataProvider.getHouseholdData();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setSkip(TestConstants.INT_ZERO);
        requestDTO.setLimit(TestConstants.INT_ONE);

        //when
        when(householdService.getHouseholdList(requestDTO)).thenReturn(List.of(householdDTO));

        //then
        List<HouseholdDTO> response = householdController.getHouseholdList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getHouseholdMemberList() {
        //given
        HouseholdMemberDTO householdMemberDTO = TestDataProvider.getHouseHoldMember();
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setSkip(TestConstants.INT_ZERO);
        requestDTO.setLimit(TestConstants.INT_ONE);

        //when
        when(householdService.getHouseholdMemberList(requestDTO)).thenReturn(List.of(householdMemberDTO));

        //then
        List<HouseholdMemberDTO> response = householdController.getHouseholdMemberList(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void testUpdateSignature() {
        RequestDTO requestDTO = new RequestDTO();
        HouseholdMemberDTO householdMemberDTO = new HouseholdMemberDTO();
        when(householdService.updateSignature(requestDTO)).thenReturn(householdMemberDTO);
        HouseholdMemberDTO response = householdController.updateSignature(requestDTO);
        Assertions.assertEquals(householdMemberDTO, response);
    }
}
