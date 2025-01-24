package com.mdtlabs.coreplatform.fhirmapper.user.controller;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.HealthFacilityRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.TestConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.user.service.UserService;

/**
 * <p>
 * UserControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in UserController class.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Mar 11, 2024
 */

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserControllerTest {

    @Mock
    UserService userService;

    @InjectMocks
    UserController userController;

    @Test
    void createUser() {
        UserRequestDTO userRequestDTO = TestDataProvider.getUserRequestDTO();
        when(userService.createUser(userRequestDTO)).thenReturn(userRequestDTO);
        UserRequestDTO response = userController.createUser(userRequestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response.getUsername(), userRequestDTO.getUsername());
    }

    @Test
    void createOrganization() {
        HealthFacilityRequestDTO healthFacilityRequestDTO = TestDataProvider.getHealthFacilityRequestDTO();
        when(userService.createOrganization(healthFacilityRequestDTO)).thenReturn(healthFacilityRequestDTO);
        HealthFacilityRequestDTO response = userController.createOrganization(healthFacilityRequestDTO);
        Assertions.assertNotNull(response);
        Assertions.assertEquals(response.getName(), healthFacilityRequestDTO.getName());
    }

    @Test
    void updateUser() {
        //given
        UserRequestDTO userRequestDTO = TestDataProvider.getUserRequestDTO();

        //when
        when(userService.updateUser(userRequestDTO)).thenReturn(userRequestDTO);

        //then
        UserRequestDTO response = userController.updateUser(userRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void updateOrganization() {
        //given
        HealthFacilityRequestDTO healthFacilityRequestDTO = TestDataProvider.getHealthFacilityRequestDTO();

        //when
        when(userService.updateOrganization(healthFacilityRequestDTO)).thenReturn(healthFacilityRequestDTO);

        //then
        HealthFacilityRequestDTO response = userController.updateOrganization(healthFacilityRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void deleteUser() {
        //when
        doNothing().when(userService).deleteUser(TestConstants.UNIQUE_ID);

        //then
        userController.deleteUser(TestConstants.UNIQUE_ID);
        verify(userService).deleteUser(TestConstants.UNIQUE_ID);
    }

    @Test
    void deleteOrganization() {
        //when
        doNothing().when(userService).deleteOrganization(TestConstants.UNIQUE_ID);

        //then
        userController.deleteOrganization(TestConstants.UNIQUE_ID);
        verify(userService).deleteOrganization(TestConstants.UNIQUE_ID);
    }

    @Test
    void deleteUsers() {
        //when
        doNothing().when(userService).deleteUsers(List.of(TestConstants.UNIQUE_ID));

        //then
        userController.deleteUsers(List.of(TestConstants.UNIQUE_ID));
        verify(userService).deleteUsers(List.of(TestConstants.UNIQUE_ID));
    }

    @Test
    void activateDeactivateUsers() {
        //given
        List<String> userIds = new ArrayList<>();
        Boolean isActivate = false;

        //when
        doNothing().when(userService).activateDeactivateUsers(userIds, isActivate);

        //then
        userController.activateDeactivateUsers(userIds, isActivate);
        verify(userService).activateDeactivateUsers(userIds, isActivate);
    }

    @Test
    void activateDeactivateOrganization() {
        //given
        List<String> userIds = new ArrayList<>();
        Boolean isActivate = false;

        //when
        doNothing().when(userService).activateDeactivateOrganizations(userIds, isActivate);

        //then
        userController.activateDeactivateOrganization(userIds, isActivate);
        verify(userService).activateDeactivateOrganizations(userIds, isActivate);
    }
}