package com.mdtlabs.coreplatform.adminservice.Village.controller;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.adminservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.adminservice.controller.VillageController;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.VillageService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

/**
 * <p>
 * The VillageControllerTest class is a JUnit test class that tests the functionality of the
 * VillageController class.
 * </p>
 *
 * @author Divya S
 */
@ExtendWith(MockitoExtension.class)
class VillageControllerTest {

    @Mock
    private VillageService villageService;

    @InjectMocks
    private VillageController villageController;

    @Test
    void uploadFileTest() {
        //given
        MultipartFile multipartFile = mock(MultipartFile.class);
        List<String> appTypes = List.of("COMMUNITY");

        //then
        SuccessResponse<List<Village>> actualResponse = villageController.uploadRegionFile(multipartFile, appTypes);
        assertNotNull(actualResponse);
        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
    }

    @Test
    void downloadFileTest() {
        SearchRequestDTO request = new SearchRequestDTO();
        request.setCountryId(1L);
        HttpServletResponse response = new MockHttpServletResponse();
        byte[] bytes = new byte[5];

        //when
        when(villageService.downloadRegionFile(request.getCountryId(), request.getAppTypes())).thenReturn(bytes);

        //then
        byte[] actualResponse = villageController.downloadRegionFile(response, request);
        assertNotNull(actualResponse);
    }

    @Test
    void getMemberSequenceByVillageIdTest() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO(1l);
        //when
        when(villageService.getMemberSequenceByVillageId(requestDTO.getId())).thenReturn(10l);

        //then
        Long actualResponse = villageController.getMemberSequenceByVillageId(requestDTO);
        assertNotNull(actualResponse);
        assertEquals(10l, actualResponse);

    }

    @Test
    void getHouseholdSequenceByVillageIdTest() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO(1l);
        //when
        when(villageService.getHouseholdSequenceByVillageId(requestDTO.getId())).thenReturn(20l);

        //then
        Long actualResponse = villageController.getHouseholdSequenceByVillageId(requestDTO);
        assertNotNull(actualResponse);
        assertEquals(20l, actualResponse);
    }

    @Test
    void getVillageDetailsByVillageIdTest() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO(1l);
        VillageDTO villageDTO = TestDataProvider.getVillageDTO();

        //when
        when(villageService.getVillageDetailsByVillageId(requestDTO.getId())).thenReturn(villageDTO);

        //then
        VillageDTO actualResponse = villageController.getVillageDetails(requestDTO);
        assertNotNull(actualResponse);
        assertEquals(villageDTO, actualResponse);
    }
}