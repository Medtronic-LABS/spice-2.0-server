package com.mdtlabs.coreplatform.fhirmapper.screening.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.fhirmapper.common.TestDataProvider;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.BioDataDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetails;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.DashboardDetailsRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLog;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ScreeningLogRequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.screening.service.ScreeningService;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ScreeningControllerTest {

    @Mock
    ScreeningService screeningService;

    @InjectMocks
    ScreeningController screeningController;

    /**
     * Creates new Screening  Unit Test case
     */
    @Test
    void processScreeningLogTest() {
        ScreeningLogRequestDTO screeningLogRequestDTO = TestDataProvider.getScreeningRequest();
        BioDataDTO response = new BioDataDTO();
        when(screeningService.processScreeningLog(screeningLogRequestDTO)).thenReturn(response);
        BioDataDTO responseValue = screeningController.processScreeningLog(screeningLogRequestDTO);
        Assertions.assertNotNull(responseValue);
    }


    /**
     * Get screening log Unit Test case
     */
    @Test
    void getScreeningLog() {
        ScreeningLogRequestDTO screeningLogRequestDTO = TestDataProvider.getScreeningRequest();
        when(screeningService.getScreeningLog(screeningLogRequestDTO)).thenReturn(new ScreeningLog());
        ScreeningLog responseValue = screeningController.getScreeningLog(screeningLogRequestDTO);
        Assertions.assertNotNull(responseValue);
    }

    @Test
    void getPatientCountDetails() {
        //given
        DashboardDetailsRequestDTO dashboardDetailsRequestDTO = new DashboardDetailsRequestDTO();

        //when
        when(screeningService.getPatientCountOfUsers(dashboardDetailsRequestDTO)).thenReturn(new DashboardDetails());

        //then
        DashboardDetails response = screeningController.getPatientCountDetails(dashboardDetailsRequestDTO);
        Assertions.assertNotNull(response);
    }
}
