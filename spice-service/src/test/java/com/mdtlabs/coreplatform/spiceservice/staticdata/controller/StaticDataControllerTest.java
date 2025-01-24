package com.mdtlabs.coreplatform.spiceservice.staticdata.controller;

import java.util.ArrayList;
import java.util.List;

import com.mdtlabs.coreplatform.spiceservice.common.dto.*;
import com.mdtlabs.coreplatform.spiceservice.common.model.Frequency;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;

/**
 * <p>
 * StaticDataControllerTest class used to test all possible positive
 * and negative cases for all methods and conditions used in StaticDataController class.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on july 03 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class StaticDataControllerTest {

    @InjectMocks
    StaticDataController staticDataController;

    @Mock
    private StaticDataService staticDataService;

    @Test
    void getUserStatiCData() {
        //given
        StaticUserDataResponseDTO staticUserDataResponseDTO = TestDataProvider.getStaticUserDataResponseDTO();

        //when
        when(staticDataService.getUserStaticData()).thenReturn(staticUserDataResponseDTO);
        //then
        SuccessResponse<StaticUserDataResponseDTO>  response = staticDataController.getUserStatiCData();
        Assertions.assertNotNull(response);
    }

    @Test
    void getFormData() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setWorkflowIds(List.of(TestConstants.ONE));
        //when
        when(staticDataService.getWorkflowFormDataForStaticData(requestDTO)).thenReturn(TestDataProvider.getMetaDTO());

        //then
        SuccessResponse<List<FormMetaDTO>> response = staticDataController.getFormData(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMetaData() {
        //given
        SearchRequestDTO requestDTO = new SearchRequestDTO();
        requestDTO.setMetaNames(List.of(TestConstants.STRING_ONE));
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(requestDTO.getMetaNames(), null)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getMetaData(requestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getStaticMetaData() {
        //given
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();
        List<String> metaNames = List.of(Constants.SYSTEMIC_EXAMINATION, Constants.DIAGNOSIS, Constants.PRESENTING_COMPLAINTS,
                Constants.MEDICAL_SUPPLIES, Constants.COST, Constants.PATIENT_STATUS, Constants.DOSAGE_FREQUENCY);

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.ABOVE_FIVE_YEARS)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaData();
        Assertions.assertNotNull(response);
    }

    @Test
    void getStaticMetaDataForICCMUnderTwoMonths() {
        //given
        List<String> metaNames = List.of(Constants.EXAMINATION, Constants.DIAGNOSIS, Constants.PATIENT_STATUS,
                Constants.COUNSELLED_ON, Constants.DOSAGE_FREQUENCY);
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.UNDER_TWO_MONTHS)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaDataForICCMUnderTwoMonths();
        Assertions.assertNotNull(response);
    }
    @Test
    void getStaticMetaDataForICCMUnderFiveYears() {
        //given
        List<String> metaNames = List.of(Constants.EXAMINATION, Constants.SYSTEMIC_EXAMINATION,
                Constants.SYMPTOMS_BY_CATEGORY, Constants.DOSAGE_FREQUENCY, Constants.DIAGNOSIS);
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.UNDER_FIVE_YEARS)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaDataForICCMUnderFiveYears();
        Assertions.assertNotNull(response);
    }

    @Test
    void getStaticMetaDataForMotherNeonateANC() {
        //given
        List<String> metaNames = List.of(Constants.PRESENTING_COMPLAINTS, Constants.OBSTETRIC_EXAMINATION,
                Constants.PREGNANCY_HISTORY, Constants.PATIENT_STATUS, Constants.BLOOD_GROUP,
                Constants.DOSAGE_FREQUENCY, Constants.DIAGNOSIS);
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.ANC)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaDataForMotherNeonateANC();
        Assertions.assertNotNull(response);
    }

    @Test
    void getStaticMetaDataForMotherNeonatePNCMother() {
        //given
        List<String> metaNames = List.of(Constants.PRESENTING_COMPLAINTS, Constants.SYSTEMIC_EXAMINATION,
                Constants.DOSAGE_FREQUENCY, Constants.DIAGNOSIS);
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.PNC_MOTHER)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaDataForMotherNeonatePNCMother();
        Assertions.assertNotNull(response);
    }

    @Test
    void getStaticMetaDataForMotherNeonatePNCBaby() {
        //given
        List<String> metaNames = List.of(Constants.PRESENTING_COMPLAINTS, Constants.SYSTEMIC_EXAMINATION, Constants.DOSAGE_FREQUENCY);
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.PNC_CHILD_REVIEW)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaDataForMotherNeonatePNCBaby();
        Assertions.assertNotNull(response);
    }

    @Test
    void getStaticMetaDataForMotherDelivery() {
        //given
        List<String> metaNames = List.of(Constants.DELIVERY_AT, Constants.DELIVERY_BY, Constants.DELIVERY_TYPE,
                Constants.DELIVERY_STATUS, Constants.NEONATE_OUTCOME, Constants.RISK_FACTORS,
                Constants.CONDITION_OF_MOTHER, Constants.NEONATE_OUTCOME, Constants.MOTHER_DELIVERY_STATUS,
                Constants.SYMPTOMS_BY_CATEGORY, Constants.SYMPTOMS_BY_CATEGORY, Constants.DOSAGE_FREQUENCY);
        StaticMetaDataResponseDTO staticMetaDataResponseDTO = TestDataProvider.getStaticMetaDataResponseDTO();

        //when
        when(staticDataService.getNonNCDMetaData(metaNames, Constants.MOTHER_DELIVERY_REVIEW)).thenReturn(staticMetaDataResponseDTO);

        //then
        SuccessResponse<StaticMetaDataResponseDTO> response = staticDataController.getStaticMetaDataForMotherDelivery();
        Assertions.assertNotNull(response);
    }

    @Test
    void getMobileUsersAndFacilities() {
        //given
        StaticUserDataResponseDTO staticUserDataResponseDTO = TestDataProvider.getStaticUserDataResponseDTO();

        //when
        when(staticDataService.getMobileUsersAndFacilities()).thenReturn(staticUserDataResponseDTO);

        //then
        SuccessResponse<StaticUserDataResponseDTO> response = staticDataController.getMobileUsersAndFacilities();
        Assertions.assertNotNull(response);
    }

    @Test
    void checkAppVersion() {
        //when
        when(staticDataService.checkAppVersion(any())).thenReturn(Boolean.TRUE);
        //then
        SuccessResponse<Object> response = staticDataController.checkAppVersion(TestConstants.STRING_ONE);
        Assertions.assertNotNull(response);
    }

    @Test
    void checkAppVersion_ifCase() {

        //when
        when(staticDataService.checkAppVersion(any())).thenReturn(Boolean.FALSE);
        //then
        SuccessResponse<Object> response = staticDataController.checkAppVersion(TestConstants.STRING_ONE);
        Assertions.assertNotNull(response);
    }



    @Test
    void getMedicalReviewStaticData() {
        MedicalReviewStaticDataDTO medicalReviewStaticDataDTO = new MedicalReviewStaticDataDTO();
        when(staticDataService.getNCDMedicalReviewStaticData()).thenReturn(medicalReviewStaticDataDTO);

        SuccessResponse<MedicalReviewStaticDataDTO> response = staticDataController.getMedicalReviewStaticData();
        Assertions.assertNotNull(response);
    }

    @Test
    void getMetaFormData() {
        when(staticDataService.getMetaFormData(any())).thenReturn(null);
        SuccessResponse<MetaFormDTO> response = staticDataController.getMetaFormData(TestConstants.STRING_ONE);
        Assertions.assertNotNull(response);
    }

    @Test
    void getMenu() {
        SearchRequestDTO searchRequestDTO = new SearchRequestDTO();
        when(staticDataService.getMenu(any())).thenReturn(null);
        SuccessResponse<MenuDTO> response = staticDataController.getMenu(searchRequestDTO);
        Assertions.assertNotNull(response);
    }

    @Test
    void getAllFrequencies(){
        List<Frequency> frequencies = new ArrayList<>();
        when(staticDataService.getAllFrequencies()).thenReturn(frequencies);
        List<Frequency> response = staticDataController.getAllFrequencies();
        Assertions.assertNotNull(response);
    }
}
