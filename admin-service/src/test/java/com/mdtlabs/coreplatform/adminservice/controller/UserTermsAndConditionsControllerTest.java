package com.mdtlabs.coreplatform.adminservice.controller;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.UserTermsAndConditionsService;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;


/**
 * <p>
 * This class has the test methods for UserTermsAndConditionController.
 * </p>
 *
 * @author Denisha J
 */
@ExtendWith(MockitoExtension.class)
class UserTermsAndConditionsControllerTest {

    @InjectMocks
    private UserTermsAndConditionsController userTermsAndConditionsController;

    @Mock
    private UserTermsAndConditionsService userTermsAndConditionsService;

    @Test
    void getTermsAndConditionDetailsTest() {
        //given
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        UserTermsAndConditions userTermsAndConditions = TestDataProvider.getUserTermsAndCondition();

        //when
        when(userTermsAndConditionsService.getTermsAndConditionsValue(requestDto)).thenReturn(userTermsAndConditions);

        //then
        SuccessResponse<UserTermsAndConditions> userTermsAndConditionResponse = userTermsAndConditionsController.getTermsAndConditionDetails(requestDto);
        assertNotNull(userTermsAndConditionResponse);
        assertEquals(HttpStatus.OK, userTermsAndConditionResponse.getStatusCode());
    }
}
