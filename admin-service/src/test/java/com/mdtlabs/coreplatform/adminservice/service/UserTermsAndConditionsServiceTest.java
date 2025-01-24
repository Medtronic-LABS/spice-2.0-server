package com.mdtlabs.coreplatform.adminservice.service;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.adminservice.repository.UserTermsAndConditionsRepository;
import com.mdtlabs.coreplatform.adminservice.service.impl.UserTermsAndConditionsServiceImpl;
import com.mdtlabs.coreplatform.adminservice.util.TestDataProvider;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;

/**
 * <p>
 * This class has the test methods for UserTermsandCondition service implementation.
 * </p>
 *
 * @author Divya created on Oct 22, 2024
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserTermsAndConditionsServiceTest {

    @InjectMocks
    private UserTermsAndConditionsServiceImpl userTermsAndConditionServiceImpl;

    @Mock
    private UserTermsAndConditionsRepository userTermsAndConditionsRepository;

    @Test
    void getTermsAndConditionValueTest() {
        //given
        UserTermsAndConditions userTermsAndConditionsRequest = TestDataProvider.getUserTermsAndCondition();
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        List<UserTermsAndConditions> userTermsAndConditionlist = List.of(userTermsAndConditionsRequest);

        //when
        when(userTermsAndConditionsRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(requestDto.getCountryId())).thenReturn(userTermsAndConditionlist);

        //then
        UserTermsAndConditions userTermsAndConditionsResponse = userTermsAndConditionServiceImpl.getTermsAndConditionsValue(requestDto);
        assertNotNull(userTermsAndConditionsResponse);
        assertEquals(userTermsAndConditionsRequest.getCountryId(), userTermsAndConditionsResponse.getCountryId());
    }

    @Test
    void getEmptyTermsAndConditionValueTest() {
        //given
        SearchRequestDTO requestDto = TestDataProvider.getSearchRequestDTO();
        List<UserTermsAndConditions> userTermsAndConditionlist = new ArrayList<>();

        //when
        when(userTermsAndConditionsRepository.findByCountryIdAndIsDeletedFalseAndIsActiveTrue(requestDto.getCountryId())).thenReturn(userTermsAndConditionlist);

        //then
        assertThrows(DataNotFoundException.class, () -> userTermsAndConditionServiceImpl.getTermsAndConditionsValue(requestDto));
    }
}
