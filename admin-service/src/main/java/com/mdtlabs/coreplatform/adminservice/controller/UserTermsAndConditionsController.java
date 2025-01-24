package com.mdtlabs.coreplatform.adminservice.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.UserTermsAndConditionsService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.UserTermsAndConditions;

/**
 * <p>
 *   This controller class maintains CRUD operation for Terms and condition data.
 * </p>
 *
 * @author Divya created on Oct 22, 2024
 */
@RestController
@RequestMapping("/terms-and-conditions")
public class UserTermsAndConditionsController {

    private final UserTermsAndConditionsService userTermsAndConditionsService;

    @Autowired
    public UserTermsAndConditionsController(UserTermsAndConditionsService userTermsAndConditionsService) {
        this.userTermsAndConditionsService = userTermsAndConditionsService;
    }

    /**
     * <p>
     *   Get the Terms and Condition data based on country and tenant id.
     * </p>
     *
     * @param searchRequestDTO {@link SearchRequestDTO} UserTermsAndCondition request dto
     * @return {@link SuccessResponse UserTermsAndCondition} UserTermsAndCondition entity
     */
    @PostMapping("/details")
    public SuccessResponse<UserTermsAndConditions> getTermsAndConditionDetails(@RequestBody SearchRequestDTO searchRequestDTO) {
        return new SuccessResponse<>(SuccessCode.GET_TERMS_AND_CONDITION,
                userTermsAndConditionsService.getTermsAndConditionsValue(searchRequestDTO), HttpStatus.OK);
    }
}
