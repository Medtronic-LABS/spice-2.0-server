package com.mdtlabs.coreplatform.cqlservice.util;

import java.util.Date;
import java.util.HashMap;
import java.util.Set;

import org.hl7.fhir.r4.model.Bundle;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.cqlservice.constants.CqlConstants;
import com.mdtlabs.coreplatform.cqlservice.model.dto.CqlRequestDTO;
import com.mdtlabs.coreplatform.cqlservice.model.entity.CqlResult;

public class TestDataProvider {

    private static MockedStatic<CommonUtil> commonUtil;
    private static final Date date = new Date();
    public static CqlRequestDTO getCqlRequestDTO() {
        CqlRequestDTO cqlRequestDTO = new CqlRequestDTO();
        cqlRequestDTO.setResourceBundle(TestConstants.TEST_BUNDLE);
        cqlRequestDTO.setLibraries(Set.of(CqlConstants.ANC_DT_01, CqlConstants.ANC_S_01));
        cqlRequestDTO.setExpressions(Set.of(Constants.EMPTY));
        return cqlRequestDTO;
    }

    public static CqlResult getCqlResult() {
        CqlResult cqlResult = new CqlResult();
        cqlResult.setMemberId(TestConstants.ONE);
        cqlResult.setPatientId(TestConstants.ONE);
        cqlResult.setResourceId(TestConstants.ONE);
        cqlResult.setResults(new HashMap<>());
        return cqlResult;
    }

    public static Date getDate() {
        return date;
    }

    public static Bundle getBundle() {
        return CqlUtils.getFhirContext().newJsonParser().parseResource(Bundle.class, TestConstants.TEST_BUNDLE);
    }

    public static void init() {
        commonUtil = Mockito.mockStatic(CommonUtil.class);
    }

    public static void getStaticMock() {
        commonUtil.when(CommonUtil::getAuthToken).thenReturn(TestConstants.TOKEN);
        commonUtil.when(CommonUtil::getClient).thenReturn(TestConstants.CLIENT_VALUE);
    }

    public static void cleanUp() {
        commonUtil.close();
    }
}
