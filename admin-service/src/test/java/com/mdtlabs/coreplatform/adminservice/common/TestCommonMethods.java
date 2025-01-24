package com.mdtlabs.coreplatform.adminservice.common;

import com.mdtlabs.coreplatform.adminservice.util.TestConstants;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserSelectedTenantContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserDTO;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;

import org.mockito.MockedStatic;

import static com.mdtlabs.coreplatform.adminservice.util.TestDataProvider.modelMapper;
import static org.mockito.Mockito.mockStatic;

/**
 * <p>
 *   This class has the common mock methods for static class.
 * </p>
 *
 * @author Divya S
 */
public class TestCommonMethods {

    private static MockedStatic<CommonUtil> commonUtil;
    private static MockedStatic<UserSelectedTenantContextHolder> userSelectedTenantContextHolder;
    private static MockedStatic<UserContextHolder> userContextHolder;


    public static void init() {
        commonUtil = mockStatic(CommonUtil.class);
        userSelectedTenantContextHolder = mockStatic(UserSelectedTenantContextHolder.class);
        userContextHolder = mockStatic(UserContextHolder.class);
    }



    public static void getStaticMock() {
        UserDTO userDTO = getUserDTO();
        userDTO.setId(TestConstants.ONE);
        commonUtil.when(CommonUtil::getAuthToken).thenReturn("BearerTest");
        commonUtil.when(CommonUtil::getClient).thenReturn("TestAdmin");
        userSelectedTenantContextHolder.when(UserSelectedTenantContextHolder::get).thenReturn(1L);
    }
    public static UserDTO getUserDTO() {
        UserDTO userDTO = modelMapper.map(TestDataProvider.getUser(), UserDTO.class);
        return userDTO;
    }

    public static void getStaticMockValidation(String searchTerm) {
        commonUtil.when(() -> CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)).thenReturn(true);
        commonUtil.when(() -> CommonUtil.isValidSearchData(searchTerm, Constants.REGEX_SEARCH_PATTERN)).thenReturn(true);
    }

    public static void getStaticMockValidationFalse(String searchTerm) {
        commonUtil.when(() -> CommonUtil.isValidSearchData(searchTerm, Constants.SEARCH_TERM)).thenReturn(false);
        commonUtil.when(() -> CommonUtil.isValidSearchData(searchTerm, Constants.REGEX_SEARCH_PATTERN)).thenReturn(false);
    }

    public static void cleanUp() {
        commonUtil.close();
        userSelectedTenantContextHolder.close();
        userContextHolder.close();
    }
}
