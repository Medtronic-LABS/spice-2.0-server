package com.mdtlabs.coreplatform.commonservice.common;

import java.io.File;
import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.CultureDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import org.springframework.web.multipart.MultipartFile;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CommonUtilTest {

    @InjectMocks
    CommonUtil commonUtil;

    private static MockedStatic<UserContextHolder> userContextHolder;

    @Test
    void isNotNull() {
        //then
        Boolean response = commonUtil.isNotNull(null);
        assertNotNull(response);
        Assertions.assertFalse(response);

        //then
        response = commonUtil.isNotNull(new Object());
        Assertions.assertTrue(response);
    }

    @Test
    void getListOfStringValues() {
        //then
        List<String> response = commonUtil.getListOfStringValues(TestConstants.TEST);
        assertNotNull(response);
        Assertions.assertFalse(response.isEmpty());
    }

    @Test
    void getListOfStringValuesTest() {
        //then
        List<String> response = commonUtil.getListOfStringValues(null);
        Assertions.assertEquals(null, response);
    }

    @Test
    void getRoundedValue() {
        //then
        Double response = commonUtil.getRoundedValue(8.30);
        assertNotNull(response);
        Assertions.assertEquals(8.30, response);
    }

    @Test
    void getCurrentTimeStamp() {
        //then
        String response = commonUtil.getCurrentTimeStamp();
        assertNotNull(response);
    }

    @Test
    void convertTimeStringToDouble() {
        //then
        Double response = commonUtil.convertTimeStringToDouble("8.50");
        assertNotNull(response);
        Assertions.assertEquals(8.5, response);
    }

    @Test
    void createPagination() {
        //then
        int response = commonUtil.createPagination(TestConstants.TEN, 20);
        Assertions.assertEquals(1.0, response);
    }

    @Test
    void createPaginationTest() {
        //then
        int response = commonUtil.createPagination(Constants.ZERO, TestConstants.TEN);
        Assertions.assertEquals(0.0, response);
    }

    @Test
    void validatePatientSearchData() {
        //then
        Boolean response = commonUtil.validatePatientSearchData(List.of("123"));
        Assertions.assertFalse(response);
    }

    @Test
    void validateEmail() {
        //then
        Boolean response = commonUtil.validateEmail(TestConstants.TEST_MAIL_ID);
        assertNotNull(response);
        Assertions.assertTrue(response);

    }

    @Test
    void validatePhoneNumber() {
        //then
        Boolean response = commonUtil.validatePhoneNumber(TestConstants.TEST_MOBILE_NO);
        assertNotNull(response);
        Assertions.assertTrue(response);

    }

    @Test
    void isValidSearchData() {
        //then
        Boolean response = commonUtil.isValidSearchData("ane", Constants.EMAIL_REGEX);
        assertNotNull(response);
        Assertions.assertTrue(response);
    }

    @Test
    void isValidSearchDataTest() {
        //then
        Boolean response = commonUtil.isValidSearchData(TestConstants.EMPTY_STRING, Constants.EMAIL_REGEX);
        assertNotNull(response);
        Assertions.assertTrue(response);
    }

    @Test
    void getCurrentEntity() {
        //then
        userContextHolder = mockStatic(UserContextHolder.class);
        UserContextDTO userDTO = new UserContextDTO();
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);

        HttpEntity<String> response = commonUtil.getCurrentEntity();
        assertNotNull(response);
        userContextHolder.close();
    }

    @Test
    void isAllAlphabetic() {
        //then
        boolean response = commonUtil.isAllAlphabetic("patient");
        Assertions.assertTrue(response);

        response = commonUtil.isAllAlphabetic(TestConstants.EMPTY_STRING);
        Assertions.assertFalse(response);
    }

    @Test
    void isAllNumeric() {
        //then
        boolean response = commonUtil.isAllNumeric("1234");
        Assertions.assertTrue(response);

        response = commonUtil.isAllNumeric(TestConstants.EMPTY_STRING);
        Assertions.assertFalse(response);
    }

    @Test
    void getAuthToken() {
        //given
        userContextHolder = mockStatic(UserContextHolder.class);
        UserContextDTO userDTO = new UserContextDTO();
        userDTO.setAuthorization(Constants.AUTHORIZATION);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);

        //then
        String response = commonUtil.getAuthToken();
        assertNotNull(response);
        userContextHolder.close();
    }

    @Test
    void getAuthCookie() {
        //given
        userContextHolder = mockStatic(UserContextHolder.class);
        UserContextDTO userDTO = new UserContextDTO();
        userDTO.setCookie(Constants.AUTHORIZATION);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);

        //then
        String response = commonUtil.getAuthCookie();
        assertNotNull(response);
        userContextHolder.close();
    }

    @Test
    void getClient() {
        //given
        userContextHolder = mockStatic(UserContextHolder.class);
        UserContextDTO userDTO = new UserContextDTO();
        userDTO.setClient(TestConstants.CLIENT);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);

        //then
        String response = commonUtil.getClient();
        assertNotNull(response);
        userContextHolder.close();
    }

    @Test
    void getLoggedInEmployeeLog() {
        //given
        userContextHolder = mockStatic(UserContextHolder.class);
        UserContextDTO userDTO = new UserContextDTO();
        userDTO.setId(TestConstants.ONE);
        userDTO.setClient(TestConstants.CLIENT);
        userDTO.setUsername(TestConstants.TEST_MAIL_ID);
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userDTO);

        //then
        String response = commonUtil.getLoggedInEmployeeLog();
        assertNotNull(response);
        userContextHolder.close();
    }

    @Test
    void convertMultipartFileToFileNull() throws IOException {
        //then
        File response = CommonUtil.convertMultipartFileToFile(null);
        assertNull(response);
    }

    @Test
    void isCultureCodeNull() {
        //given
        CultureDTO cultureDTO = new CultureDTO();
        cultureDTO.setCode("12345");
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(1L);
        userContextDTO.setCulture(cultureDTO);
        userContextHolder = mockStatic(UserContextHolder.class);

        //when
        userContextHolder.when(UserContextHolder :: getUserDto).thenReturn(userContextDTO);

        //then
        boolean response = CommonUtil.isCultureCodeNull();
        assertTrue(response);
        userContextHolder.close();
    }

    @Test
    void convertMultipartFileToFile() throws IOException {
        //given
        MultipartFile multipartFile = TestDataProvider.getMockedSignatureFile();
        File convertedFile = CommonUtil.convertMultipartFileToFile(multipartFile);

        //then
        assertNotNull(convertedFile);
        assertTrue(convertedFile.delete());
    }

    @Test
    void isCommunityApp() {
        //given
        List<String> appTypes = List.of(Constants.APP_TYPE_COMMUNITY);

        //then
        assertTrue(CommonUtil.isCommunityApp(appTypes));
    }

    @Test
    void forNonCommunityApp() {
        //given
        List<String> appTypes = List.of(Constants.APP_TYPE_COMMUNITY, Constants.APP_TYPE_NON_COMMUNITY);

        //then
        assertFalse(CommonUtil.isCommunityApp(appTypes));
    }

    @Test
    void getTenantId() {
        //given
        userContextHolder = mockStatic(UserContextHolder.class);
        UserContextDTO userContextDTO = new UserContextDTO();
        userContextDTO.setId(1L);
        userContextDTO.setTenantId(1L);

        //when
        userContextHolder.when(UserContextHolder::getUserDto).thenReturn(userContextDTO);

        //then
        Long response = CommonUtil.getTenantId();
        assertEquals(userContextDTO.getTenantId(), response);
        userContextHolder.close();
    }
}