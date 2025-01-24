package com.mdtlabs.coreplatform.commonservice.common.util;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static com.mdtlabs.coreplatform.commonservice.common.util.StringUtil.concatString;
import static com.mdtlabs.coreplatform.commonservice.common.util.StringUtil.constructListFromGroupOfString;
import static com.mdtlabs.coreplatform.commonservice.common.util.StringUtil.constructString;
import static com.mdtlabs.coreplatform.commonservice.common.util.StringUtil.getObjectValueAsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.TestConstants;

@ExtendWith(MockitoExtension.class)
class StringUtilTest {

    @Test
    @DisplayName("GetObjectValueAsNull Test")
    void getObjectValueAsNullTest() {
        //then
        String result = getObjectValueAsString(null);
        assertEquals(Constants.EMPTY, result);
        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("GetObjectWithEmpty Test")
    void getObjectValueWithEmpty() {
        //then
        String result = getObjectValueAsString(new Object());
        assertFalse(result.isEmpty());
    }

    @Test
    @DisplayName("GetObjectValueWithString Test")
    void getObjectValueWithString() {
        //given
        String name = Constants.NAME;
        String value = Constants.EMPTY + name;

        //then
        String result = getObjectValueAsString(name);
        assertEquals(value, result);
    }

    @Test
    @DisplayName("GetObjectValueWithInteger Test")
    void getObjectValueWithInteger() {
        //given
        int request = TestConstants.TEN;
        String value = Constants.EMPTY + request;

        //then
        String result = getObjectValueAsString(request);
        assertEquals(value, result);
    }

    @Test
    @DisplayName("GetObjectValueWithLong Test")
    void getObjectValueWithLong() {
        //given
        long id = TestConstants.ONE;
        String value = Constants.EMPTY + id;

        //then
        String result = getObjectValueAsString(id);
        assertEquals(value, result);
    }

    @Test
    @DisplayName("GetObjectValueWithDouble Test")
    void getObjectValueWithDouble() {
        //given
        double request = 32.3d;
        String value = Constants.EMPTY + request;

        //then
        String result = getObjectValueAsString(request);
        assertEquals(value, result);
    }

    @Test
    @DisplayName("GetObjectValueWithFloat Test")
    void getObjectValueWithFloat() {
        //given
        float request = 32.3f;
        String value = Constants.EMPTY + request;

        //then
        String result = getObjectValueAsString(request);
        assertEquals(value, result);
    }

    @Test
    @DisplayName("GetObjectValueWithBoolean Test")
    void getObjectValueWithBoolean() {
        //given
        Boolean request = Constants.BOOLEAN_TRUE;
        String value = Constants.EMPTY + "1";

        //then
        String result = getObjectValueAsString(request);
        assertEquals(value, result);

        //given
        request = Constants.BOOLEAN_FALSE;
        value = Constants.EMPTY + Constants.ZERO;

        //then
        result = getObjectValueAsString(request);
        assertEquals(value, result);

    }

    @Test
    @DisplayName("ConstructString Test")
    void constructStringTest() {
        //given
        String value = "construct String args";

        //then
        String result = constructString("construct", "String", "args");
        assertEquals(value, result);
    }

    @Test
    @DisplayName("ConcatString Test")
    void concatStringTest() {
        //given
        String value = "concatStringArgs";

        //then
        String result = concatString("concat", "String", "Args");
        assertEquals(value, result);
    }

    @Test
    @DisplayName("ConstructListOfStrings Test")
    void constructListOfString() {
        //given
        List<String> list = List.of("Construct", "List", "of", "string");

        //then
        List<String> result = constructListFromGroupOfString("Construct", "List", "of", "string");
        assertEquals(list, result);
        assertEquals(list.size(), result.size());
    }

    @Test
    void convertToDateStringTest() {
        String response = StringUtil.convertToDateString(null);
        assertNull(response);
    }

    @Test
    void convertToDateStringTestDate() {
        String response = StringUtil.convertToDateString(new Date());
        assertNotNull(response);
    }

    @Test
    void getDateStringTest() {
        String response = StringUtil.getDateString(new Date());
        assertNotNull(response);
    }

    @Test
    void getDateStringNullTest() {
        String response = StringUtil.getDateString(null);
        assertNull(response);
    }

    @Test
    void getDateStringExceptionTest() {
        String response = StringUtil.getDateString(new Date(), "dfttt");
        assertNull(response);
    }

    @Test
    void getFormattedDateStringNullTest() {
        String response = StringUtil.getFormattedDateString(new Date().toString(), Constants.JSON_DATE_FORMAT);
        assertNull(response);
    }

    @Test
    void getFormattedDateStringTestException() {
        String response = StringUtil.getFormattedDateString(null, "dfttt");
        assertNull(response);
    }

    @Test
    void getCommaSeparatedValuesTest() {
        String response = StringUtil.getCommaSepratedValues(List.of("one", "two"));
        assertNotNull(response);
        assertEquals("one,two", response);
    }

    @Test
    void getCommaSeparatedValuesNullTest() {
        String response = StringUtil.getCommaSepratedValues(null);
        assertNotNull(response);
        assertEquals(Constants.EMPTY, response);
    }

    @Test
    void convertTimeDoubleToStringTest() {
        String response = StringUtil.convertTimeDoubleToString(8.5);
        assertNotNull(response);
        assertEquals("08:30", response);

    }

    @Test
    void convertTimeDoubleToStringTestNull() {
        String response = StringUtil.convertTimeDoubleToString(8.2);
        assertNotNull(response);
        assertEquals("8.2", response);
    }

    @Test
    void parseEmailTemplateNullTest() {
        String response = StringUtil.parseEmailTemplate(null, Map.of());
        assertNull(response);
    }

    @Test
    void parseEmailTemplateNullTestvalue() {
        String response = StringUtil.parseEmailTemplate(null, null);
        assertNull(response);
    }

    @Test
    void parseEmailTemplateTest() {
        String response = StringUtil.parseEmailTemplate("{{name}}", Map.of("name", "Test"));
        assertEquals("Test", response);
    }

    @Test
    void isBlank() {
        //when
        String value = "Test";
        //then
        assertFalse(StringUtil.isBlank(value));
    }

    @Test
    void isBlankWhenValueIsNull() {
        //then
        assertTrue(StringUtil.isBlank(null));
    }

    @Test
    void isBlankWhenValueIsEmpty() {
        //when
        String value = "";
        //then
        assertTrue(StringUtil.isBlank(value));
    }

    @Test
    void isNotBlank() {
        //when
        String value = "Test";

        //then
        assertTrue(StringUtil.isNotBlank(value));
    }

    @Test
    void isNotBlankWhenValueIsNull() {
        //then
        assertFalse(StringUtil.isNotBlank(null));
    }

    @Test
    void isNotBlankWhenValueIsEmpty() {
        //when
        String value = "";

        //then
        assertFalse(StringUtil.isNotBlank(value));
    }

    @Test
    void capitalizeEachWord() {
        //given
        String words = "test word";

        //then
        assertEquals("Test Word", StringUtil.capitalizeEachWord(words));
    }

    @Test
    void capitalizeEachWordWhenWordIsEmpty() {
        //given
        String words = "";

        //then
        assertEquals("", StringUtil.capitalizeEachWord(words));
    }
}