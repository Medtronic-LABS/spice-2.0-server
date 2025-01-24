package com.mdtlabs.coreplatform.commonservice.common.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import com.mdtlabs.coreplatform.commonservice.common.Constants;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DateUtilTest {

    @Test
    void daysSincePastTest() {
        Long response = DateUtil.daysSincePast(new Date());
        assertEquals(0L, response);
    }

    @Test
    void daysSincePastInWeeks() {
        Long response = DateUtil.daysSincePastInWeeks((new Date()));
        assertNotNull(response);

        response = DateUtil.daysSincePastInWeeks(null);
        assertEquals(0L, response);
    }

    @Test
    void daysSincePastInMonths() {
        Long response = DateUtil.daysSincePastInMonths((new Date()));
        assertNotNull(response);

        response = DateUtil.daysSincePastInMonths(null);
        assertEquals(0L, response);
    }

    @Test
    void daysSincePastNullTest() {
        Long response = DateUtil.daysSincePast(null);
        assertEquals(0L, response);
    }

    @Test
    void getCalendarDiff() {
        Integer calenderDiff = DateUtil.getCalendarDiff(new Date(), new Date(), 2);
        assertEquals(Integer.class, calenderDiff.getClass());
    }

    @Test
    void getCalendarDiffNullTest() {
        Assertions.assertThrows(NullPointerException.class, () -> DateUtil.getCalendarDiff(null, null, 0));
    }

    @Test
    void getCalendarDiffWithTwoDates() {
        Integer calenderDiff = DateUtil.getCalendarDiff(new Date(), new Date());
        assertEquals(Integer.class, calenderDiff.getClass());
    }

    @Test
    void yearsSincePastTest() {
        Integer response = DateUtil.yearsSincePast(new Date());
        assertEquals(Constants.ZERO, response);

        //given
        Date date = null;

        //then
        response = DateUtil.yearsSincePast(date);
        assertEquals(Constants.ZERO, response);
    }

    @Test
    void yearsSincePastNullTest() {
        Integer response = DateUtil.yearsSincePast("12/12/2002");
        assertEquals(LocalDate.now().getYear() - 1 - 2002, response);
    }

    @Test
    void yearsSincePastBlankTest() {
        Integer response = DateUtil.yearsSincePast("");
        assertEquals(0, response);
    }

    @Test
    void getEndOfDay() {
        Date response = DateUtil.getEndOfDay();
        assertNotNull(response);
    }

    @Test
    void getStartOfDay() {
        String response = DateUtil.getStartOfDay("+02:00");
        assertNotNull(response);
    }

    @Test
    void getEndOfDayTest() {
        String response = DateUtil.getEndOfDay("+02:00");
        assertNotNull(response);
    }

    @Test
    void getStartOfDayTest() {
        Date response = DateUtil.getStartOfDay();
        assertNotNull(response);
    }

    @Test
    void testValidDateFormat() {
        //given
        String dateStr = "2025-01-01";
        String formatStr = "yyyy-MM-dd";

        //then
        Date result = DateUtil.formatDate(dateStr, formatStr);
        assertNotNull(result);
    }

    @Test
    void formatDateTestWhenReturnsNullThroughException() {
        Date dateResponse = DateUtil.formatDate("12-12-2020", Constants.JSON_DATE_FORMAT);
        assertNull(dateResponse);
    }

    @Test
    void formatDateNullTest() {
        Date dateResponse = DateUtil.formatDate("12-12-2020", null);
        assertNull(dateResponse);
    }

    @Test
    void formatDateTestDate() {
        Date dateResponse = DateUtil.formatDate(new Date(), Constants.JSON_DATE_FORMAT);
        assertNotNull(dateResponse);
        assertEquals(new Date().getClass(), dateResponse.getClass());
    }

    @Test
    void formatDateTestNewDate() {
        Date dateResponse = DateUtil.formatDate(new Date());
        assertNotNull(dateResponse);
        assertEquals(Date.class, dateResponse.getClass());
    }

    @Test
    void testFormatDateWhenNullDate() {
        //given
        Date date = null;
        MockedStatic<CommonUtil> commonUtilMockedStatic = Mockito.mockStatic(CommonUtil.class);

        //when
        commonUtilMockedStatic.when(CommonUtil :: isNull).thenReturn(true);

        //then
        Date result = DateUtil.formatDate(date);
        assertNull(result);
        commonUtilMockedStatic.close();
    }

    @Test
    void getTreatmentPlanFollowupDate() {
        Date dateResponse = DateUtil.getTreatmentPlanFollowupDate(Constants.WEEK, 2, null);
        assertNotNull(dateResponse);
        assertEquals(Date.class, dateResponse.getClass());
    }

    @Test
    void getTreatmentPlanFollowupDateWhenAssessmentDateIsNotNull() {
        Date dateResponse = DateUtil.getTreatmentPlanFollowupDate(Constants.WEEK, 2, new Date());
        assertNotNull(dateResponse);
        assertEquals(Date.class, dateResponse.getClass());
    }

    @Test
    void getTreatmentPlanFollowupDay() {
        Date dateResponse = DateUtil.getTreatmentPlanFollowupDate(Constants.DAY, 2, null);
        assertNotNull(dateResponse);
        assertEquals(Date.class, dateResponse.getClass());
    }

    @Test
    void getTreatmentPlanFollowupDayEmpty() {
        Date dateResponse = DateUtil.getTreatmentPlanFollowupDate("", 2, null);
        assertNotNull(dateResponse);
        assertEquals(Date.class, dateResponse.getClass());
    }

    @Test
    void getDateDiffInMinutesNullTest() {
        Long response = DateUtil.getDateDiffInMinutes(null, null);
        assertEquals(0L, response);
    }

    @Test
    void getDateDiffInMinutesNullTests() {
        Long response = DateUtil.getDateDiffInMinutes(new Date(), null);
        assertEquals(0L, response);
    }

    @Test
    void getDateDiffInMinutesTest() {
        Long response = DateUtil.getDateDiffInMinutes(new Date(), new Date());
        assertEquals(0L, response);
    }

    @Test
    void addDateTest() {
        Date dateResponse = DateUtil.addDate(new Date(), 2);
        assertNotNull(dateResponse);
    }

    @Test
    void getDiffYears() {
        Integer dateResponse = DateUtil.getDiffYears(new Date(), new Date());
        assertNotNull(dateResponse);
    }

    @Test
    void subtractDates() {
        Date dateResponse = DateUtil.subtractDates(new Date(), 2);
        assertNotNull(dateResponse);
    }

    @Test
    void getDateField() {
        Integer dateResponse = DateUtil.getDateField(new Date(), 0);
        assertNotNull(dateResponse);
    }

    @Test
    void getDayOfWeek() {
        Date dateResponse = DateUtil.getDayOfWeek(0);
        assertNotNull(dateResponse);
    }

    @Test
    void formatDateString() {
        Date formatDate = DateUtil.formatDate(new Date().toString());
        assertEquals(null, formatDate);
    }

    @Test
    void getHoursFromString() {
        Float response = DateUtil.getHoursFromString("10");
        assertNotNull(response);
    }

    @Test
    void getCurrentDay() {
        Date response = DateUtil.getCurrentDay();
        assertNotNull(response);
    }

    @Test
    void getPreviousWeekStartDate() {
        Date response = DateUtil.getPreviousWeekStartDate();
        assertNotNull(response);
    }

    @Test
    void getFirstDayOfWeek() {
        Date response = DateUtil.getFirstDayOfWeek();
        assertNotNull(response);
    }

    @Test
    void getDiffInMinutes() {
        Long response = DateUtil.getDiffInMinutes(new Date(), new Date());
        assertNotNull(response);
    }

    @Test
    void getDiffInHours() {
        Long response = DateUtil.getDiffInHours(new Date(), new Date());
        assertNotNull(response);
    }

    @Test
    void getYearDiff() {
        Integer response = DateUtil.getYearDiff(new Date());
        assertNotNull(response);
    }

    @Test
    void isDayOnFuture() {
        boolean response = DateUtil.isDayOnFuture("2012-12-12");
        Assertions.assertFalse(response);
    }

    @Test
    void isSameDate() {
        boolean response = DateUtil.isSameDate(new Date(), new Date());
        Assertions.assertTrue(response);
    }

    @Test
    void isDifferentDate() throws ParseException {
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        boolean response = DateUtil.isSameDate(new Date(), formatter.parse("12/03/2023"));
        Assertions.assertFalse(response);
    }

    @Test
    void getWeeks() throws ParseException {
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        int weeks = DateUtil.getWeeks(formatter.parse("12/03/2023"));
        Assertions.assertTrue(weeks > 16);
    }

    @Test
    void addMinutesToCurrentDate() {
        Date response = DateUtil.addMinutesToCurrentDate(1);
        assertNotNull(response);
    }

    @Test
    void addDaysToCurrentDate() {
        Date response = DateUtil.addDaysToCurrentDate(1);
        assertNotNull(response);
    }

    @Test
    void getDiffInHoursWithNullValues() {
        Long response = DateUtil.getDiffInHours(null, new Date());
        assertEquals(0, response);

        response = DateUtil.getDiffInHours(new Date(), null);
        assertEquals(0, response);
    }

    @Test
    void getDateFieldWithValues() {
        Integer response = DateUtil.getDateField("12-12-2020", Constants.JSON_DATE_FORMAT, 2);
        assertNull(response);
    }

    @Test
    void getDateFieldWithTwoValues() {
        Integer response = DateUtil.getDateField("12-12-2020", 2);
        assertNull(response);
    }

    @Test
    void getUserTimezoneTime() {
        //given
        String timezone = "+05:30";
        int subtractDay = 10;
        boolean isEndTime = true;

        //then
        String result = DateUtil.getUserTimezoneTime(timezone, subtractDay, isEndTime);
        assertNotNull(result);
    }

    @Test
    void getUserTimezoneTimeWhenIsSendTimeIsFalse() {
        //given
        String timezone = "+05:30";
        int subtractDay = 1;
        boolean isEndTime = false;

        //then
        String result = DateUtil.getUserTimezoneTime(timezone, subtractDay, isEndTime);
        assertNotNull(result);
    }

    @Test
    void convertDateToString() {
        //given
        Date date = new Date();

        //then
        String result = DateUtil.convertDateToString(date);
        assertNotNull(result);
    }

    @Test
    void convertDateToStringInFHIRFormat() {
        //given
        Date date = new Date();

        //then
        String result = DateUtil.convertDateToStringInFHIRFormat(date);
        assertNotNull(result);
    }

    @Test
    void getStartDayOfWeekByUserTimeZone() {
        String timezone = "+05:30";

        //then
        String result = DateUtil.getStartDayOfWeekByUserTimeZone(timezone);
        assertNotNull(result);
    }

    @Test
    void addDateWithTimezone() {
        //given
        String timezone = "+05:30";
        int daysToAdd = 1;
        Date date = new Date();

        //then
        Date result = DateUtil.addDateWithTimezone(date, daysToAdd, timezone);
        assertNotNull(result);
    }

    @Test
    void getStartDayOfMonthByUserTimeZone() {
        //given
        String timezone = "+05:30";

        //then
        String result = DateUtil.getStartDayOfMonthByUserTimeZone(timezone);
        assertNotNull(result);
    }

    @Test
    void getISOStringThrowsException() {
        //given
        String timezone = "+05:30";
        Calendar calendar = mock(Calendar.class);

        //then
        assertThrows(NullPointerException.class, () -> DateUtil.getISOString(calendar, timezone));
    }

    @Test
    void getISODateThrowsException() {
        //given
        Calendar calendar = mock(Calendar.class);

        //then
        assertThrows(IllegalArgumentException.class, () -> DateUtil.getISODate(calendar));
    }

    @Test
    void getYesterdayStartOfTheDay() {
        //then
        Date result = DateUtil.getYesterdayStartOfTheDay();
        assertNotNull(result);
    }

    @Test
    void getYesterdayEndOfTheDay() {
        //then
        Date result = DateUtil.getYesterdayEndOfTheDay();
        assertNotNull(result);
    }

    @Test
    void getStartDayOfWeek() {
        //then
        Date result = DateUtil.getStartDayOfWeek();
        assertNotNull(result);
    }

    @Test
    void getFirstDayOfMonth() {
        //then
        Date result = DateUtil.getFirstDayOfMonth();
        assertNotNull(result);
    }

}
