package com.mdtlabs.coreplatform.commonservice.common.util;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.StringUtils;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;


/**
 * <p>
 * This is used to calculate the past day from particular date
 * </p>
 *
 * @author Karthick Murugesan Created on Jan 11, 2024
 */
public class DateUtil {

    private DateUtil() {
    }

    /**
     * <p>
     * this is used to calculate days since particular date
     * </p>
     *
     * @param date  input date
     * @return long  days since past
     */
    public static long daysSincePast(Date date) {
        if (null != date) {
            LocalDate dateBefore = getZonedDateTime(date).toLocalDate();
            LocalDate dateAfter = getZonedDateTime(new Date()).toLocalDate();
            return ChronoUnit.DAYS.between(dateBefore, dateAfter);
        }
        return Constants.ZERO;
    }

    /**
     * Calculates the number of weeks between the current date and the provided date.
     *
     * @param date The date from which to calculate the number of weeks until now.
     * @return The number of weeks between the current date and the provided date.
     *         If the provided date is null, returns zero.
     */
    public static long daysSincePastInWeeks(Date date) {
        if (null != date) {
            LocalDate dateBefore = getZonedDateTime(date).toLocalDate();
            LocalDate dateAfter = getZonedDateTime(new Date()).toLocalDate();
            return ChronoUnit.WEEKS.between(dateBefore, dateAfter);
        }
        return Constants.ZERO;
    }

    /**
     * Used to get time with user's timezone
     * @param timezone
     * @param subtractDay
     * @param isEndTime
     * @return
     */
    public static String getUserTimezoneTime(String timezone, int subtractDay, boolean isEndTime) {
        Calendar calendar = getCalendar(timezone, subtractDay);
        setCalenderValues(calendar, isEndTime);
        return getISOString(calendar);
    }

    /**
     * Sets a calender values by start and end time
     *
     * @param calendar
     * @param isEndTime
     */
    public static void setCalenderValues(Calendar calendar, boolean isEndTime) {
        calendar.set(Calendar.HOUR_OF_DAY, isEndTime ? 23 : 0);
        calendar.set(Calendar.MINUTE, isEndTime ? 59: 0);
        calendar.set(Calendar.SECOND, isEndTime ? 59 :0);
        calendar.set(Calendar.MILLISECOND, isEndTime ? 999 : 0);
    }

    /**
     * Used to get Calender
     * @param timeZone
     * @param subtractDay
     * @return
     */
    public static Calendar getCalendar(String timeZone, int subtractDay) {
        ZoneOffset zoneOffSet = ZoneOffset.of(timeZone);
        OffsetDateTime currentOffSetDateTime = OffsetDateTime.now(zoneOffSet);

        Date currentDate = Date.from(currentOffSetDateTime.toInstant());
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(currentDate);
        calendar.add(Calendar.DATE, -subtractDay);
        calendar.set(Calendar.ZONE_OFFSET, currentOffSetDateTime.getOffset().getTotalSeconds() * 1000);
        return calendar;
    }

    /**
     * Get a Iso date string format.
     * 
     * @return {@Link String} - Return the converted iso date format. 
     */
    public static String getISOString(Calendar calendar) {
        DateFormat df = new SimpleDateFormat(Constants.FHIR_DATE_TIME_FORMAT);
        // offset
        df.setTimeZone(TimeZone.getTimeZone(Constants.TIMEZONE_UTC));
        return df.format(calendar.getTime());
    }

    /**
     * Gets start day of week based on user timezone
     *
     * @param timeZone
     * @return
     */
    public static String getStartDayOfWeekByUserTimeZone(String timeZone) {
        ZoneOffset zoneOffSet = ZoneOffset.of(timeZone);
        OffsetDateTime currentOffSetDateTime = OffsetDateTime.now(zoneOffSet);
        Date currentDate = Date.from(currentOffSetDateTime.toInstant());
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(currentDate);
        int day = calendar.get(Calendar.DAY_OF_WEEK);
        calendar.add(Calendar.DATE, -day + 1);
        calendar.set(Calendar.ZONE_OFFSET, currentOffSetDateTime.getOffset().getTotalSeconds() * 1000);
        setCalenderValues(calendar, false);
        return getISOString(calendar);
    }

    /**
     * Used to convert Date to String
     * @param date To convert the date
     * @return dateString
     */
    public static String convertDateToStringInFHIRFormat(Date date) {
        DateFormat dateFormat = new SimpleDateFormat(Constants.FHIR_DATE_TIME_FORMAT);
        dateFormat.setTimeZone(TimeZone.getTimeZone(Constants.TIMEZONE_UTC));
        return dateFormat.format(date);
    }

    /**
     * Used to add days from given date with given timezone
     * @param date add days from this date
     * @param daysToAdd days need to add
     * @param timezone timezone
     * @return Date
     */
    public static Date addDateWithTimezone(Date date, int daysToAdd, String timezone) {
        ZoneOffset zoneOffSet = ZoneOffset.of(timezone);
        OffsetDateTime currentOffSetDateTime = OffsetDateTime.now(zoneOffSet);
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.DATE, daysToAdd);
        calendar.set(Calendar.ZONE_OFFSET, currentOffSetDateTime.getOffset().getTotalSeconds() * 1000);
        setCalenderValues(calendar, true);
        return calendar.getTime();
    }

    /**
     * Gets start day of week based on user timezone
     *
     * @param timeZone
     * @return
     */
    public static String getStartDayOfMonthByUserTimeZone(String timeZone) {
        Calendar calendar = getCalendar(timeZone, 0);
        calendar.set(Calendar.DATE, 1);
        setCalenderValues(calendar, false);
        return getISOString(calendar);
    }

    /**
     * Calculates the number of months between the current date and the provided date.
     *
     * @param date The date from which to calculate the number of months until now.
     * @return The number of months between the current date and the provided date.
     *         If the provided date is null, returns zero.
     */
    public static long daysSincePastInMonths(Date date) {
        if (null != date) {
            LocalDate dateBefore = getZonedDateTime(date).toLocalDate();
            LocalDate dateAfter = getZonedDateTime(new Date()).toLocalDate();
            return ChronoUnit.MONTHS.between(dateBefore, dateAfter);
        }
        return Constants.ZERO;
    }

    /**
     * <p>
     * This method is used to calculate year since particular date in string.
     * </p>
     *
     * @param dateString  input date string
     * @return int  years since past input date
     */
    public static int yearsSincePast(String dateString) {
        if (StringUtils.isNotBlank(dateString)) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.DATE_TIME_FORMATTER);
            LocalDate dateTimeDob = LocalDate.parse(dateString, formatter);
            return Period.between(dateTimeDob, LocalDate.now()).getYears();
        }
        return Constants.ZERO;
    }

    /**
     * <p>
     * This method is used to calculate year since particular date.
     * </p>
     *
     * @param date input date
     * @return int  years since past
     */
    public static int yearsSincePast(Date date) {
        if (null != date) {
            LocalDate dateBefore = getZonedDateTime(date).toLocalDate();
            LocalDate dateAfter = getZonedDateTime(new Date()).toLocalDate();
            return Period.between(dateBefore, dateAfter).getYears();
        }
        return Constants.ZERO;
    }

    /**
     * <p>
     * This method is used to get zonedDateTime.
     * </p>
     *
     * @param date  input date
     * @return ZonedDateTime  zoned date time response
     */
    public static ZonedDateTime getZonedDateTime(Date date) {
        Instant timeStamp = date.toInstant();
        String zoneId = String.valueOf(ZoneId.of("UTC"));
        return timeStamp.atZone(ZoneId.of(zoneId));
    }

    /**
     * <p>
     * This method is used to format date.
     * </p>
     *
     * @param dateStr    input date
     * @param formatStr  format type
     * @return Date  converted date into specified format
     */
    public static Date formatDate(String dateStr, String formatStr) {
        DateFormat df = null;
        Date date = null;
        try {
            if (!Objects.isNull(dateStr) && !Objects.isNull(formatStr)) {
                df = new SimpleDateFormat(formatStr);
                date = df.parse(dateStr);
                date = new Date(date.getTime() - Calendar.getInstance().getTimeZone().getOffset(date.getTime()));
            }
        } catch (ParseException ex) {
            return null;
        }
        return date;
    }

    /**
     * Formats the provided date string into a Date object.
     *
     * @param dateStr The date string to be formatted.
     * @return A Date object representing the formatted date string.
     *         If the provided date string is null, returns null.
     */
    public static Date formatDate(String dateStr) {
        return !CommonUtil.isNull(dateStr) ? formatDate(dateStr, Constants.FHIR_DATE_TIME_FORMAT) : null;
    }

    /**
     * <p>
     * This method is used to format date on Date.
     * </p>
     *
     * @param date  date to be formatted
     * @return Date  formatted date
     */
    public static Date formatDate(Date date) {
        return !CommonUtil.isNull(date) ? formatDate(StringUtil.getDateString(date), Constants.JSON_DATE_FORMAT) : null;
    }

    /**
     * Formats the provided date into a string using the provided format.
     *
     * @param date The date to be formatted.
     * @param format The format to be used for formatting the date.
     * @return A Date object representing the formatted date.
     *         If the provided date is null, returns null.
     */
    public static Date formatDate(Date date, String format) {
        return !CommonUtil.isNull(date) ? formatDate(StringUtil.getDateString(date), format) : null;
    }

    /**
     * Calculates the difference between two dates based on the provided type.
     *
     * @param fromDate The start date.
     * @param toDate The end date.
     * @param type The type of difference to be calculated (e.g., Calendar.YEAR for years, Calendar.MONTH for months, etc.).
     * @return The difference between the two dates based on the provided type.
     *         If either of the provided dates is null, returns zero.
     */
    public static int getCalendarDiff(Date fromDate, Date toDate, int type) {
        Calendar fromDateCalendar = Calendar.getInstance();
        Calendar toDateCalendar = Calendar.getInstance();
        fromDateCalendar.setTime(fromDate);
        toDateCalendar.setTime(toDate);
        if (!CommonUtil.isNull(fromDate, toDate)) {
            return (toDateCalendar.get(type) - fromDateCalendar.get(type));
        }
        return Constants.ZERO;
    }

    /**
     * Calculates the difference in days between two dates.
     *
     * @param fromDate The start date.
     * @param toDate The end date.
     * @return The difference in days between the two dates.
     *         If either of the provided dates is null, returns zero.
     */
    public static int getCalendarDiff(Date fromDate, Date toDate) {
        Instant fromDateInstant = fromDate.toInstant();
        Instant toDateInstant = toDate.toInstant();
        if (!CommonUtil.isNull(fromDate, toDate)) {
            Long noOfDaysLong = ChronoUnit.DAYS.between(fromDateInstant, toDateInstant);
            return noOfDaysLong.intValue();
        }
        return Constants.ZERO;
    }


    /**
     * Checks if two dates are the same (i.e., they fall on the same day, month, and year).
     *
     * @param fromDate The first date.
     * @param toDate The second date.
     * @return true if the two dates are the same, false otherwise.
     */
    public static boolean isSameDate(Date fromDate, Date toDate) {
        Calendar fromDateCalendar = Calendar.getInstance();
        Calendar toDateCalendar = Calendar.getInstance();
        fromDateCalendar.setTime(fromDate);
        toDateCalendar.setTime(toDate);
        return fromDateCalendar.get(Calendar.YEAR) == toDateCalendar.get(Calendar.YEAR) &&
                fromDateCalendar.get(Calendar.MONTH) == toDateCalendar.get(Calendar.MONTH) &&
                fromDateCalendar.get(Calendar.DAY_OF_MONTH) == toDateCalendar.get(Calendar.DAY_OF_MONTH);
    }

    /**
     * Calculates the difference in years between two dates.
     *
     * @param fromDate The start date.
     * @param toDate The end date.
     * @return The difference in years between the two dates.
     */
    public static int getDiffYears(Date fromDate, Date toDate) {
        return getCalendarDiff(fromDate, toDate, Calendar.YEAR);
    }

    /**
     * Subtracts a specified number of days from a date.
     *
     * @param date The date from which days will be subtracted.
     * @param daysToSubtract The number of days to subtract from the date.
     * @return A Date object representing the date after subtracting the specified number of days.
     */
    public static Date subtractDates(Date date, int daysToSubtract) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.add(Calendar.DATE, -daysToSubtract);
        return cal.getTime();
    }

    /**
     * Retrieves a specific field from a date.
     *
     * @param date The date from which the field will be retrieved.
     * @param type The type of field to retrieve from the date.
     * @return The value of the specified field in the date.
     */
    public static Integer getDateField(Date date, int type) {
        Integer value = null;
        Calendar calendar = null;
        if (!CommonUtil.isNull(date, type)) {
            calendar = Calendar.getInstance();
            calendar.setTime(date);
            return calendar.get(type);
        }
        return value;
    }

    /**
     * Retrieves a specific field from a date string.
     *
     * @param date The date string from which the field will be retrieved.
     * @param dateFormat The format of the date string.
     * @param type The type of field to retrieve from the date string.
     * @return The value of the specified field in the date string.
     */
    public static Integer getDateField(String date, String dateFormat, int type) {
        return getDateField(formatDate(date, dateFormat), type);
    }

    /**
     * Retrieves a specific field from a date string.
     *
     * @param date The date string from which the field will be retrieved.
     * @param type The type of field to retrieve from the date string.
     * @return The value of the specified field in the date string.
     */
    public static Integer getDateField(String date, int type) {
        return getDateField(formatDate(date), type);
    }

    /**
     * Gets a Date object representing a specific day of the week.
     *
     * @param dayOfWeek The day of the week (1 = Sunday, 2 = Monday, ..., 7 = Saturday).
     * @return A Date object representing the specified day of the week.
     */
    public static Date getDayOfWeek(int dayOfWeek) {
        Calendar c = Calendar.getInstance();
        c.set(Calendar.DAY_OF_WEEK, dayOfWeek);
        return c.getTime();
    }

    /**
     * Converts a string representing hours into a float.
     *
     * @param loggedHours The string representing hours.
     * @return The float value of the hours.
     */
    public static float getHoursFromString(String loggedHours) {
        loggedHours = loggedHours.replaceAll(Constants.HOUR_SEPERATOR, Constants.DOT);
        return Float.parseFloat(loggedHours);
    }

    /**
     * Checks if a specified date is in the future.
     *
     * @param loggedDate The date to check.
     * @return true if the specified date is in the future, false otherwise.
     */
    public static boolean isDayOnFuture(String loggedDate) {
        Date formattedDate = formatDate(loggedDate, Constants.DATE_FORMAT_YYYY_MM_DD);
        return new Date().before(formattedDate);
    }

    /**
     * Gets the current date.
     *
     * @return A Date object representing the current date.
     */
    public static Date getCurrentDay() {
        return new Date();
    }

    /**
     * Calculates the start date of the previous week.
     *
     * @return A Date object representing the start date of the previous week.
     */
    public static Date getPreviousWeekStartDate() {
        Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        c.add(Calendar.DATE, -Constants.ONE);
        int i = c.get(Calendar.DAY_OF_WEEK) - c.getFirstDayOfWeek();
        c.add(Calendar.DATE, -i - 7);
        return c.getTime();
    }

    /**
     * Calculates the date for the first day of the last week (Monday).
     *
     * @return A Date object representing the first day of the last week.
     */
    public static Date getFirstDayOfWeek() {
        Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        int i = c.get(Calendar.DAY_OF_WEEK) - c.getFirstDayOfWeek();
        c.add(Calendar.DATE, -i - 7);
        return c.getTime();
    }

    /**
     * Calculates the difference in minutes between two dates.
     *
     * @param fromDate The start date.
     * @param toDate The end date.
     * @return The difference in minutes between the two dates.
     */
    public static long getDiffInMinutes(Date fromDate, Date toDate) {
        long difference = fromDate.getTime() - toDate.getTime();
        return TimeUnit.MILLISECONDS.toMinutes(difference) % Constants.SIXTY;
    }

    /**
     * Calculates the difference in hours between two dates.
     *
     * @param fromDate The start date.
     * @param toDate The end date.
     * @return The difference in hours between the two dates.
     *         If either of the provided dates is null, returns zero.
     */
    public static long getDiffInHours(Date fromDate, Date toDate) {
        if (null == fromDate || null == toDate) {
            return 0;
        }
        long difference = fromDate.getTime() - toDate.getTime();
        return TimeUnit.MILLISECONDS.toHours(difference) % Constants.TWENTY_FOUR;
    }

    /**
     * Returns the end of the day for a given timezone in ISO string format.
     *
     * @param timeZone The timezone for which the end of the day is to be calculated.
     * @return A string representing the end of the day in the given timezone in ISO format.
     */
    public static String getEndOfDay(String timeZone) {
        Calendar calendar = getCalendar(timeZone);
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        calendar.set(Calendar.MILLISECOND, 999);
        return getISOStringWithoutMillisecond(calendar);
    }

    /**
     * Returns a Calendar instance set to the current date and time in a given timezone.
     *
     * @param timeZone The timezone for which the Calendar instance is to be created.
     * @return A Calendar instance set to the current date and time in the given timezone.
     */
    public static Calendar getCalendar(String timeZone) {
        ZoneOffset zoneOffSet = ZoneOffset.of(timeZone);
        OffsetDateTime currentOffSetDateTime = OffsetDateTime.now(zoneOffSet);

        Date currentDate = Date.from(currentOffSetDateTime.toInstant());
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(currentDate);
        calendar.set(Calendar.ZONE_OFFSET, currentOffSetDateTime.getOffset().getTotalSeconds() * 1000);
        return calendar;
    }

    /**
     * Returns the start of the day for a given timezone in ISO string format.
     *
     * @param timeZone The timezone for which the start of the day is to be calculated.
     * @return A string representing the start of the day in the given timezone in ISO format.
     */
    public static String getStartOfDay(String timeZone) {
        Calendar calendar = getCalendar(timeZone);
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return getISOStringWithoutMillisecond(calendar);
    }

    /**
     * Converts a Calendar instance to an ISO string format.
     *
     * @param calendar The Calendar instance to be converted.
     * @return A string representing the date and time of the Calendar instance in ISO format.
     */
    public static String getISOString(Calendar calendar, String timezone) {
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_TIMEZONE);// Quoted "Z" to indicate UTC, no timezone// offset
        return calendar.getTime().toInstant().atZone(ZoneId.of(timezone)).format(dateTimeFormatter);
    }

    /**
     * Converts a Calendar instance to an ISO string format.
     *
     * @param calendar The Calendar instance to be converted.
     * @return A string representing the date and time of the Calendar instance in ISO format.
     */
    public static String getISOStringWithoutMillisecond(Calendar calendar) {
        DateFormat df = new SimpleDateFormat(Constants.DATE_FORMAT_WITHOUT_MILLISECOND); // Quoted "Z" to indicate UTC, no timezone
        // offset
        df.setTimeZone(TimeZone.getTimeZone(Constants.TIMEZONE_UTC));
        return df.format(calendar.getTime());
    }

    /**
     * Used to convert Date to String
     * @param date To convert the date
     * @return dateString
     */
    public static String convertDateToString(Date date) {
        DateFormat dateFormat = new SimpleDateFormat(Constants.JSON_DATE_FORMAT);
        dateFormat.setTimeZone(TimeZone.getTimeZone(Constants.TIMEZONE_UTC));
        return dateFormat.format(date);
    }

    /**
     * Adds a specified number of days to a given date.
     *
     * @param date The date to which days will be added.
     * @param daysToAdd The number of days to add to the date.
     * @return A Date object representing the new date after adding the specified number of days.
     */
    public static Date addDate(Date date, int daysToAdd) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.add(Calendar.DATE, daysToAdd);
        return cal.getTime();
    }

    /**
     * Calculates the difference between two dates in minutes.
     *
     * @param dateFrom The start date.
     * @param dateTo The end date.
     * @return The difference in minutes between the two dates.
     *         If either of the provided dates is null, returns zero.
     */
    public static long getDateDiffInMinutes(Date dateFrom, Date dateTo) {

        if (null == dateFrom || null == dateTo) {
            return 0;
        }

        long differenceInTime = dateTo.getTime() - dateFrom.getTime();

        long differenceInYears = TimeUnit.MILLISECONDS.toDays(differenceInTime) / 365;
        long differenceInDays = TimeUnit.MILLISECONDS.toDays(differenceInTime) % 365;
        long differenceInHours = TimeUnit.MILLISECONDS.toHours(differenceInTime) % 24;
        long differenceInMinutes = TimeUnit.MILLISECONDS.toMinutes(differenceInTime) % 60;

        return ((differenceInYears * 365 * 24 * 60) + (differenceInDays * 24 * 60) + (differenceInHours * 60)
                + differenceInMinutes);
    }

    /**
     * Calculates the next date for the treatment plan based on the provided period and duration.
     *
     * @param period The period to be used for the calculation. It can be "day", "week", or any other period.
     * @param duration The duration to be used for the calculation. It represents the number of periods.
     * @return A Date object representing the next date for the treatment plan.
     *         If the provided period is not recognized, returns null.
     */
    public static Date getTreatmentPlanFollowupDate(String period, Integer duration, Date assessmentdate) {
        Calendar calendar = Calendar.getInstance();
        if (!Objects.isNull(assessmentdate)) {
            calendar.setTime(assessmentdate);
        }
        Date date = null;
        if (Constants.DAY.equalsIgnoreCase(period)) {
            calendar.add(Calendar.DATE, duration);
            date = calendar.getTime();
        } else if (Constants.WEEK.equalsIgnoreCase(period)) {
            calendar.add(Calendar.WEEK_OF_YEAR, duration);
            date = calendar.getTime();
        } else {
            calendar.add(Calendar.MONTH, duration);
            date = calendar.getTime();
        }

        return date;
    }

    /**
     * Returns the start of the current day in UTC timezone in ISO string format.
     *
     * @return A string representing the start of the current day in UTC timezone in ISO format.
     */
    public static Date getStartOfDay() {
        Calendar calander = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        setStartOfTheDay(calander);
        return calander.getTime();
    }

    /**
     * Returns the end of the current day in UTC timezone in ISO string format.
     *
     * @return A string representing the end of the current day in UTC timezone in ISO format.
     */
    public static Date getEndOfDay() {
        Calendar calander = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        calander.set(Calendar.HOUR_OF_DAY, 23);
        calander.set(Calendar.MINUTE, 59);
        calander.set(Calendar.SECOND, 59);
        calander.set(Calendar.MILLISECOND, 999);
        return calander.getTime();
    }

    /**
     * Calculates the difference in years between the current date and the provided date.
     *
     * @param date The date from which to calculate the difference in years until now.
     * @return The difference in years between the current date and the provided date.
     */
    public static int getYearDiff(Date date) {
        LocalDate localDate = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        return Period.between(
            LocalDate.of(localDate.getYear(), localDate.getMonthValue(), localDate.getDayOfMonth()),
            LocalDate.now()).getYears();
    }

    /**
     * Calculates the number of weeks between the current date and the provided date.
     *
     * @param date The date from which to calculate the number of weeks until now.
     * @return The number of weeks between the current date and the provided date.
     */
    public static int getWeeks(Date date) {
        Calendar calendar = Calendar.getInstance();
        long currentTime = calendar.getTimeInMillis();
        calendar.setTime(date);
        long targetTime = calendar.getTimeInMillis();
        long diffInMillis = currentTime - targetTime;
        long weeks = diffInMillis / (1000 * 60 * 60 * 24 * 7);
        return (int) weeks;
    }

    /**
     * Adds a specified number of minutes to the current date.
     *
     * @param minutes The number of minutes to add to the current date.
     * @return A Date object representing the new date after adding the specified number of minutes to the current date.
     */
    public static Date addMinutesToCurrentDate(int minutes) {
        Date date = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.add(Calendar.MINUTE, minutes);
        return cal.getTime();
    }

    /**
     * Adds a specified number of days to the current date.
     *
     * @param days The number of days to add to the current date.
     * @return A Date object representing the new date after adding the specified number of days to the current date.
     */
    public static Date addDaysToCurrentDate(int days) {
        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.DAY_OF_MONTH, days);
        return calendar.getTime();
    }

    /**
     * Converts a Calendar instance to an ISO string format.
     *
     * @param calendar The Calendar instance to be converted.
     * @return A string representing the date and time of the Calendar instance in ISO format.
     */
    public static Date getISODate(Calendar calendar) {
        DateFormat df = new SimpleDateFormat(Constants.DATE_FORMAT_TIMEZONE); // Quoted "Z" to indicate UTC, no timezone
                                                                          // offset
        df.setTimeZone(TimeZone.getTimeZone(Constants.TIMEZONE_UTC));
        return calendar.getTime();
    }

    /**
     * Gets a date from yesterday's start time.
     *
     * @return A Date representing the date and time.
     */
    public static Date getYesterdayStartOfTheDay() {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.add(Calendar.DATE, -1);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }

    /**
     * Gets a date from yesterday's end time.
     *
     * @return A Date representing the date and time.
     */
    public static Date getYesterdayEndOfTheDay() {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        setEndOfTheDay(calendar);
        return calendar.getTime();

    }

    /**
     * Gets a date of the starting day of the week.
     *
     * @return A Date representing the date and time.
     */
    public static Date getStartDayOfWeek() {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        calendar.set(Calendar.DAY_OF_WEEK, 1);
        setEndOfTheDay(calendar);
        return calendar.getTime();
    }

    /**
     * Gets a date of the first day of the month.
     *
     * @return A Date representing the date and time.
     */
    public static Date getFirstDayOfMonth() {
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        setEndOfTheDay(calendar);
        return calendar.getTime();
    }


    /**
     * It helps the calendar instance to set the start time of the particular date.
     *
     */
    private static void setStartOfTheDay(Calendar calendar) {
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
    }

    /**
     * It helps the calendar instance to set the end time of the particular date.
     *
     */
    private static void setEndOfTheDay(Calendar calendar) {
        calendar.add(Calendar.DATE, -1);
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        calendar.set(Calendar.MILLISECOND, 999);
    }

    /**
     * Converts a date string to a timestamp in milliseconds.
     *
     * @param date a {@link java.util.Date} represented as a string
     * @return the corresponding timestamp in milliseconds as a {@link Long}, or {@code null} if the input is null
     */
    public static Long convertToTimestamp(String date) {
        if (date != null) {
            ZonedDateTime zonedDateTime = ZonedDateTime.parse(date, DateTimeFormatter.ISO_OFFSET_DATE_TIME);
            return zonedDateTime.toInstant().toEpochMilli();
        }
        return null;
    }
}
