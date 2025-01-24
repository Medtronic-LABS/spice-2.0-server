package com.mdtlabs.coreplatform.fhirmapper.common.utils;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;


/**
 * <p>
 * Common utils for data validation etc.
 * </p>
 *
 * @author JohnKennedy A created on Aug 05, 2024
 */
@Component
public class CommonUtils {

    private CommonUtils() {

    }

    public static String gestationalAge(Integer age) {
        return switch (age) {
            case 34, 35, 36 -> Constants.LATE_PRETERM;
            case 32, 33 -> Constants.MODERATE_PRETERM;
            case 28, 29, 30, 31 -> Constants.VERY_PRETERM;
            default -> {
                if (age < 28) yield Constants.EXTREMELY_PRETERM;
                if (age >= 37 && age <= 38) yield Constants.EARLY_TERM;
                if (age >= 39 && age <= 40) yield Constants.FULL_TERM;
                if (age == 41) yield Constants.LATE_TERM;
                if (age >= 42) yield Constants.POST_TERM;
                yield null;
            }
        };
    }

    public static String birthWeight(Double kg) {
        Integer grams = (int) (kg * 1000);
        return switch (grams) {
            case Integer g when g < 1000 -> Constants.ELBW;
            case Integer g when g < 1500 -> Constants.VLBW;
            case Integer g when g < 2500 -> Constants.LBW;
            case Integer g when g >= 2500 && g <= 4000 -> Constants.NBW;
            case Integer g when g > 4000 -> Constants.HBW;
            default -> null;
        };
    }

    /**
     * Retrieves the start and end dates for today, yesterday, and tomorrow in UTC time zone.
     *
     * @return a map containing the start and end dates for today, yesterday, and tomorrow in UTC time zone
     */
    public static Map<String, String> getTodayAndTomorrowDate() {
        HashMap<String, String> datesMap = new HashMap<>();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.ZONED_UTC_FORMAT);

        LocalDate today = LocalDate.now();
        ZonedDateTime zonedDateTime = today.atStartOfDay(ZoneId.systemDefault());
        ZonedDateTime zonedDateTimeUTC = zonedDateTime.withZoneSameInstant(ZoneId.of(Constants.TIMEZONE_UTC));
        datesMap.put(Constants.TODAY_START_DATE, zonedDateTimeUTC.format(formatter));

        zonedDateTime = zonedDateTime.plusDays(Constants.ONE).minusSeconds(Constants.ONE);
        zonedDateTimeUTC = zonedDateTime.withZoneSameInstant(ZoneId.of(Constants.TIMEZONE_UTC));
        datesMap.put(Constants.TODAY_END_DATE, zonedDateTimeUTC.format(formatter));

        LocalDate yesterday = today.minusDays(Constants.ONE);
        zonedDateTime = yesterday.atStartOfDay(ZoneId.systemDefault());
        zonedDateTimeUTC = zonedDateTime.withZoneSameInstant(ZoneId.of(Constants.TIMEZONE_UTC));
        datesMap.put(Constants.YESTERDAY_START_DATE, zonedDateTimeUTC.format(formatter));

        zonedDateTime = zonedDateTime.plusDays(Constants.ONE).minusSeconds(Constants.ONE);
        zonedDateTimeUTC = zonedDateTime.withZoneSameInstant(ZoneId.of(Constants.TIMEZONE_UTC));
        datesMap.put(Constants.YESTERDAY_END_DATE, zonedDateTimeUTC.format(formatter));

        LocalDate tomorrow = today.plusDays(Constants.ONE);
        zonedDateTime = tomorrow.atStartOfDay(ZoneId.systemDefault());
        zonedDateTimeUTC = zonedDateTime.withZoneSameInstant(ZoneId.of(Constants.TIMEZONE_UTC));
        datesMap.put(Constants.TOMORROW_START_DATE, zonedDateTimeUTC.format(formatter));

        zonedDateTime = zonedDateTime.plusDays(Constants.ONE).minusSeconds(Constants.ONE);
        zonedDateTimeUTC = zonedDateTime.withZoneSameInstant(ZoneId.of(Constants.TIMEZONE_UTC));
        datesMap.put(Constants.TOMORROW_END_DATE, zonedDateTimeUTC.format(formatter));

        return datesMap;
    }

    /**
     * Retrieves the start and end dates for today, yesterday, and tomorrow in the user's time zone.
     *
     * @return a map containing the start and end dates for today, yesterday, and tomorrow in the user's time zone
     */
    public static Map<String, String> getDatesUsingUserTimezone() {
        Map<String, String> dateFilter = new HashMap<>();
        String userTimezone = UserContextHolder.getUserDto().getTimezone().getOffset();
        dateFilter.put(Constants.TODAY_START_DATE,
                DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Boolean.FALSE));
        dateFilter.put(Constants.TODAY_END_DATE,
                DateUtil.getUserTimezoneTime(userTimezone, Constants.ZERO, Boolean.TRUE));
        dateFilter.put(Constants.YESTERDAY_START_DATE,
                DateUtil.getUserTimezoneTime(userTimezone, Constants.ONE, Boolean.FALSE));
        dateFilter.put(Constants.YESTERDAY_END_DATE,
                DateUtil.getUserTimezoneTime(userTimezone, Constants.ONE, Boolean.TRUE));
        dateFilter.put(Constants.TOMORROW_START_DATE,
                DateUtil.getUserTimezoneTime(userTimezone, Constants.NEGATIVE_ONE, Boolean.FALSE));
        dateFilter.put(Constants.TOMORROW_END_DATE,
                DateUtil.getUserTimezoneTime(userTimezone, Constants.NEGATIVE_ONE, Boolean.TRUE));
        return dateFilter;
    }


}
