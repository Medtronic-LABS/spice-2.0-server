package com.mdtlabs.coreplatform.authservice.common;

import static org.mockito.Mockito.mockStatic;

import java.time.LocalDate;
import java.util.Date;

import org.mockito.MockedStatic;

import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;

public class TestCommonMethods {

    private static MockedStatic<DateUtil> dateUtil;

    public static void init() {
        dateUtil = mockStatic(DateUtil.class);
    }

    public static void getStaticMock() {
        LocalDate localDate = LocalDate.of(2023, 3, 8);
        Date date = java.sql.Date.valueOf(localDate);
        LocalDate currentLocalDate = LocalDate.of(2023, 3, 9);
        Date currentDate = java.sql.Date.valueOf(currentLocalDate);

        dateUtil.when(() -> DateUtil.formatDate(date)).thenReturn(date);
        dateUtil.when(() -> DateUtil.formatDate(new Date())).thenReturn(currentDate);
        dateUtil.when(() -> DateUtil.getDiffInHours(date, currentDate)).thenReturn(4L);
    }

    public static void cleanUp() {
        dateUtil.close();
    }
}
