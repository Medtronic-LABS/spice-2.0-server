package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.Calendar;
import java.util.Date;
import java.util.Objects;

import lombok.Data;

/**
 * This DTO class for Bio metrics details.
 *
 * @author Gokul A created on Aug 12, 2024
 */
@Data
public class BioMetricsDTO {

    private String gender;

    private Integer age;

    private Date dateOfBirth;

    private Double height;

    private Double weight;

    private Double bmi;

    private String bmiCategory;

    private Boolean isPregnant;

    private Boolean isRegularSmoker = false;

    private Boolean isphysicallyActive;

    private boolean isFamilyDiabetesHistory;

    private boolean isBeforeGestationalDiabetes;

    public Date getDateOfBirth() {
        if (Objects.isNull(this.dateOfBirth) && Objects.nonNull(this.age)) {
            Calendar calendar = Calendar.getInstance();
            calendar.add(Calendar.YEAR, -age);
            return calendar.getTime();
        }
        return this.dateOfBirth;
    }

    public Integer getAge() {
        if (Objects.isNull(this.age) && Objects.nonNull(this.dateOfBirth)) {
            Calendar birthCalendar = Calendar.getInstance();
            Calendar currentCalendar = Calendar.getInstance();
            birthCalendar.setTime(this.dateOfBirth);
            currentCalendar.setTime(new Date());
            this.age = currentCalendar.get(Calendar.YEAR) - birthCalendar.get(Calendar.YEAR);
            int currentMonth = currentCalendar.get(Calendar.MONTH);
            int birthMonth = birthCalendar.get(Calendar.MONTH);
            int currentDay = currentCalendar.get(Calendar.DAY_OF_MONTH);
            int birthDay = birthCalendar.get(Calendar.DAY_OF_MONTH);

            if (currentMonth < birthMonth ||
                    (currentMonth == birthMonth && currentDay < birthDay)) {
                this.age--;
            }
        }
        return this.age;
    }
}
