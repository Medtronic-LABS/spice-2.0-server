package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

/**
 * <p>
 * This is a DTO class to show the patient count details.
 * </p>
 *
 * @author shrikanth Created on September 17, 2024.
 */
@Data
public class DashboardDetails {

    private long screened;

    private long assessed;

    private long registered;

    private long referred;

    private long dispensed;

    private long investigated;

    private long nutritionistLifestyleCount;

    private long psychologicalNotesCount;
}
