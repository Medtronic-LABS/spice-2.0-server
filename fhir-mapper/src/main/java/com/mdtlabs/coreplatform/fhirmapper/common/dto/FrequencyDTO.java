package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

/**
 * This DTO class for frequency details.
 *
 * @author Karthick M created on Aug 13, 2024
 */
@Data
public class FrequencyDTO implements Cloneable {

    private String name;

    private String type;

    private Integer duration;

    private String period;

    private String riskLevel;

    private String title;

    private Integer displayOrder;

    @Override
    public FrequencyDTO clone() {
        try {
            return (FrequencyDTO) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }
}
