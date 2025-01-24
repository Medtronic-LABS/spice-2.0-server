package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import lombok.Data;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

/**
 * This is a DTO class for storing Performance Monitoring request data.
 *
 * @author Nandhakumar created on July 22, 2024.
 */
@Data
public class PerformanceReport {

    private String villageId;

    private String villageName;

    private String chwName;

    private int household;

    private int iccm;

    private int otherSymptoms;

    private int rmnch;

    private int householdMember;

    private int referred;

    private int recovered;

    private int onTreatment;

    private int worsened;

    private int followUpDueVisit;

    private int followUpDueCalls;

    private int followUpCondVisit;

    private int followUpCondCalls;

    public void incrementFollowUpCondVisit() {
        this.followUpCondVisit++;
    }
    
    public void incrementHousehold() {
        this.household++;
    }

    public void incrementEncounterType(String encounterType) {
        if (Constants.ICCM.equals(encounterType)) {
            this.iccm++;
        } else if (Constants.OTHER_SYMPTOMS.equals(encounterType)) {
            this.otherSymptoms++;
        } else if (Constants.RMNCH_ASSESSMENTS.contains(encounterType)) {
            this.rmnch++;
        }
    }

    public void incrementTicketStatus(String status) {
        if (Constants.REFERRED.equals(status)) {
            this.referred++;
        } else if (Constants.ON_TREATMENT.equals(status)) {
            this.onTreatment++;
        } else if (Constants.RECOVERED.equals(status)) {
            this.recovered++;
        } else if (Constants.WORSENED.equals(status)) {
            this.worsened++;
        }
    }

    public void incrementHouseholdMember() {
        this.householdMember++;
    }

}
