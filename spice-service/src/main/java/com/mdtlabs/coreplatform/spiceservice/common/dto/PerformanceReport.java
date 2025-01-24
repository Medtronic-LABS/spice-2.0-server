package com.mdtlabs.coreplatform.spiceservice.common.dto;

import lombok.Data;

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

    private String userId;

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

    public void setFollowUpDueVisit() { this.followUpDueVisit++; }

    public void setFollowUpDueCalls() {
        this.followUpDueCalls++;
    }

    public void setFollowUpCondVisit() {
        this.followUpCondVisit++;
    }

    public void setFollowUpCondCalls() { this.followUpCondCalls++; }

}
