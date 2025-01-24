package com.mdtlabs.coreplatform.fhirmapper.common.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import lombok.Data;

import com.mdtlabs.coreplatform.fhirmapper.common.Constants;

/**
 * This DTO class for Treatment plan.
 *
 * @author Karthick M created on Aug 13, 2024
 */
@Data
public class TreatmentPlanDTO {

    private String memberReference;

    private String patientReference;

    private ProvenanceDTO provenance;

    private List<FrequencyDTO> frequencies;

    private String cvdRiskLevel;

    private boolean isPregnancyAnc;

    private boolean isHba1c;

    private boolean isBGDefaultFrequency;

    private FrequencyDTO medicalReviewFrequency;

    private FrequencyDTO bpCheckFrequency;

    private FrequencyDTO bgCheckFrequency;

    private FrequencyDTO hba1cCheckFrequency;

    private FrequencyDTO choCheckFrequency;

    private String carePlanId;

    private Date nextMedicalReviewDate;

    private Date nextBpAssessmentDate;

    private Date nextBgAssessmentDate;

    public List<FrequencyDTO> getMedicalReviewFrequencies() {
        List<FrequencyDTO> frequencyList = new ArrayList<>();
        if (Objects.nonNull(this.medicalReviewFrequency)) {
            frequencyList.add(this.getMedicalReviewFrequency());
        }
        if (Objects.nonNull(this.bpCheckFrequency)) {
            frequencyList.add(this.getBpCheckFrequency());
        }
        if (Objects.nonNull(this.bgCheckFrequency)) {
            frequencyList.add(this.getBgCheckFrequency());
        }
        if (Objects.nonNull(this.hba1cCheckFrequency)) {
            frequencyList.add(this.getHba1cCheckFrequency());
        }
        if (Objects.nonNull(this.choCheckFrequency)) {
            frequencyList.add(this.getChoCheckFrequency());
        }
        return frequencyList;
    }

    public FrequencyDTO getMedicalReviewFrequency() {
        if (!Objects.isNull(this.medicalReviewFrequency) && Constants.TREATMENT_PLAN_DEFAULT.equals(this.medicalReviewFrequency.getType())) {
            this.medicalReviewFrequency.setType(Constants.FREQUENCY_MEDICAL_REVIEW);
        }
        return medicalReviewFrequency;
    }

    public FrequencyDTO getBpCheckFrequency() {
        if (!Objects.isNull(this.bpCheckFrequency) && Constants.TREATMENT_PLAN_DEFAULT.equals(this.bpCheckFrequency.getType())) {
            this.bpCheckFrequency.setType(Constants.FREQUENCY_BP_CHECK);

        }
        return bpCheckFrequency;
    }

    public FrequencyDTO getBgCheckFrequency() {
        if (!Objects.isNull(this.bgCheckFrequency) && Constants.TREATMENT_PLAN_DEFAULT.equals(this.bgCheckFrequency.getType())) {
            this.bgCheckFrequency.setType(Constants.FREQUENCY_BG_CHECK);
        }
        return bgCheckFrequency;
    }

    public FrequencyDTO getHba1cCheckFrequency() {
        if (!Objects.isNull(this.hba1cCheckFrequency) && Constants.TREATMENT_PLAN_DEFAULT.equals(this.hba1cCheckFrequency.getType())) {
            this.hba1cCheckFrequency.setType(Constants.FREQUENCY_HBA1C_CHECK);
        }
        return hba1cCheckFrequency;
    }

    public FrequencyDTO getChoCheckFrequency() {
        if (!Objects.isNull(this.choCheckFrequency) && Constants.TREATMENT_PLAN_DEFAULT.equals(this.choCheckFrequency.getType())) {
            this.choCheckFrequency.setType(Constants.FREQUENCY_CHO_CHECK);
        }
        return choCheckFrequency;
    }
}


