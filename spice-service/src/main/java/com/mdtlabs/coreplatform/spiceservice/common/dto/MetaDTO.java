package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.ClinicalWorkflow;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;

import lombok.Data;

/**
 * This dto class for getting meta elements.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MetaDTO {

    private List<FormMetaDTO> formData;

    private List<ClinicalWorkflow> clinicalTools;

    private List<Symptom> symptoms;

    private NCDFromDTO enrollment;
    
    private NCDFromDTO screening;
    
    private NCDFromDTO assessment;

    private List<Map<String, Object>> customizedWorkflow;

    private List<Map<String, String>> modelQuestions;


    public MetaDTO(List<FormMetaDTO> formData, List<ClinicalWorkflow> clinicalTools) {
        this.formData = formData;
        this.clinicalTools = clinicalTools;
    }

    public MetaDTO() {
    }


    @Data
    public static class NCDFromDTO {
    
        private String inputForm;

        private String consentForm;

        public void setForms(CountryCustomization countryCustomization) {
            if (Constants.INPUT_FORM.equals(countryCustomization.getCategory())) {
                this.inputForm = countryCustomization.getFormInput();
            } else if (Constants.CONSENT_FORM.equals(countryCustomization.getCategory())) {
                this.consentForm = countryCustomization.getFormInput();
            }
        }
    }

    
}
