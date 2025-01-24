package com.mdtlabs.coreplatform.spiceservice.common.dto;


import lombok.Data;

/**
 * This FormMetaDTO contains the necessary data fields to represent form metadata,
 * facilitating its transfer between the server and the client in a structured format.
 * It helps in decoupling the server-side form metadata representation from the client,
 * enabling seamless communication and interaction between the two.
 */
@Data
public class FormMetaDTO {

    private String formInput;
    private String formType;
    private String workflowName;
    private Long clinicalWorkflowId;

    public FormMetaDTO(String formInput, String formType) {
        this.formInput = formInput;
        this.formType = formType;
    }

    public FormMetaDTO() {
    }

}
