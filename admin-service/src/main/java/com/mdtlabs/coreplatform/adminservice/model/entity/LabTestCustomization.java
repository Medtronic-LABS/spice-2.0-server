package com.mdtlabs.coreplatform.adminservice.model.entity;

import java.io.Serial;
import java.io.Serializable;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;

import lombok.Data;
import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.adminservice.constants.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.TenantBaseEntity;

/**
 * <p>
 * Entity class for LabTestCustomization.
 * </p>
 *
 * @author Ragul Venkatesan
 * @since Jun 20, 2024
 */
@Data
@Entity
public class LabTestCustomization extends TenantBaseEntity {

    @Serial
    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.UNIQUE_NAME)
    private String uniqueName;

    @Column(name = FieldConstants.TEST_NAME)
    private String testName;

    @Column(name = FieldConstants.FORM_INPUT)
    private String formInput;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.CODES, columnDefinition = "jsonb")
    private Code codes;

    @Data
    public static class Code implements Serializable {

        private String code;

        private String system;

        private String display;
    }
}
