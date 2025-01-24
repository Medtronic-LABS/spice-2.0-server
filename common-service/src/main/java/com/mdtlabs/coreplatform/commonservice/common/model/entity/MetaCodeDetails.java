package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This is a entity class for Metacode details.
 * 
 * @author Karthick M created On 05 Mar 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_META_CODE_DETAILS)
public class MetaCodeDetails extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = FieldConstants.ID)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Type(value = JsonBinaryType.class)
	@Column(name = FieldConstants.CODES, columnDefinition = "jsonb")
    private List<Code> codes; 

    @Column(name = FieldConstants.TEXT)
    private String text;

    @Data
    public static class Code implements Serializable{

        private String code;

        private String system;

        private String display; 
    } 

}

