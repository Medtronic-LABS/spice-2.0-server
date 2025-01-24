package com.mdtlabs.coreplatform.commonservice.common.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * This is a DTO class for MetaData DTO.
 * </p>
 *
 * @author Nandhakumar Created on Apr 05, 2024.
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
@NoArgsConstructor
public class MetaDataDTO implements Serializable {

    private Long id;

    private String name;

    private String type;

    private String category;

    private String diseaseType;

    private Integer displayOrder;

    private String description;
    
	private Map<String, Boolean> categories;

	private Long countryId;

	private Map<String, Object> cvdRiskAlgorithm;

    private List<MetaDataDTO> modelAnswers;

    private Object value;

    private boolean isDefault;

    private List<Map<String, Object>> answers;

    private boolean isAnswerDependent;

    private Integer duration;

    private String period;

    private String riskLevel;

    private boolean status;

    private Long parentComplianceId;

    private String workflow;

    private String formInput;

    private List<DiseaseConditionDTO> diseaseCondition;

    @JsonIgnore
    private Map<String, String> displayValues;

    private String displayValue;

    private List<String> meta;

    private List<String> metaForms;

    private boolean isChildExists;

    @JsonIgnore
    private Map<String, Object> jsonDisplayValues;

    private Object jsonDisplayValue;

    private List<Map<String, Object>> menus;

    private List<String> appTypes;
    
    private String gender;

    public MetaDataDTO(String category) {
        this.category = category;
    }

    public String getDisplayValue() {
        String dataName = this.name;
        if (CommonUtil.isCultureCodeNull() && !Objects.isNull(this.displayValues) && this.displayValues.containsKey(UserContextHolder.getUserDto().getCulture().getCode())) {
            dataName =  this.displayValues.get(UserContextHolder.getUserDto().getCulture().getCode());
        }
        return dataName;
    }

    public Object getMenus() {
        Object jsonName = this.menus;
        if (CommonUtil.isCultureCodeNull() && !Objects.isNull(this.menus) && !Objects.isNull(this.jsonDisplayValues) && this.jsonDisplayValues.containsKey(UserContextHolder.getUserDto().getCulture().getCode())) {
            jsonName =  this.jsonDisplayValues.get(UserContextHolder.getUserDto().getCulture().getCode());
        }
        return jsonName;
    }

    public Object getAnswers() {
        Object jsonName = this.answers;
        if (CommonUtil.isCultureCodeNull() && !Objects.isNull(this.answers) && !Objects.isNull(this.jsonDisplayValues) && this.jsonDisplayValues.containsKey(UserContextHolder.getUserDto().getCulture().getCode())) {
            jsonName =  this.jsonDisplayValues.get(UserContextHolder.getUserDto().getCulture().getCode());
        }
        return jsonName;
    }


}