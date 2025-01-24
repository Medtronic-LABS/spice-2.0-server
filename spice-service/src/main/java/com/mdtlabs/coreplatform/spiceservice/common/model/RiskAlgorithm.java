package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.Data;
import org.hibernate.annotations.Type;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.util.Map;

/**
 * This is entity class for risk algorithm.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_RISK_ALGORITHM)
public class RiskAlgorithm extends BaseEntity {

    private static final long serialVersionUID = 1L;

	@Column(name = FieldConstants.RISK_ALGORITHM, columnDefinition = "jsonb")
	@Type(value = JsonBinaryType.class)
	private Map<String, Object> cvdRiskAlgorithm;

	@Column(name = FieldConstants.COUNTRY_ID)
	private Long countryId;

}
