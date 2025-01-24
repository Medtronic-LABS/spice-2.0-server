package com.mdtlabs.coreplatform.spiceservice.common.model;

import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

import java.util.List;

/**
 * This is entity class for frequency.
 * 
 * @author Karthick M Created on Jul 13, 2024.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_FREQUENCY)
public class Frequency extends MetaBaseEntity {

  private static final long serialVersionUID = 1L;

  @Column(name = FieldConstants.NAME)
  private String name;

  @Column(name = FieldConstants.TYPE)
  private String type;

  @Column(name = FieldConstants.DURATION)
  private Integer duration;

  @Column(name = FieldConstants.PERIOD)
  private String period;

  @Column(name = FieldConstants.RISK_LEVEL)
  private String riskLevel;

  @Column(name = FieldConstants.TITLE)
  private String title;

  @Column(name = FieldConstants.DISPLAY_ORDER)
  private Integer displayOrder;

  @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
  private List<String> appTypes;
}
