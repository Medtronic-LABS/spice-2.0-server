package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

import java.util.List;

@Data
@Entity
@Table(name = TableConstants.TABLE_TIMEZONE)
public class Timezone extends BaseEntity {

	@Column(name = FieldConstants.ABBREVIATION)
	private String abbreviation;

	@Column(name = FieldConstants.DESCRIPTION)
	private String description;

	@Column(name = FieldConstants.OFFSET)
	private String offset;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;

    public Timezone(Long id) {
        super(id);
    }

    public Timezone() {
        super();
    }

}
