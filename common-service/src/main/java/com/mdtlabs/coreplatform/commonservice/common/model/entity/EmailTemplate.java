/*
 * Copyright (C) 2008 Ideas2IT Technologies Pvt Ltd. (http://www.ideas2it.com/).
 */
package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

import lombok.Data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import java.io.Serializable;
import java.util.List;

/**
 * The persistent class for the Email Template table in the database.
 * 
 * @author vigneshKumar created on Feb 19, 2019
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_EMAIL_TEMPLATE)
public class EmailTemplate extends BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.TYPE)
    private String type;

    @Column(name = FieldConstants.VM_CONTENT)
    private String vmContent;

    @Column(name = FieldConstants.SUBJECT)
    private String subject;

    @Column(name = FieldConstants.BODY)
    private String body;

    @Column(name = FieldConstants.TITLE)
    private String title;

    @Column(name = FieldConstants.APP_URL)
    private String appUrl;

    @Column(name = FieldConstants.APP_TYPE)
    private String appType;

    @OneToMany
    @JoinColumn(name = FieldConstants.EMAIL_TEMPLATE_ID)
    private List<EmailTemplateValue> emailTemplateValues;

}
