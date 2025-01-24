package com.mdtlabs.coreplatform.spiceservice.common.model;


import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;


/**
 * <p>
 * Message class represents a message entity with name, category, and type attributes.
 * </p>
 *
 * @author Divya S created on May 30, 2023
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_MESSAGE)
public class Message extends MetaBaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.CATEGORY)
    private String category;

    @Column(name = FieldConstants.TYPE)
    private String type;
}
