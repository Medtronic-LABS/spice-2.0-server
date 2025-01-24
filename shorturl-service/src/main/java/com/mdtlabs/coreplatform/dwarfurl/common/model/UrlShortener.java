package com.mdtlabs.coreplatform.dwarfurl.common.model;

import com.mdtlabs.coreplatform.dwarfurl.common.FieldConstants;
import jakarta.persistence.*;
import lombok.Data;

import java.util.Date;


/**
 * <p>
 * The UrlShortener class is a model class for URL shortening related operations.
 * This class is mapped to the "url_shortener" table in the database.
 * </p>
 *
 * @author JohnKennedy Created on 04 sep 2024
 */
@Data
@Entity
public class UrlShortener {

    @Id
    @Column(name = FieldConstants.ID)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = FieldConstants.URL)
    private String url;

    @Column(name = FieldConstants.TOKEN)
    private String token;

    @Column(name = FieldConstants.APP)
    private String app;

    @Column(name = FieldConstants.ENV)
    private String env;

    @Column(name = FieldConstants.VISITS)
    private int visits;

    @Column(name = FieldConstants.IS_ACTIVE)
    private boolean isActive;

    @Column(name = FieldConstants.CREATED_AT)
    private Date createdAt;

    @Column(name = FieldConstants.UPDATED_AT)
    private Date updatedAt;

    @Column(name = FieldConstants.EXPIRES_AT)
    private Date expiresAt;

    public UrlShortener() {
    }

}
