package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

@Data
@Entity
@Table(name = "user_token")
public class UserToken extends BaseEntity implements Serializable {
    
    @Column(name = "user_id")
    private Long userId;

    @Column(name = "last_logged_in")
    private Date lastLoggedIn;

    @Column(name = "last_logged_out")
    private Date lastLoggedOut;

    @Column(name = "client")
    private String client;

    @Column(name = "auth_token")
    private String authToken;

    @Column(name = "app_version")
    private String appVersion;

}
