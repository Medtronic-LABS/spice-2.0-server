package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;
import lombok.Data;
import org.hibernate.annotations.ColumnTransformer;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.CustomAuthorityDeserializer;
import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;


@Data
@Entity
@Table(name = TableConstants.TABLE_USER)
public class User extends TenantBaseEntity implements Serializable, UserDetails {

    private static final long serialVersionUID = 1L;

    @Column(name = FieldConstants.FIRST_NAME)
    private String firstName;

    @Column(name = FieldConstants.MIDDLE_NAME)
    private String middleName;

    @Column(name = FieldConstants.LAST_NAME)
    private String lastName;

    @Column(name = FieldConstants.GENDER)
    private String gender;

    @Column(name = FieldConstants.PHONE_NUMBER)
    private String phoneNumber;

    @Column(name = FieldConstants.USERNAME)
    private String username;

    @Column(name = FieldConstants.FHIR_ID)
    private String fhirId;

    @ColumnTransformer(forColumn = "", read = "public.pgp_sym_decrypt(password::bytea, " + "'"
        + Constants.CORE_PLATFORM + "'"
        + ")", write = "public.pgp_sym_encrypt(?, " + "'" + Constants.CORE_PLATFORM + "'" + ")")
    @Column(name = FieldConstants.PASSWORD, columnDefinition  = "bytea")
    private String password;

    @Column(name = FieldConstants.COUNTRY_CODE)
    private String countryCode;

    @Column(name = FieldConstants.IS_BLOCKED)
    private Boolean isBlocked = false;

    @Column(name = FieldConstants.BLOCKED_DATE, columnDefinition = "TIMESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date blockedDate;

    @Column(name = FieldConstants.FORGET_PASSWORD_TOKEN)
    private String forgetPasswordToken;

    @Column(name = FieldConstants.FORGET_PASSWORD_SHORT_TOKEN)
    private String forgetPasswordShortToken;

    @Column(name = FieldConstants.FORGET_PASSWORD_TIME, columnDefinition = "TIMESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date forgetPasswordTime;

    @Column(name = FieldConstants.FORGET_PASSWORD_COUNT)
    private int forgetPasswordCount = 0;

    @Column(name = FieldConstants.INVALID_LOGIN_TIME, columnDefinition = "TIMESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date invalidLoginTime;

    @Column(name = FieldConstants.INVALID_LOGIN_ATTEMPTS)
    private int invalidLoginAttempts = 0;

    @Column(name = FieldConstants.IS_PASSWORD_RESET_ENABLED)
    private Boolean isPasswordResetEnabled = false;

    @Column(name = FieldConstants.PASSWORD_RESET_ATTEMPTS)
    private int passwordResetAttempts = 0;
    
    @Column(name = FieldConstants.LAST_LOGGED_IN)
    private Date lastLoggedIn;

    @ManyToOne
    @JoinColumn(name = FieldConstants.COUNTRY_ID)
    private Country country;

    @ManyToOne
    @JoinColumn(name = FieldConstants.TIMEZONE_ID)
    private Timezone timezone;

    @Column(name = FieldConstants.SUITE_ACCESS, columnDefinition = "varchar[]")
    private Set<String> suiteAccess = new HashSet<>();

    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_USER_ORGANIZATION, joinColumns = {
        @JoinColumn(name = FieldConstants.USER_ID) }, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.ORGANIZATION_ID) })
    private Set<Organization> organizations = new HashSet<>();

    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_USER_ROLE, joinColumns = {
        @JoinColumn(name = FieldConstants.USER_ID) }, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.ROLE_ID) })
    private Set<Role> roles = new HashSet<>();

    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_USER_VILLAGE, joinColumns = {
        @JoinColumn(name = FieldConstants.USER_ID) }, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.VILLAGE_ID) })
    private List<Village> villages = new ArrayList<>();

    @Column(name = FieldConstants.COMMUNITY_UNIT_ID)
    private Long communityUnitId;

    @ManyToOne
    @JoinColumn(name = FieldConstants.CULTURE_ID)
    private Culture culture;
    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_INSIGHT_USER_ORGANIZATION, joinColumns = {
            @JoinColumn(name = FieldConstants.USER_ID) }, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.ORGANIZATION_ID) })
    private List<Organization> insightUserOrganization = new ArrayList<>();

    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_REPORT_USER_ORGANIZATION, joinColumns = {
            @JoinColumn(name = FieldConstants.USER_ID) }, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.ORGANIZATION_ID) })
    private List<Organization> reportUserOrganization = new ArrayList<>();

    @Column(name = FieldConstants.INSIGHT_ID)
    private Integer insightId;

    @Column(name = FieldConstants.IS_TERMS_AND_CONDITION_ACCEPTED)
    private Boolean isTermsAndConditionsAccepted = false;
    
    @ManyToOne
    @JoinColumn(name = FieldConstants.DESIGNATION_ID)
    private Designation designation;

    @Override
    @Transient
    @JsonDeserialize(using = CustomAuthorityDeserializer.class)
    public Set<GrantedAuthority> getAuthorities() {
        Set<GrantedAuthority> authorities = new LinkedHashSet<>();
        authorities.addAll(roles);
        return authorities;
    }

    @Transient
    private String shortenUrl;

    @Override
    public boolean isAccountNonExpired() {
        return false;
    }

    @Override
    public boolean isAccountNonLocked() {
        return false;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return false;
    }

    @Override
    public boolean isEnabled() {
        return super.isActive();
    }

    public void setEnabled(boolean isActive) {
        super.setActive(isActive);
    }

    public User(Long id) {
        super(id);
    }

    /**
     * Default Constructor for User Entity.
     */
    public User() {

    }
}
