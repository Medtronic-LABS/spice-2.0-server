package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.springframework.security.core.GrantedAuthority;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.Data;

@Data
@Entity
@Table(name = TableConstants.TABLE_ROLE)
public class Role extends BaseEntity implements Serializable, GrantedAuthority {

    @Column(name =  FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.LEVEL)
    private Long level;

	@Column(name = FieldConstants.SUITE_ACCESS_NAME)
	private String suiteAccessName;

	@Column(name = FieldConstants.DISPLAY_NAME)
	private String displayName;

	@Column(name = FieldConstants.ROLE_GROUP_NAME)
	private String groupName;

	@OneToMany(fetch = FetchType.EAGER)
    @Fetch(FetchMode.SELECT)
    @JoinColumn(name = FieldConstants.ROLE_ID)
	private List<CountryRole> countryRoles;

	@Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
	private List<String> appTypes;

    /** 
	 * @return the name property (getAuthority required by Acegi's GrantedAuthority
	 *         interface)
	 * @see org.springframework.security.core.GrantedAuthority#getAuthority()
	 */
	@Transient
	private String authority;

	public String getAuthority() {
		return getName();
	}

	@Override
	public boolean equals(Object object) {
		if (this == object)
			return true;
		if (!(object instanceof Role))
			return false;
		Role role = (Role) object;
		return !Objects.isNull(this.getId()) && this.getId() == role.getId();
	}

	public Role(Long id, String name) {
		super(id);
		this.name = name;
	}

	public Role() {}

	public Role(Long id, String name, Long level) {
		super(id);
		this.name = name;
		this.level = level;
	}

	public Role(Long id, String name, Long level, String suiteAccessName, List<String> appTypes) {
		super(id);
		this.name = name;
		this.level = level;
		this.suiteAccessName = suiteAccessName;
		this.appTypes = appTypes;
	}


	@Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(authority, level, name);
        return result;
    }

	public String getDisplayName() {
		if (!Objects.isNull(UserContextHolder.getUserDto()) && !Objects.isNull(this.countryRoles) && !this.countryRoles.isEmpty() && Objects.nonNull(UserContextHolder.getUserDto().getCountry())) {
			return this.countryRoles.stream().filter(countryRole -> countryRole.getCountryId().equals(UserContextHolder.getUserDto().getCountry().getId())).findFirst().get().getDisplayName();
		}
		return this.displayName;
	}  
}
