package com.mdtlabs.coreplatform.commonservice.common.model.entity;

import java.util.HashSet;
import java.util.Set;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.Table;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

import com.mdtlabs.coreplatform.commonservice.common.FieldConstants;
import com.mdtlabs.coreplatform.commonservice.common.TableConstants;

/**
 * <p>
 * A CommunityUnit is a core entity that represents a specific community within a larger region or country.
 * It can be associated with multiple villages, indicating the geographical areas it covers.
 * </p>
 *
 * @author Divya created on Jul 15, 2024
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_COMMUNITY_UNIT)
@NoArgsConstructor
public class CommunityUnit extends BaseEntity {

    @Column(name = FieldConstants.NAME)
    private String name;

    @Column(name = FieldConstants.PARENT_REGION_ID)
    private Long parentRegionId;

    @Column(name = FieldConstants.COUNTRY_ID)
    private Long countryId;

    @ManyToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SELECT)
    @JoinTable(name = TableConstants.TABLE_COMMUNITY_UNIT_VILLAGE, joinColumns = {
            @JoinColumn(name = FieldConstants.COMMUNITY_UNIT_ID)}, inverseJoinColumns = {
            @JoinColumn(name = FieldConstants.VILLAGE_ID)})
    private Set<Village> villages = new HashSet<>();
}
