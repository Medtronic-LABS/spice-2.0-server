package com.mdtlabs.coreplatform.spiceservice.common.model;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hibernate.annotations.Type;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.BaseEntity;
import com.mdtlabs.coreplatform.spiceservice.common.FieldConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TableConstants;
import com.vladmihalcea.hibernate.type.array.ListArrayType;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

/**
 * This entity class for Menu.
 */
@Data
@Entity
@Table(name = TableConstants.TABLE_MENU)
public class Menu extends BaseEntity {

    @Column(name = FieldConstants.ROLE_NAME)
    private String roleName;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.MENU, columnDefinition = "jsonb")
    private List<Map<String, Object>> menus;

    @Type(value = ListArrayType.class)
    @Column(name = FieldConstants.META, columnDefinition = "varchar[]")
    private List<String> meta;

    @Column(name = FieldConstants.META_FORMS, columnDefinition = "varchar[]")
    private List<String> metaForms;

    @Type(value = JsonBinaryType.class)
    @Column(name = FieldConstants.JSON_DISPLAY_VALUES)
    private Map<String, List<Map<String, Object>>> jsonDisplayValues;

    @Column(name =  FieldConstants.COUNTRY_ID)
    private Long countryId;

    @Column(name = FieldConstants.APP_TYPES, columnDefinition = "text[]")
    private List<String> appTypes;

    public List<Map<String, Object>> getMenus() {
        List<Map<String, Object>> menus = this.menus;
        if (CommonUtil.isCultureCodeNull()) {
            if (!Objects.isNull(this.menus) && Objects.nonNull(this.jsonDisplayValues)
                    && this.jsonDisplayValues.containsKey(UserContextHolder.getUserDto().getCulture().getCode())) {
                menus = this.jsonDisplayValues.get(UserContextHolder.getUserDto().getCulture().getCode());
            }
        }
        return menus;
    }
}
