package com.mdtlabs.coreplatform.adminservice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.CountryCustomization;

/**
 * <p>
 *   This Repository class contains the needed customized functions for country
 *   customization.
 * </p>
 *
 * @author Karthick M created on Jun 30, 2022
 */
@Repository
public interface CountryCustomizationRepository extends JpaRepository<CountryCustomization, Long> {

    public static final String GET_COUNTRY_CUSTOMIZATION_WITH_CONDITIONS = "SELECT countryCustomization "
        + " FROM CountryCustomization AS countryCustomization "
        + " WHERE (countryCustomization.countryId = :countryId) "
        + " AND (:category IS NULL OR countryCustomization.category = :category) "
        + " AND (:type IS NULL OR upper(countryCustomization.type) = upper(:type)) "
        + " AND countryCustomization.isDeleted= :isDeleted AND countryCustomization.tenantId = :tenantId"
        + " AND (:cultureId is null or countryCustomization.cultureId=:cultureId)";

    /**
     * <p>
     *   To get a country customization details with conditions.
     * </p>
     *
     * @param countryId {@link Long} Country id
     * @param category {@link String} Category
     * @param type {@link String} Type
     * @param isDeleted True or false
     * @return {@link CountryCustomization} CountryCustomization entity
     */
    @Query(value = GET_COUNTRY_CUSTOMIZATION_WITH_CONDITIONS)
    public List<CountryCustomization> findByCountryIdAndCategoryAndTypeAndTenantId(
    	@Param("countryId") Long countryId, @Param("category") String category,
        @Param("type") String type, @Param("isDeleted") boolean isDeleted, @Param("tenantId") Long tenantId, @Param("cultureId") Long cultureId);

    /**
     * <p>
     *   Gets list of countryCustomization by list of categories and list of types.
     * </p>
     *
     * @param countryConsentFormTypes {@link List} List of customization categories
     * @param countryCustomizationTypes {@link List} List of country customization types
     * @return {@link List} List of CountryCustomization entity
     */
    public List<CountryCustomization> findByCategoryInAndTypeInAndCountryId(
    	List<String> countryConsentFormTypes, List<String> countryCustomizationTypes, Long countryId);

    /**
     * <p>
     *   Gets list of CountryCustomization by isDefault.
     * </p>
     *
     * @return {@link List} List of CountryCustomization entity
     */
    public List<CountryCustomization> findByIsDefaultTrue();

    /**
     * <p>
     *   Gets a country customization by Id.
     * </p>
     *
     * @param id {@link Long} country customization id
     * @return {@link CountryCustomization} CountryCustomization entity
     */
    public CountryCustomization findByIdAndTenantIdAndIsDeletedFalse(Long id, Long tenantId);

    /**
     * <p>
     * Gets list of CountryCustomization by countryId and cultureId.
     * </p>
     *
     * @param countryId
     * @param cultureId
     * @return
     */
    public List<CountryCustomization> findByCountryIdAndCultureId(Long countryId, Long cultureId);

    /**
     * <p>
     * Gets list of CountryCustomization by countryId and cultureId.
     * </p>
     *
     * @param countryId
     * @param cultureId
     * @param Category
     * @return
     */
    public List<CountryCustomization> findByCountryIdAndCultureIdAndCategory(Long countryId, Long cultureId, String Category);

}
