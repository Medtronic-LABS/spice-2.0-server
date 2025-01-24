package com.mdtlabs.coreplatform.userservice.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the organization module action in
 * database. In query annotation (nativeQuery = true) the below query perform
 * like SQL. Otherwise its perform like HQL default value for nativeQuery FALSE.
 * </p>
 *
 * @author Karthick Murugesan created on Feb 01, 2024
 */
@Repository
public interface OrganizationRepository extends JpaRepository<Organization, Long> {

    public static final String GET_ORGANIZATION = "select organization from Organization  as organization ";

    public static final String GET_ORGANIZATION_BY_ID = GET_ORGANIZATION + "where organization.id =:id ";

    String GET_ORGANIZATIONS_BY_FORM_NAME = "select org from Organization as org where formName =:formName and isDeleted = false and isActive = true";

    /**
     * ids
     * <p>
     * This method is used to get Organization by its ID that has not been deleted.
     * </p>
     *
     * @param id {@link Long} The id of the organization need to retrieve from the
     *           database is given
     * @return {@link Organization} The organization for the given id which is not
     * deleted is retrieved
     */
    Organization findByIdAndIsDeletedFalse(Long id);

    /**
     * This method is used to find a list of organizations that have the given parent organization ID and are not deleted.
     *
     * @param id The ID of the parent organization.
     * @return List<Organization> A list of organizations that have the given parent organization ID and are not deleted.
     */
    List<Organization> findByParentOrganizationIdAndIsDeletedFalse(Long id);

    /**
     * This method is used to find an organization by its name, form name, deletion status, and active status.
     *
     * @param name      The name of the organization.
     * @param formName  The form name of the organization.
     * @param isDeleted The deletion status of the organization.
     * @param isActive  The active status of the organization.
     * @return Organization The organization that matches the given parameters.
     */
    Organization findByNameIgnoreCaseAndFormNameAndIsDeletedAndIsActive(String name, String formName, boolean isDeleted, boolean isActive);

    /**
     * This method is used to find a list of organizations by their IDs, which are not deleted and are active.
     *
     * @param ids The IDs of the organizations.
     * @return List<Organization> A list of organizations that match the given parameters.
     */
    List<Organization> findByIdInAndIsDeletedFalseAndIsActiveTrue(List<Long> ids);

    /**
     * <p>
     * This method used to get the organization detail using id.
     * </p>
     *
     * @param id The id of the organization need to retrieve from the
     *           database is given
     * @return {@link Organization} The organization for the given id is retrieved
     */
    @Query(value = GET_ORGANIZATION_BY_ID)
    public Organization getOrganizationById(@Param(Constants.ID) long id);

    /**
     * <p>
     * This method is used to get the set of organizations with specified IDs
     * that have not been deleted for the given active status.
     * </p>
     *
     * @param isActive The active status of the organizations need to retrieve from the
     *                 database is given
     * @param idList   {@link List<Long>} The ids of the organizations need to retrieve from the
     *                 database is given
     * @return {@link List<Organization>} The set of organizations for the given list of
     * ids and given active status which are all not deleted is retrieved
     */
    public Set<Organization> findByIsDeletedFalseAndIsActiveAndIdIn(boolean isActive, List<Long> idList);

    /**
     * <p>
     * This method is used to get the list of organizations that have a parent organization ID in a given list of IDs.
     * </p>
     *
     * @param childOrgIds {@link List<Long>} The list of parentOrganizationIds of the
     *                    organizations need to retrieve from the
     *                    database is given
     * @return {@link List<Organization>} The list of organizations for the given
     * parent organization ids is retrieved
     */
    List<Organization> findByParentOrganizationIdIn(List<Long> childOrgIds);

    /**
     * <p>
     * Retrieves a list of organizations by the specified form name.
     * </p>
     *
     * @param formName the name of the form associated with the organizations
     * @return a list of organizations that match the specified form name
     */
    @Query(value = GET_ORGANIZATIONS_BY_FORM_NAME)
    List<Organization> getOrganizationsByFormName(@Param(Constants.FORM_NAME) String formName);

    /**
     * This method is used to find an organization by its name, form name
     *
     * @param name      The name of the organization.
     * @param formName  The form name of the organization.
     * @return {@link Organization} The organization that matches the given parameters.
     */
    Organization findByNameIgnoreCaseAndFormName(String name, String formName);
}
