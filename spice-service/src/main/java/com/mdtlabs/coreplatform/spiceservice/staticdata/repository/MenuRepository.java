package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.Menu;

/**
 * <p>
 * This is the repository class for communicate link between server side and
 * database. This class used to perform all the role module action in database.
 * In query annotation (nativeQuery = true) the below query perform like SQL.
 * Otherwise its perform like HQL default value for nativeQuery FALSE
 * </p>
 */
@Repository
public interface MenuRepository extends JpaRepository<Menu, Long> {

    String GET_MENU = "SELECT menu FROM Menu as menu WHERE roleName =:name AND (countryId =:countryId OR countryId IS NULL)";

    /**
     * Finds menu by roleName.
     *
     * @param name
     * @return
     */
    Menu findByRoleNameAndIsActiveTrueAndIsDeletedFalse(String name);

    /**
     * Finds menu by roleName.
     *
     * @param roleNames
     * @return
     */
    List<Menu> findByRoleNameInAndIsActiveTrueAndIsDeletedFalse(List<String> roleNames);

    /**
     * Finds menu by roleName and countryId.
     *
     * @param name      the name of the role
     * @param countryId the ID of the country
     * @return a list of Menu objects that match the roleName and countryId criteria
     */
    @Query(GET_MENU)
    List<Menu> getMenuByRole(@Param("name") String name, @Param("countryId") Long countryId);
    
}
