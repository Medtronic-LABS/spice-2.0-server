package com.mdtlabs.coreplatform.spiceservice.customizedmodules.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.mdtlabs.coreplatform.spiceservice.common.model.CustomizedModule;


/**
 * <p>
 * This interface is used for maintaining connection between server and database
 * for CustomizedModules Entity.
 * </p>
 *
 * @since Oct 24, 2022
 * @author Divya S
 */
public interface CustomizedModuleRepository extends JpaRepository<CustomizedModule, Long> {

    /**
     * <p>
     * Get list of CustomizedModule by member ID.
     * </p>
     *
     * @param memberId The member ID of the patient is given
     * @return The list of CustomizedModule entity is returned
     */
    List<CustomizedModule> findByMemberIdAndPatientIdIsNullAndIsActiveTrueAndIsDeletedFalse(String memberId);

    /**
     * <p>
     * Get list of CustomizedModule by member ID.
     * </p>
     *
     * @param memberId The member ID of the patient is given
     * @return The list of CustomizedModule entity is returned
     */
    List<CustomizedModule> findByMemberIdAndIsActiveTrueAndIsDeletedFalse(String memberId);
}
