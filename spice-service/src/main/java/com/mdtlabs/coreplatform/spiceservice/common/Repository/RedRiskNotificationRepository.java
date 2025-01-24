package com.mdtlabs.coreplatform.spiceservice.common.Repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.RedRiskNotification;

import java.util.List;

/**
 * This is the repository which acts as link between server side and database.
 * This class is used to perform all the complaints module action in database.
 * In query annotation (nativeQuery = true), the below query perform like SQL.
 * Otherwise it performs like HQL.
 *
 *@since Aug 21, 2024
 *@author Tamilarasi Shanmugasundaram
 */
@Repository
public interface RedRiskNotificationRepository extends JpaRepository<RedRiskNotification, Long> {

    /**
     * <p>
     * This function finds RedRiskNotifications by patient Id.
     * </p>
     *
     * @param patientId - patient id is a long type variable that represents the unique
     * identifier of a patient. This method is used to find all the RedRiskNotification objects
     * associated with a particular patient identified by the patient id.
     * @param status - Status to be update.
     * @return The method `findByPatientIdAndStatus` returns a list of `RedRiskNotification` objects that
     * match the given `patient id`.
     */
    public List<RedRiskNotification> findByPatientIdAndStatus(String patientId, String status);
}