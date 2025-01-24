package com.mdtlabs.coreplatform.spiceservice.followup.repository;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.enumeration.AppointmentType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.CallRegister;

/**
 * <p>
 * This class is a repository class to perform operation on Call Register repositories
 * </p>
 *
 * @author Yogeshwaran Mohan created on Mar 26, 2024
 */
@Repository
public interface CallRegisterRepository extends JpaRepository<CallRegister, Long> {

    String GET_CALL_REGISTER_BY_VILLAGE_IDS = "FROM CallRegister as callRegister where" +
            " (COALESCE(:villageIds) is null OR (callRegister.villageId in (:villageIds)))" +
            " and ((callRegister.type = 'HH_VISIT' and callRegister.nextVisitDate < :householdVisitDate)" +
            " or (callRegister.type = 'REFERRED' and callRegister.encounterDate < :referralDate)" +
            " or (callRegister.type = 'MEDICAL_REVIEW' and callRegister.nextVisitDate < :medicalReviewDate))" +
            " and (cast(:lastSyncTime as Date) is null OR (callRegister.updatedAt > :lastSyncTime))" +
            " and (cast(:currentSyncTime as Date) is null OR (callRegister.updatedAt <= :currentSyncTime))" +
            " and (cast(:isCompleted as Boolean) is null OR (callRegister.isCompleted = :isCompleted))" +
            " and callRegister.isDeleted=false";

    String GET_CALL_REGISTER_BY_VILLAGE_ID = "select cr.patient_id as patientId, cr.patient_status as patientStatus, " +
            "cr.is_completed as completed, cr.created_by as createdBy, cr.updated_by as updatedBy, cr.attempts as attempts ,cr" +
            ".encounter_type as encounterType, cr.visits as visits, cr.type as type , uv.user_id as " +
            "chwId, uv.village_id as villageId from " +
            "call_register cr join user_village uv on cr.village_id = cast(uv.village_id as text) AND cr.is_completed = false where" +
            " (COALESCE(:villageIds) is null OR (cr.village_id in (:villageIds))) " +
            "and (cast(:startDate as Date) is null OR ((cr.type = 'HH_VISIT' and cr.next_visit_date >= :startDate " +
            "and cr.next_visit_date <= :endDate) or (cr.type = 'REFERRED' and cr.encounter_date + INTERVAL '2 " +
            "days' >= :startDate " +
            "and cr.encounter_date + INTERVAL '2 days' <= :endDate) or (cr.type = 'MEDICAL_REVIEW' and " +
            "cr.next_visit_date >= :startDate " + "and cr.next_visit_date <= :endDate))) " +
            "and cr.type in (:appointmentTypes)";

    String GET_CALL_REGISTER_DETAILS_BY_VILLAGE_ID = "select cr.patient_id as patientId, cr.patient_status as " +
            "patientStatus, cr.is_completed as completed, cr.created_by as createdBy, cr.updated_by as updatedBy, " +
            "cr.attempts as attempts ,cr.encounter_type as encounterType, cr.visits as visits, cr.type as type , uv.user_id as " +
            "chwId, uv.village_id as villageId from " +
            "call_register cr join user_village uv on cr.village_id = cast(uv.village_id as text) " +
            "join call_register_detail crd on crd.call_register_id = cr.id where" +
            " (COALESCE(:villageIds) is null OR (cr.village_id in (:villageIds)))" +
            " and (cast(:startDate as Date) is null OR " +
            "(crd.call_date >= :startDate and crd.call_date <= :endDate)) " +
            "and cr.type in (:appointmentTypes)";

    String GET_CALL_REGISTERS_BY_PATIENT_IDS = " SELECT CR.*, VILLAGE.ID AS VILLAGE_ID, VILLAGE.NAME AS VILLAGE_NAME, " +
            " HF.ID AS HEALTH_FACILITY_ID, HF.NAME AS HEALTH_FACILITY_NAME, CF.ID AS CHIEFDOM_ID, " +
            " CF.NAME AS CHIEFDOM_NAME, DT.ID AS DISTRICT_ID, DT.NAME AS DISTRICT_NAME, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) IS NULL THEN CR.NEXT_BP_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) IS NULL THEN CR.NEXT_BG_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) > CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE ELSE CR.NEXT_BG_ASSESSMENT_DATE END AS ASSESSMENT_DATE, " +
            " CASE WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END AS MEDICAL_REVIEW_DATE, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END AS NEXT_BG_ASSESSMENT_DATE, " +
            " CASE WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END AS NEXT_BP_ASSESSMENT_DATE , " +
            " GREATEST(CASE " +
            " WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END, CASE WHEN " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , CASE " +
            " WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE as TIMESTAMP) ELSE NULL END) " +
            " AS DUE_DATE " +
            " FROM CALL_REGISTER CR " +
            " LEFT JOIN VILLAGE ON VILLAGE.ID = CAST(CR.VILLAGE_ID AS INTEGER) " +
            " LEFT JOIN HEALTH_FACILITY HF ON HF.FHIR_ID = CR.REFERRED_SITE_ID " +
            " LEFT JOIN CHIEFDOM CF ON CF.ID = HF.CHIEFDOM_ID " +
            " LEFT JOIN DISTRICT DT ON DT.ID = HF.DISTRICT_ID " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (COALESCE(:patientIds) IS NULL OR CR.PATIENT_ID IN (:patientIds)) ";

    String GET_PATIENT_IDS_BY_SCREENING_DATE_TIME = "SELECT CR.PATIENT_ID FROM CALL_REGISTER CR " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = true " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND (CAST(:startDate AS DATE) IS NULL OR CR.SCREENING_DATE_TIME >= CAST(:startDate AS DATE)) " +
            " AND (CAST(:endDate AS DATE) IS NULL OR CR.SCREENING_DATE_TIME <= CAST(:endDate AS DATE)) ";

    String GET_PATIENTS_BY_SCREENING_DATE_TIME = "SELECT CR.*, VILLAGE.ID AS VILLAGE_ID, VILLAGE.NAME AS VILLAGE_NAME, " +
            " HF.ID AS HEALTH_FACILITY_ID, HF.NAME AS HEALTH_FACILITY_NAME, CF.ID AS CHIEFDOM_ID, " +
            " CF.NAME AS CHIEFDOM_NAME, DT.ID AS DISTRICT_ID, DT.NAME AS DISTRICT_NAME FROM CALL_REGISTER CR " +
            " LEFT JOIN VILLAGE ON VILLAGE.ID = CAST(CR.VILLAGE_ID AS INTEGER) " +
            " LEFT JOIN HEALTH_FACILITY HF ON HF.FHIR_ID = CR.REFERRED_SITE_ID " +
            " LEFT JOIN CHIEFDOM CF ON CF.ID = HF.CHIEFDOM_ID " +
            " LEFT JOIN DISTRICT DT ON DT.ID = HF.DISTRICT_ID " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = true " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND (CAST(:startDate AS DATE) IS NULL OR CR.SCREENING_DATE_TIME >= CAST(:startDate AS DATE)) " +
            " AND (CAST(:endDate AS DATE) IS NULL OR CR.SCREENING_DATE_TIME <= CAST(:endDate AS DATE)) ";

    String GET_PATIENT_IDS_BY_ASSESSMENT_DATE_TIME = " SELECT CR.PATIENT_ID FROM CALL_REGISTER CR " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = TRUE " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND ((CR.NEXT_BG_ASSESSMENT_DATE IS NOT NULL AND CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE) " +
            " OR (CR.NEXT_BP_ASSESSMENT_DATE IS NOT NULL AND CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE)) " +
            " AND (CAST(:startDate AS DATE) IS NULL AND CAST(:endDate AS DATE) IS NULL " +
            " OR CASE WHEN CR.NEXT_BG_ASSESSMENT_DATE IS NULL " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE >= CAST(:startDate AS DATE) " +
            " AND CR.NEXT_BP_ASSESSMENT_DATE <= CAST(:endDate AS DATE) " +
            " WHEN CR.NEXT_BP_ASSESSMENT_DATE IS NULL " +
            " THEN CR.NEXT_BG_ASSESSMENT_DATE >= CAST(:startDate AS DATE) AND CR.NEXT_BG_ASSESSMENT_DATE <= CAST(:endDate AS DATE) " +
            " WHEN CR.NEXT_BP_ASSESSMENT_DATE > CR.NEXT_BG_ASSESSMENT_DATE AND CR.NEXT_BP_ASSESSMENT_DATE <= CURRENT_DATE " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE >= CAST(:startDate AS DATE) AND CR.NEXT_BP_ASSESSMENT_DATE <= CAST(:endDate AS DATE) " +
            " ELSE CR.NEXT_BG_ASSESSMENT_DATE >= CAST(:startDate AS DATE) AND CR.NEXT_BG_ASSESSMENT_DATE <= CAST(:endDate AS DATE) END ) ";

    String GET_ASSESSMENT_CALL_REGISTERS = " SELECT CR.*, VILLAGE.ID AS VILLAGE_ID, VILLAGE.NAME AS VILLAGE_NAME, " +
            " HF.ID AS HEALTH_FACILITY_ID, HF.NAME AS HEALTH_FACILITY_NAME, CF.ID AS CHIEFDOM_ID, " +
            " CF.NAME AS CHIEFDOM_NAME, DT.ID AS DISTRICT_ID, DT.NAME AS DISTRICT_NAME, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) IS NULL THEN CR.NEXT_BP_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) IS NULL THEN CR.NEXT_BG_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) < CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE ELSE CR.NEXT_BG_ASSESSMENT_DATE END AS assessment_date " +
            " FROM CALL_REGISTER CR " +
            " LEFT JOIN VILLAGE ON VILLAGE.ID = CAST(CR.VILLAGE_ID AS INTEGER) " +
            " LEFT JOIN HEALTH_FACILITY HF ON HF.FHIR_ID = CR.REFERRED_SITE_ID " +
            " LEFT JOIN CHIEFDOM CF ON CF.ID = HF.CHIEFDOM_ID " +
            " LEFT JOIN DISTRICT DT ON DT.ID = HF.DISTRICT_ID " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = TRUE " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND ((CR.NEXT_BG_ASSESSMENT_DATE IS NOT NULL AND CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE) " +
            " OR (CR.NEXT_BP_ASSESSMENT_DATE IS NOT NULL AND CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE)) " +
            " AND (CAST(:startDate AS DATE) IS NULL AND CAST(:endDate AS DATE) IS NULL " +
            " OR CASE WHEN CR.NEXT_BG_ASSESSMENT_DATE IS NULL " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE >= CAST(:startDate AS DATE) " +
            " AND CR.NEXT_BP_ASSESSMENT_DATE <= CAST(:endDate AS DATE) " +
            " WHEN CR.NEXT_BP_ASSESSMENT_DATE IS NULL " +
            " THEN CR.NEXT_BG_ASSESSMENT_DATE >= CAST(:startDate AS DATE) AND CR.NEXT_BG_ASSESSMENT_DATE <= CAST(:endDate AS DATE) " +
            " WHEN CR.NEXT_BP_ASSESSMENT_DATE > CR.NEXT_BG_ASSESSMENT_DATE AND CR.NEXT_BP_ASSESSMENT_DATE <= CURRENT_DATE " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE >= CAST(:startDate AS DATE) AND CR.NEXT_BP_ASSESSMENT_DATE <= CAST(:endDate AS DATE) " +
            " ELSE CR.NEXT_BG_ASSESSMENT_DATE >= CAST(:startDate AS DATE) AND CR.NEXT_BG_ASSESSMENT_DATE <= CAST(:endDate AS DATE) END ) ";

    String GET_LOST_TO_FOLLOWUP_PATIENT_IDS = " SELECT CR.PATIENT_ID FROM CALL_REGISTER CR " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = TRUE " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND ((CR.NEXT_BP_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day') OR " +
            " (CR.NEXT_BG_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " OR (CR.NEXT_MEDICAL_REVIEW_DATE IS NOT NULL AND " +
            " CAST(NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day')) " +
            " AND ( cr.is_completed = false or (CR.UPDATED_AT < CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) " +
            " AND (CR.NEXT_MEDICAL_REVIEW_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day')) " +
            " OR (CR.UPDATED_AT < CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) " +
            " AND (CR.NEXT_BP_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day'))" +
            " OR (CR.UPDATED_AT < CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) AND (CR.NEXT_BG_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day'))) " +
            " AND ((CAST(:startDate AS DATE) IS NULL AND CAST(:endDate AS DATE) IS NULL) OR (GREATEST(CASE " +
            " WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END, CASE WHEN " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , CASE " +
            " WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END) >= (CAST(:startDate AS TIMESTAMP) " +
            " - INTERVAL '90 day') AND GREATEST(CASE WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= " +
            " (CURRENT_TIMESTAMP - INTERVAL '90 day') THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) " +
            " ELSE NULL END , " +
            " CASE WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END) " +
            " <= (CAST(:endDate AS TIMESTAMP) - INTERVAL '90 day'))) ";

    String GET_LOST_TO_FOLLOWUP_CALL_REGISTERS = " SELECT CR.id as callRegisterId, CR.*, " +
            " CASE WHEN CR.IS_COMPLETED = FALSE THEN CR.ID ELSE NULL END AS ID, " +
            " CASE WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END AS MEDICAL_REVIEW_DATE, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END AS NEXT_BG_ASSESSMENT_DATE, " +
            " CASE WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END AS NEXT_BP_ASSESSMENT_DATE , " +
            " GREATEST(CASE " +
            " WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END, CASE WHEN " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , CASE " +
            " WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE as TIMESTAMP) ELSE NULL END) " +
            " AS DUE_DATE , " +
            " VILLAGE.ID AS VILLAGE_ID, VILLAGE.NAME AS VILLAGE_NAME, " +
            " HF.ID AS HEALTH_FACILITY_ID, HF.NAME AS HEALTH_FACILITY_NAME, CF.ID AS CHIEFDOM_ID, " +
            " CF.NAME AS CHIEFDOM_NAME, DT.ID AS DISTRICT_ID, DT.NAME AS DISTRICT_NAME, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) IS NULL THEN CR.NEXT_BP_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) IS NULL THEN CR.NEXT_BG_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) > CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CURRENT_DATE " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE ELSE CR.NEXT_BG_ASSESSMENT_DATE END AS ASSESSMENT_DATE " +
            " FROM CALL_REGISTER CR " +
            " LEFT JOIN VILLAGE ON VILLAGE.ID = CAST(CR.VILLAGE_ID AS INTEGER) " +
            " LEFT JOIN HEALTH_FACILITY HF ON HF.FHIR_ID = CR.REFERRED_SITE_ID " +
            " LEFT JOIN CHIEFDOM CF ON CF.ID = HF.CHIEFDOM_ID " +
            " LEFT JOIN DISTRICT DT ON DT.ID = HF.DISTRICT_ID " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = TRUE " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND ((CR.NEXT_BP_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day') OR " +
            " (CR.NEXT_BG_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " OR (CR.NEXT_MEDICAL_REVIEW_DATE IS NOT NULL AND " +
            " CAST(NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day')) " +
            " AND ( cr.is_completed = false or  (CR.UPDATED_AT < CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) " +
            " AND (CR.NEXT_MEDICAL_REVIEW_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day')) " +
            " OR (CR.UPDATED_AT < CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) " +
            " AND (CR.NEXT_BP_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day'))" +
            " OR (CR.UPDATED_AT < CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) AND (CR.NEXT_BG_ASSESSMENT_DATE IS NOT NULL AND " +
            " CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= CURRENT_TIMESTAMP - INTERVAL '90 day'))) " +
            " AND ((CAST(:startDate AS DATE) IS NULL AND CAST(:endDate AS DATE) IS NULL) OR (GREATEST(CASE " +
            " WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END, CASE WHEN " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , CASE " +
            " WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END) >= (CAST(:startDate AS TIMESTAMP) " +
            " - INTERVAL '90 day') AND GREATEST(CASE WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= " +
            " (CURRENT_TIMESTAMP - INTERVAL '90 day') THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) " +
            " ELSE NULL END , " +
            " CASE WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CURRENT_TIMESTAMP - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END) " +
            " <= (CAST(:endDate AS TIMESTAMP) - INTERVAL '90 day'))) ";

    String GET_MEDICAL_REVIEW_FOLLOWUP_PATIENT_IDS = "SELECT CR.PATIENT_ID FROM CALL_REGISTER CR " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = true " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND (CAST(:startDate AS DATE) IS NULL OR CR.NEXT_MEDICAL_REVIEW_DATE >= CAST(:startDate AS DATE)) " +
            " AND (CAST(:endDate AS DATE) IS NULL OR CR.NEXT_MEDICAL_REVIEW_DATE <= CAST(:endDate AS DATE)) " +
            " AND (CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS DATE) IS NOT NULL AND " +
            " CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS DATE) <= CURRENT_DATE) ";

    String GET_MEDICAL_REVIEW_FOLLOWUP_LIST = "SELECT CR.*, VILLAGE.ID AS VILLAGE_ID, VILLAGE.NAME AS VILLAGE_NAME, " +
            " HF.ID AS HEALTH_FACILITY_ID, HF.NAME AS HEALTH_FACILITY_NAME, CF.ID AS CHIEFDOM_ID, " +
            " CF.NAME AS CHIEFDOM_NAME, DT.ID AS DISTRICT_ID, DT.NAME AS DISTRICT_NAME FROM CALL_REGISTER CR " +
            " LEFT JOIN VILLAGE ON VILLAGE.ID = CAST(CR.VILLAGE_ID AS INTEGER) " +
            " LEFT JOIN HEALTH_FACILITY HF ON HF.FHIR_ID = CR.REFERRED_SITE_ID " +
            " LEFT JOIN CHIEFDOM CF ON CF.ID = HF.CHIEFDOM_ID " +
            " LEFT JOIN DISTRICT DT ON DT.ID = HF.DISTRICT_ID " +
            " WHERE (COALESCE(:referredSiteId) is null OR CR.REFERRED_SITE_ID = :referredSiteId) " +
            " AND CR.\"type\" = :type AND CR.IS_COMPLETED = :isCompleted AND CR.IS_DELETED = FALSE " +
            " AND (((:retryAttempts) IS NULL OR (CASE WHEN CR.IS_COMPLETED = true " +
            " THEN 0 ELSE CR.ATTEMPTS END) IN (:retryAttempts)) OR ((:callRegisterCount) = 0 " +
            " AND CR.ATTEMPTS IS NULL)) " +
            " AND (CAST(:startDate AS DATE) IS NULL OR CR.NEXT_MEDICAL_REVIEW_DATE >= CAST(:startDate AS DATE)) " +
            " AND (CAST(:endDate AS DATE) IS NULL OR CR.NEXT_MEDICAL_REVIEW_DATE <= CAST(:endDate AS DATE)) " +
            " AND (CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS DATE) IS NOT NULL AND " +
            " CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS DATE) <= CURRENT_DATE) ";

    String FETCH_ALL_CALL_REGISTERS = "SELECT CR.*, VILLAGE.ID AS VILLAGE_ID, VILLAGE.NAME AS VILLAGE_NAME, " +
            " HF.ID AS HEALTH_FACILITY_ID, HF.NAME AS HEALTH_FACILITY_NAME, CF.ID AS CHIEFDOM_ID, " +
            " CF.NAME AS CHIEFDOM_NAME, DT.ID AS DISTRICT_ID, DT.NAME AS DISTRICT_NAME, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) IS NULL " +
            " AND CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CAST(:currentSyncTime AS DATE)  THEN CR.NEXT_BP_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) IS NULL " +
            " AND CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) <= CAST(:currentSyncTime AS DATE) THEN CR.NEXT_BG_ASSESSMENT_DATE " +
            " WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) > CAST(CR.NEXT_BG_ASSESSMENT_DATE AS DATE) AND " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS DATE) <= CAST(:currentSyncTime AS DATE) " +
            " THEN CR.NEXT_BP_ASSESSMENT_DATE ELSE CR.NEXT_BG_ASSESSMENT_DATE END AS ASSESSMENT_DATE, " +
            " CASE WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CAST(:currentSyncTime AS TIMESTAMP) - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END AS MEDICAL_REVIEW_DATE, " +
            " CASE WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CAST(:currentSyncTime AS TIMESTAMP) - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END AS NEXT_BG_ASSESSMENT_DATE, " +
            " CASE WHEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CAST(:currentSyncTime AS TIMESTAMP) - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END AS NEXT_BP_ASSESSMENT_DATE , " +
            " GREATEST(CASE " +
            " WHEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) <= (CAST(:currentSyncTime AS TIMESTAMP) - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_MEDICAL_REVIEW_DATE AS TIMESTAMP) ELSE NULL END, CASE WHEN " +
            " CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) <= (CAST(:currentSyncTime AS TIMESTAMP) - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BP_ASSESSMENT_DATE AS TIMESTAMP) ELSE NULL END , CASE " +
            " WHEN CAST(CR.NEXT_BG_ASSESSMENT_DATE AS TIMESTAMP) <= (CAST(:currentSyncTime AS TIMESTAMP) - INTERVAL '90 day') " +
            " THEN CAST(CR.NEXT_BG_ASSESSMENT_DATE as TIMESTAMP) ELSE NULL END) " +
            " AS DUE_DATE " +
            " FROM CALL_REGISTER CR " +
            " LEFT JOIN VILLAGE ON VILLAGE.ID = CAST(CR.VILLAGE_ID AS INTEGER) " +
            " LEFT JOIN HEALTH_FACILITY HF ON HF.FHIR_ID = CR.REFERRED_SITE_ID " +
            " LEFT JOIN CHIEFDOM CF ON CF.ID = HF.CHIEFDOM_ID " +
            " LEFT JOIN DISTRICT DT ON DT.ID = HF.DISTRICT_ID " +
            " WHERE CR.\"type\" = :type " +
            " AND CR.UPDATED_AT <= :currentSyncTime " +
            " AND (CAST(:lastSyncTime AS DATE) IS NULL OR CR.UPDATED_AT > CAST(:lastSyncTime AS DATE)) " +
            " AND (COALESCE(:villageIds) IS NULL OR CR.VILLAGE_ID IN (:villageIds)) ";

    /**
     * Find a CallRegister by id
     *
     * @param id - Id
     * @return CallRegister
     */
    CallRegister findByIdAndIsDeletedFalse(Long id);

    /**
     * Find a CallRegister by id
     *
     * @param id - Id
     * @return CallRegister
     */
    CallRegister findByIdAndIsDeletedFalseAndIsCompletedFalse(Long id);

    /**
     * Find a List of CallRegister by Village ids and cut off date
     *
     * @param villageIds         - Village Ids
     * @param householdVisitDate - Household Visit Date
     * @param referralDate       - Referral Date
     * @param medicalReviewDate  - Medical Review Date
     * @param isCompleted        - Is completed
     * @return List<CallRegister>
     */
    @Query(value = GET_CALL_REGISTER_BY_VILLAGE_IDS)
    List<CallRegister> findByVillageIds(@Param(Constants.VILLAGE_IDS) List<String> villageIds,
                                        @Param(Constants.HOUSEHOLD_VISIT_DATE) Date householdVisitDate,
                                        @Param(Constants.REFERRAL_DATE) Date referralDate,
                                        @Param(Constants.MEDICAL_REVIEW_DATE) Date medicalReviewDate,
                                        @Param(Constants.LAST_SYNC_TIME) Date lastSyncTime,
                                        @Param(Constants.CURRENT_SYNC_TIME) Date currentSyncTime,
                                        @Param(Constants.IS_COMPLETED) Boolean isCompleted);

    /**
     * Find a list of CallRegister by village IDs and user IDs.
     *
     * @param villageIds - List of village IDs
     * @return List of CallRegister entities
     */
    @Query(value = GET_CALL_REGISTER_BY_VILLAGE_ID, nativeQuery = true)
    List<Map<String, Object>> findByVillageIds(@Param(Constants.VILLAGE_IDS) Set<String> villageIds, @Param(Constants.START_DATE) Date startDate,
                                               @Param(Constants.END_DATE) Date endDate, @Param(Constants.APPOINTMENT_TYPES) List<String> appointmentTypes);

    /**
     * Find a list of CallRegister by village IDs and user IDs.
     *
     * @param villageIds - List of village IDs
     * @return List of CallRegister entities
     */
    @Query(value = GET_CALL_REGISTER_DETAILS_BY_VILLAGE_ID, nativeQuery = true)
    List<Map<String, Object>> findCallRegisterByVillageIdsWithDetails(@Param(Constants.VILLAGE_IDS) Set<String> villageIds, @Param(Constants.START_DATE) Date startDate,
                                               @Param(Constants.END_DATE) Date endDate, @Param(Constants.APPOINTMENT_TYPES) List<String> appointmentTypes);


    /**
     * Find a List of CallRegister by Encounter types
     *
     * @param memberId      - Member Id
     * @param encounterType - List of Encounter type
     * @param type          - List of Appointment type
     * @param isCompleted   - Is completed
     * @return List<CallRegister>
     */
    List<CallRegister> findByMemberIdAndEncounterTypeInAndTypeInAndIsCompletedAndIsDeletedFalse(String memberId, List<String> encounterType,
                                                                                              List<AppointmentType> type, Boolean isCompleted);

    /**
     * Find a List of CallRegister by memberId
     *
     * @param memberId    - Member Id
     * @param isCompleted - Is completed
     * @return List<CallRegister>
     */
    List<CallRegister> findByMemberIdAndIsCompletedAndIsDeletedFalse(String memberId, Boolean isCompleted);

    /**
     * Find a  CallRegister by memberId and Type
     *
     * @param memberId - Member Id
     * @param type     - Is completed
     * @return CallRegister
     */
    CallRegister findByMemberIdAndTypeAndIsDeletedFalse(String memberId, AppointmentType type);

    /**
     * Fetch list of call register based on member id and is completed and type.
     *
     * @param memberId member id for call register
     * @param type call type
     * @return list of call register
     */
    List<CallRegister> findByMemberIdAndTypeAndIsDeletedFalseAndIsCompletedFalse(String memberId, AppointmentType type);

    /**
     * Fetch the call register details from the call register.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param patientIds set of patient ids
     * @return the call register details as a page object.
     */
    @Query(value = GET_CALL_REGISTERS_BY_PATIENT_IDS, nativeQuery = true)
    List<Map<String, Object>> getCallRegisterByPatientIds(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                          @Param(Constants.TYPE) String type,
                                                          @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                          @Param(Constants.PATIENT_IDS) Set<String> patientIds);

    /**
     * Fetch the call register details from the call register. It includes the chiefdom, district, health facility,
     * village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @return the call register details as a page object.
     */
    @Query(value = GET_PATIENT_IDS_BY_SCREENING_DATE_TIME, nativeQuery = true)
    List<Map<String, Object>> getScreeningCallRegisterPatientIds(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                                 @Param(Constants.TYPE) String type,
                                                                 @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                                 @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                                 @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                                 @Param(Constants.START_DATE) String startDate,
                                                                 @Param(Constants.END_DATE) String endDate);

    /**
     * Fetch the call register details from the call register. It includes the chiefdom, district, health facility,
     * village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param pageable pageable information contains the no of call register details to fetch and sort details
     * @return the call register details as a page object.
     */
    @Query(value = GET_PATIENTS_BY_SCREENING_DATE_TIME, nativeQuery = true)
    Page<Map<String, Object>> getScreeningCallRegister(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                       @Param(Constants.TYPE) String type,
                                                       @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                       @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                       @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                       @Param(Constants.START_DATE) String startDate,
                                                       @Param(Constants.END_DATE) String endDate,
                                                       Pageable pageable);

    /**
     * Fetch the assessment call register details from the call register. It includes the chiefdom, district,
     * health facility, village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param retryAttempts retry attempts to fetch the call register details
     * @param callRegisterCount call register count to map with call register count
     * @param startDate start date to fetch the call register details
     * @param endDate end date
     * @return the call register details as a page object.
     */
    @Query(value = GET_PATIENT_IDS_BY_ASSESSMENT_DATE_TIME, nativeQuery = true)
    List<Map<String, Object>> getAssessmentCallRegisterPatientIds(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                        @Param(Constants.TYPE) String type,
                                                        @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                        @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                        @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                        @Param(Constants.START_DATE) String startDate,
                                                        @Param(Constants.END_DATE) String endDate);

    /**
     * Fetch the assessment call register details from the call register. It includes the chiefdom, district,
     * health facility, village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param retryAttempts retry attempts to fetch the call register details
     * @param callRegisterCount call register count to map with call register count
     * @param startDate start date to fetch the call register details
     * @param endDate end date
     * @param pageable pageable information contains the no of call register details to fetch and sort details
     * @return the call register details as a page object.
     */
    @Query(value = GET_ASSESSMENT_CALL_REGISTERS, nativeQuery = true)
    Page<Map<String, Object>> getAssessmentCallRegister(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                        @Param(Constants.TYPE) String type,
                                                        @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                        @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                        @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                        @Param(Constants.START_DATE) String startDate,
                                                        @Param(Constants.END_DATE) String endDate,
                                                        Pageable pageable);

    /**
     * Fetch the lost to follow up call register details from the call register. It includes the chiefdom, district,
     * health facility, village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param retryAttempts retry attempts to fetch the call register details
     * @param callRegisterCount call register count to map with call register count
     * @param startDate start date to fetch the call register details
     * @param endDate end date
     * @return the call register details as a page object.
     */
    @Query(value = GET_LOST_TO_FOLLOWUP_PATIENT_IDS, nativeQuery = true)
    List<Map<String, Object>> getLostToFollowUpCallRegisterPatientIds(@Param(Constants.TYPE) String type,
                                                            @Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                            @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                            @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                            @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                            @Param(Constants.START_DATE) String startDate,
                                                            @Param(Constants.END_DATE) String endDate);

    /**
     * Fetch the lost to follow up call register details from the call register. It includes the chiefdom, district,
     * health facility, village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param retryAttempts retry attempts to fetch the call register details
     * @param callRegisterCount call register count to map with call register count
     * @param startDate start date to fetch the call register details
     * @param endDate end date
     * @param pageable pageable information contains the no of call register details to fetch and sort details
     * @return the call register details as a page object.
     */
    @Query(value = GET_LOST_TO_FOLLOWUP_CALL_REGISTERS, nativeQuery = true)
    Page<Map<String, Object>> getLostToFollowUpCallRegister(@Param(Constants.TYPE) String type,
                                                            @Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                            @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                            @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                            @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                            @Param(Constants.START_DATE) String startDate,
                                                            @Param(Constants.END_DATE) String endDate,
                                                            Pageable pageable);


    /**
     * Fetch the assessment call register details from the call register. It includes the chiefdom, district,
     * health facility, village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param retryAttempts retry attempts to fetch the call register details
     * @param callRegisterCount call register count to map with call register count
     * @param startDate start date to fetch the call register details
     * @param endDate end date
     * @return the call register details as a page object.
     */
    @Query(value = GET_MEDICAL_REVIEW_FOLLOWUP_PATIENT_IDS, nativeQuery = true)
    List<Map<String, Object>> getMedicalReviewFollowUpCallRegisterPatientIds(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                                   @Param(Constants.TYPE) String type,
                                                                   @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                                   @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                                   @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                                   @Param(Constants.START_DATE) String startDate,
                                                                   @Param(Constants.END_DATE) String endDate);

    /**
     * Fetch the assessment call register details from the call register. It includes the chiefdom, district,
     * health facility, village details.
     *
     * @param referredSiteId referred site id to fetch the call register details
     * @param type AppointmentType to fetch the call register details
     * @param isCompleted whether the call register is completed or not
     * @param retryAttempts retry attempts to fetch the call register details
     * @param callRegisterCount call register count to map with call register count
     * @param startDate start date to fetch the call register details
     * @param endDate end date
     * @param pageable pageable information contains the no of call register details to fetch and sort details
     * @return the call register details as a page object.
     */
    @Query(value = GET_MEDICAL_REVIEW_FOLLOWUP_LIST, nativeQuery = true)
    Page<Map<String, Object>> getMedicalReviewFollowUpCallRegister(@Param(Constants.REFERRED_SITE_ID) String referredSiteId,
                                                                   @Param(Constants.TYPE) String type,
                                                                   @Param(Constants.IS_COMPLETED) boolean isCompleted,
                                                                   @Param(Constants.RETRY_ATTEMPTS) List<Integer> retryAttempts,
                                                                   @Param(Constants.CALL_REGISTER_COUNT) int callRegisterCount,
                                                                   @Param(Constants.START_DATE) String startDate,
                                                                   @Param(Constants.END_DATE) String endDate,
                                                                   Pageable pageable);

    /**
     * Get all call registers by updated at and village id and type, etc
     *
     * @param type AppointmentType to fetch the call register details.
     * @param lastSyncTime last sync time.
     * @param villageIds list of village id.
     * @param pageable pageable information contains the no of call register details to fetch and sort details
     * @return the call register details as a page object.
     */
    @Query(value = FETCH_ALL_CALL_REGISTERS, nativeQuery = true)
    Page<Map<String, Object>> getAllCallRegisters(@Param(Constants.TYPE) String type,
                                                  @Param(Constants.LAST_SYNC_TIME) Date lastSyncTime,
                                                  @Param(Constants.CURRENT_SYNC_TIME) Date currentSyncTime,
                                                  @Param(Constants.VILLAGE_IDS) List<String> villageIds,
                                                  Pageable pageable);

    /**
     * <p>
     * Fetch the  call registers of given member id
     * </p>
     *
     * @param memberId The member id of the patient
     * @return the call registers of given member id is returned.
     */
    List<CallRegister> findByMemberIdAndIsDeletedFalse(String memberId);

    /**
     * <p>
     * This method used to retrieve list of call register entities based on the user id
     * </p>
     *
     * @param userId of the call register.
     * @return the list of call register entities
     */
    List<CallRegister> findByUserIdAndIsInitiatedTrueAndIsDeletedFalse(Long userId);

    /**
     * <p>
     * This method used to retrieve list of call register entities based on the patient id and site id
     * </p>
     *
     * @param patientReference patient id to fetch call registers.
     * @param siteReference site id to fetch call registers
     * @return the list of call register entities
     */
    List<CallRegister> findByPatientIdAndReferredSiteIdAndIsDeletedFalseAndIsCompletedFalse(String patientReference,
                                                                                            String siteReference);
}
