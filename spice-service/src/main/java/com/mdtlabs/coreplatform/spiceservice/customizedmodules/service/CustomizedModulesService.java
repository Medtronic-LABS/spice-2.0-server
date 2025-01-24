package com.mdtlabs.coreplatform.spiceservice.customizedmodules.service;

import java.util.List;
import java.util.Map;

/**
 * This interface is responsible for performing actions in CustomizedModules Entity.
 *
 * @since Oct 24, 2022
 * @author Divya S
 */
public interface CustomizedModulesService {
	
    /**
     * <p>
     * Creates customized modules.
     * </p>
     *
     * @param modules        customized modules with dynamic fields and its values.
     * @param type           type of workflow like Screening, Enrollment or Assessment
     * @param memberId the member ID of the patient
     * @param patientId the patient ID for whom to be created is given
     */
    void createCustomizedModules(List<Map<String, Object>> modules, String type, String memberId, String patientId);

    /**
     * <p>
     * Creates customized modules.
     * </p>
     *
     * @param memberId the member ID of the patient for whom to be updated is given
     * @param patientId the patient ID for whom to be updated is given
     */
    void updateCustomizedModules(String memberId, String patientId);
    /**
     * <p>
     * Creates customized modules.
     * </p>
     *
     * @param memberId the member ID of the patient for whom to be updated is given
     * @param tenantId the tenant ID of the user is given
     */
    void updateCustomizedModulesForTransfer(String memberId, Long tenantId);
}
