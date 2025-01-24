package com.mdtlabs.coreplatform.adminservice.message;

/**
 * <p>
 * Success code to fetch message from property. Property
 * file(application.property) present in resource folder.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 22, 2024
 */
public enum SuccessCode {

    //File
    FILE_UPLOAD(1001),
    FILE_DOWNLOAD(1002),


    WORKFLOW_SAVE(2101),
    WORKFLOW_UPDATE(2102),
    GET_WORKFLOW(2103),
    WORKFLOW_DELETE(2104),

    COUNTRY_CUSTOMIZATION_SAVE(2151),
    COUNTRY_CUSTOMIZATION_UPDATE(2152),
    GET_COUNTRY_CUSTOMIZATION(2153),

    // REGION_DELETE(2154),

    USER_SAVE(15001),
    USER_UPDATE(15002),
    GET_USERS(15003),
    GET_USER(15004),
    USER_DELETE(15005),

    //Medication
    MEDICATION_SAVE(12001),
    MEDICATION_UPDATE(12002),
    GET_MEDICATION(12003),
    GET_MEDICATIONS(12004),
    MEDICATION_STATUS_UPDATE(12005),
    GET_MEDICATION_CLASSIFICATION(12008),
    GET_DOSAGE_FORMS(12009),

    //Lab test
    LABTEST_SAVE(18001),
    LABTEST_UPDATE(18002),
    GET_LABTEST(18003),
    GET_LABTESTS(18004),
    LABTEST_DELETE(18005),
    LABTEST_VALIDATE(18006),
    COUNTRY_UPDATE(19002),
    GET_COUNTRY(19003),
    HEALTH_FACILITY_UPDATE(27002),
    GET_HEALTH_FACILITY(27008),
    GET_HEALTH_FACILITIES_BY_DISTRICT_ID(27009),
    HEALTH_FACILITY_ADMIN_SAVE(27005),
    HEALTH_FACILITY_ADMIN_UPDATE(27006),
    HEALTH_FACILITY_ADMIN_REMOVE(27007),
    GET_HEALTH_FACILITY_TYPES(27010),
    REGION_ADMIN_SAVE(19011),
    REGION_ADMIN_UPDATE(19012),
    REGION_ADMIN_DELETE(19013),
    GET_COUNTRY_CODES(19005),
    GET_CULTURES(19006),
    GET_VILLAGE_LIST(1300),
    GET_CHIEFDOM_LIST(1301),
    GET_DISTRICT_LIST(1302), SUPER_ADMIN_SAVE(27011),
    
    PROGRAM_SAVE(14001),
    PROGRAM_DELETE(14002),
    GET_PROGRAM(14003),
    GET_PROGRAMS(14004),
    PROGRAM_STATUS_UPDATE(14005),
    
    WORKFLOW_CUSTOMIZATION_SAVE(16001),
    WORKFLOW_CUSTOMIZATION_UPDATE(16002),
    GET_WORKFLOW_CUSTOMIZATION(16003),
    WORKFLOW_CUSTOMIZATION_DELETE(16004),

    //District
    GET_DISTRICT(2003),
    GET_DISTRICTS(2010),
    GET_DEACTIVATED_DISTRICTS(20100),
    UPDATE_DISTRICT(2002),
    DISTRICT_ACTIVATE(2004),
    DISTRICT_DEACTIVATE(2005),

    //chiefdom
    GOT_CHIEFDOM(3000),
    UPDATE_CHIEFDOM(3001),
    GET_TERMS_AND_CONDITION(4001),

    //Designation
    GET_DESIGNATIONS(20200),
    DESIGNATION_NOT_FOUND(20201);

    private int key;

    SuccessCode(int key) {
        this.key = key;
    }

    public int getKey() {
        return this.key;
    }
}
