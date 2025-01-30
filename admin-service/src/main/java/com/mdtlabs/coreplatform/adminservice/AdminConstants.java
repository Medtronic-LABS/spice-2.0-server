package com.mdtlabs.coreplatform.adminservice;

/**
 * <p>
 * The AdminConstants class contains constants for various field names used in a admin service.
 * </p>
 *
 * @author Divya S
 */

public class AdminConstants {
    private AdminConstants() {}

    public static final String COUNTRY = "country";
    public static final String DISTRICT = "district";
    public static final String COUNTY = "county";
    public static final String SUB_COUNTY = "sub county";
    public static final String CHIEFDOM = "chiefdom";
    public static final String HEALTH_FACILITY = "healthfacility";
    public static final String VILLAGE = "village";

    public static final String COUNTRY_CODE = "country code";
    public static final String CHIEFDOM_CODE = "chiefdom code";
    public static final String DISTRICT_CODE = "district code";
    public static final String COUNTY_CODE = "county code";
    public static final String SUB_COUNTY_CODE = "sub county code";
    public static final String HEALTH_FACILITY_CODE = "hf_code";
    public static final String VILLAGE_CODE = "village code";
    public static final String VILLAGE_TYPE = "village type";


    public static final String COUNTRIES = "countries";
    public static final String DISTRICTS = "districts";
    public static final String CHIEFDOMS = "chiefdoms";
    public static final String HEALTH_FACILITIES = "healthfacilities";
    public static final String VILLAGES = "villages";

    public static final String VILLAGE_REFERENCE = "__village__";
    public static final String ID_REFERENCE = "__id__";
    public static final String WORKSHEET = "Worksheet";

    public static final String KEY_COUNTRY_CODE = "countryCode";
    public static final String KEY_DISTRICT_CODE = "districtCode";
    public static final String KEY_CHIEFDOM_CODE = "chiefdomCode";
    public static final String KEY_VILLAGE_CODE = "villageCode";

    public static final String KEY_COUNTRY_ID = "countryId";
    public static final String KEY_DISTRICT_ID = "districtId";
    public static final String KEY_CHIEFDOM_ID = "chiefdomId";
    public static final String KEY_VILLAGE_ID = "villageId";

    public static final String KEY_COUNTRY_NAME = "countryName";
    public static final String KEY_DISTRICT_NAME = "districtName";
    public static final String KEY_CHIEFDOM_NAME = "chiefdomName";
    public static final String KEY_VILLAGE_NAME = "villageName";
    public static final String KEY_VILLAGE_TYPE = "villageType";
    public static final String KEY_COUNTRY_TENANT_ID = "countryTenantId";
    public static final String KEY_DISTRICT_TENANT_ID = "districtTenantId";
    public static final String KEY_CHIEFDOM_TENANT_ID = "chiefdomTenantId";
    public static final String KEY_PARENT_TENANT_ID = "parentTenantId";

    public static final int ZERO = 0;
    public static final int THREE = 3;
    public static final int FOUR = 4;
    public static final int NINE = 9;
    public static final String DEFAULT_FORM_INPUT = "{\"time\":1735901698841,\"formLayout\":[{\"id\":\"other1735891043228\",\"viewType\":\"CardView\",\"title\":\"Other\",\"familyOrder\":0},{\"id\":\"TestedOn\",\"viewType\":\"DatePicker\",\"title\":\"Tested On\",\"fieldName\":\"TestedOn\",\"family\":\"other1735891043228\",\"isMandatory\":true,\"isEnabled\":true,\"visibility\":\"visible\",\"isDefault\":false,\"disableFutureDate\":true,\"minDays\":null,\"maxDays\":null,\"isDeletable\":false,\"orderId\":1},{\"id\":\"%s\",\"viewType\":\"EditText\",\"title\":\"Result\",\"fieldName\":\"%s\",\"family\":\"other1735891043228\",\"isMandatory\":true,\"isEnabled\":true,\"isResult\":true,\"visibility\":\"visible\",\"hint\":\"\",\"errorMessage\":\"\",\"inputType\":2,\"isDefault\":false,\"orderId\":2,\"code\":null,\"url\":null,\"resource\":\"Quantity\",\"condition\":null,\"minValue\":null,\"maxValue\":null,\"unitList\":[{\"name\":\"mg/dL\",\"id\":\"mg/dL\"},{\"name\":\"mcmol/L\",\"id\":\"mcmol/L\"},{\"name\":\"pg/mL\",\"id\":\"pg/mL\"},{\"name\":\"mg/24 hrs\",\"id\":\"mg/24 hrs\"},{\"name\":\"mmol/L\",\"id\":\"mmol/L\"},{\"name\":\"mg/g\",\"id\":\"mg/g\"},{\"name\":\"g/dL\",\"id\":\"g/dL\"},{\"name\":\"mU/L\",\"id\":\"mU/L\"},{\"name\":\"mug/dL\",\"id\":\"mug/dL\"},{\"name\":\"U/L\",\"id\":\"U/L\"},{\"name\":\"mmol/mol\",\"id\":\"mmol/mol\"},{\"name\":\"ng/dL\",\"id\":\"ng/dL\"},{\"name\":\"mEq/L\",\"id\":\"mEq/L\"},{\"name\":\"%%\",\"id\":\"%%\"},{\"name\":\"mU/mL\",\"id\":\"mU/mL\"},{\"name\":\"mg/mg\",\"id\":\"mg/mg\"}]}]}";
}
