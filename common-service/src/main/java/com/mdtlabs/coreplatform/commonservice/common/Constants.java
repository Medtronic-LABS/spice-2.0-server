package com.mdtlabs.coreplatform.commonservice.common;

import java.util.List;
import java.util.Map;

import static java.util.Map.entry;


/**
 * <p>
 * To define the common static parameter used all over the application.
 * </p>
 *
 * @author Karthick Murugesan created on Jan 11, 2024
 */
public final class Constants {

    private Constants() {
    }

    public static final String PACKAGE_CORE_PLATFORM = "com.mdtlabs.coreplatform";
    public static final String SPICE_LOGGER = "Logger";
    public static final int INACTIVE_SESSION_EXPIRE_HOURS = 5;

    // Common Symbols & Strings
    public static final String SPACE = " ";
    public static final String EMPTY = "";
    public static final String OPEN_BRACKET = "[";
    public static final String CLOSE_BRACKET = "]";
    public static final String HYPHEN = "-";
    public static final String LESSER_THAN = "<";
    public static final String GREATER_THAN = ">";
    public static final String LESSER_THAN_OR_EQUAL_TO = "<=";
    public static final String GREATER_THAN_OR_EQUAL_TO = ">=";
    public static final String ASTERISK_SYMBOL = "*";
    public static final String FORWARD_SLASH = "/";
    public static final String COMMA = ",";
    public static final String AND = "and";
    public static final String HOUR_SEPERATOR = ":";
    public static final String DOT = ".";
    public static final String HEXA_FORMATTER = "%02x";
    public static final String SHA_512 = "HmacSHA512";
    public static final String DOUBLE_COLON = "::";
    public static final String SUCCESS = "SUCCESS";
    public static final String ERROR = "ERROR";
    public static final String GENDER_MALE = "Male";
    public static final String GENDER_FEMALE = "Female";
    public static final String COLON = ":";
    public static final String UNDER_SCORE = "_";

    // Date Formate
    public static final String DATE_FORMAT_WITHOUT_MILLISECOND = "yyyy-MM-dd'T'HH:mm:ss";
    public static final String JSON_DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
    public static final String CURRENT_TIMESTAMP_DATE_FORMAT = "yyyy.MM.dd.HH.mm.ss";
    public static final String FHIR_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";
    public static final String CURRENT_TIMESTAMP_DATE_FORMAT_CONSTRAINTS = "yyyy.MM.dd.HH";
    public static final String DATE = "DATE";
    public static final String DATE_FORMAT_YYYY_MM_DD = "yyyy-MM-dd";

    public static final String DATE_FORMAT_TIMEZONE = "yyyy-MM-dd'T'HH:mm:ssxxx";
    public static final String DATE_FORMAT = "dd MMM, yyyy";
    public static final String NO_DATA_FOUND = "No Data Found";
    public static final List<Object> NO_DATA_LIST = List.of();
    public static final String GMT = "GMT";

    public static final String USER_ENTITY = "User";
    public static final String ROLE_ENTITY = "Role";
    public static final String FORGOT_NOTIFICATION_SUBJECT = "Spice Forgot Password";
    public static final String RESET_NOTIFICATION_SUBJECT = "Spice Password Reset";
    public static final String CREATION_NOTIFICATION_SUBJECT = "Spice User Creation";
    public static final String FORGOT_PASSWORD = "Forgot Password";
    public static final String USER_CREATION = "User Creation";
    public static final String APP_URL_EMAIL = "app_url_email";
    public static final String NEW_USER_CREATION = "User_Creation";
    public static final String FORGOT_PASSWORD_USER = "Forgot_Password";
    public static final String NOTIFICATION_STATUS_PROCESSED = "Notification status processed";
    public static final String NOTIFICATION_STATUS_FAILED = "Notification status failed";
    public static final String OFFLINE_STATUS_FAILED = "FAILED";

    public static final String SAVING_EMAIL_NOTIFICATION_ERROR = "Error while saving notification for email";
    public static final String HEADER_CLIENT = "client";

    // AESKEY
    public static final String CORE_PLATFORM = "Sp!(Ec0rpL@tf*rM$l";
    public static final String AES_KEY = "Telec0unseL0r";
    public static final String AES_KEY_TOKEN = "te!e(0unsElfb4287fncwin432fsjhbfbwnuejnciqm@vnr(0cqefeqccvernquxnufnq@1542for@321";

    // Application
    public static final String ISSUER = "SpiceApplication";
    public static final String ROLE_ID_PARAM = "roleId";
    public static final String PAGE_NUMBER = "pageNumber";
    public static final String ERROR_USER_BLOCKED = "User is blocked.";
    public static final String TOKEN_ISSUER = "User";
    public static final String AUTH_TOKEN_SUBJECT = "Auth_Token";
    public static final String REFRESH_TOKEN_SUBJECT = "Auth_Token";
    public static final Object WEB = "Web";
    public static final long EXPIRY_HOURS = 12;
    public static final long THOUSAND = 1000;
    public static final String SALT_KEY = "spice_uat";
    public static final String HASHING_CODE = "SHA-512";
    public static final long THREE = 3;
    public static final String GET_CLASS = "getClass";
    public static final int NUMBER_THREE = 3;
    public static final long TWENTY_FOUR = 24;
    public static final Long LONG_ZERO = 0L;
    public static final String STRING_ZERO = "0";
    public static final String STRING_TWO = "2";
    public static final String STRING_THREE = "3";
    public static final String STRING_FOUR = "4";
    public static final String STRING_FIVE = "5";
    public static final String LOG_PREFIX_REQUEST = "|>";
    public static final String LOG_PREFIX_RESPONSE = "|<";
    public static final String SPLIT_CONTENT = "\r\n|\r|\n";
    public static final String BEARER = "Bearer ";
    public static final long AUTH_TOKEN_EXPIRY_MINUTES = 120;
    public static final long REFRESH_TOKEN_EXPIRY_HOURS = 8;
    public static final String AUTH_COOKIE_NAME = "AuthCookie";
    public static final String CONTENT_TYPE = "application/json;charset=UTF-8";
    public static final String MESSAGE = "message";
    public static final String INFO_USER_EXIST = "Login employee isEnabled : ";
    public static final String CONTENT_TEXT_TYPE = "text/x-json;charset=UTF-8";
    public static final String CACHE_HEADER_NAME = "Cache-Control";
    public static final String CACHE_HEADER_VALUE = "no-cache";
    public static final String RSA = "RSA";
    public static final String ACCESS_CONTROL_EXPOSE_HEADERS = "Access-Control-Expose-Headers";
    public static final String AUTHORIZATION = "Authorization";
    public static final String COOKIE_HEADER = "Cookie";
    public static final String USER_ID_PARAM = "userId";
    public static final String USER_DATA = "userData";
    public static final String APPLICATION_TYPE = "applicationType";
    public static final String ANONYMOUS_USER = "anonymousUser";
    public static final String HIBERNATE_EJB_INTERCEPTOR = "hibernate.ejb.interceptor";
    public static final String HIBERNATE_SESSION_FACTORY = "hibernate.session_factory.interceptor";
    public static final String AUDIT = "Audit";
    public static final String GET = "get";
    public static final String MASTER_DATA = "master_data";
    public static final String APPLICATION_JSON = "application/json";
    public static final String DOCUMENTATION_SWAGGER = "documentation.swagger";
    public static final String DOCUMENTATION_SWAGGER_SERVICES = "documentation.swagger.services";
    public static final String TOKEN = "token";
    public static final String EMAIL = "email";
    public static final String ROLE_IDS = "roleIds";
    public static final String JWE_TOKEN = "jweToken";
    public static final String JWE_REFRESH_TOKEN = "jweRefreshToken";
    public static final String CLIENT = "client";
    public static final String SERVICE_NAME = "serviceName";
    public static final String NOTIFICATION_INSTANCE = "NOTIFICATION-SERVICE";
    public static final String LOGGER = "Logger";
    public static final int ZERO = 0;
    public static final int ONE = 1;
    public static final String STRING_ONE = "1";
    public static final Long LONG_ONE = 1l;
    public static final int TWO = 2;
    public static final int INT_THREE = 3;
    public static final int FOUR = 4;
    public static final long SIXTY = 60;
    public static final Boolean BOOLEAN_TRUE = Boolean.TRUE;
    public static final Boolean BOOLEAN_FALSE = Boolean.FALSE;
    public static final String TEXT = "text/*";
    public static final String APPLICATION_XML = "application/*+xml";
    public static final String APPLICATIONJSON = "application/*+json";
    public static final String EXP = "exp";
    public static final String AUTHORITY = "authority";
    public static final String DATE_TIME_FORMATTER = "d/M/yyyy";
    public static final String DATE_FORMAT_DD_MM_YYYY = "dd/MM/yyyy";
    public static final String SEPARATOR = "\\s*,\\s*";
    public static final String TIMESTAMP = "TIMESTAMP";
    public static final int WEB_NOTIFICATION_LIMIT = 5;
    public static final String NOTIFICATION_ID = "notificationId";
    public static final String TRUE = "true";
    public static final String MAIL_SMTP_AUTH = "mail.smtp.auth";
    public static final String MAIL_SMTP_STARTTLS_ENABLE = "mail.smtp.starttls.enable";
    public static final String MAIL_SMTP_HOST = "mail.smtp.host";
    public static final String MAIL_SMTP_PORT = "mail.smtp.port";
    public static final String MAIL_SMTP_SSL_TRUST = "mail.smtp.ssl.trust";
    public static final String MAIL_SMTP_SSL_PROTOCALS = "mail.smtp.ssl.protocols";
    public static final String SMTP_GMAIL_COM = "smtp.gmail.com";
    public static final String SMTP_PORT = "587";
    public static final String SMTP_SSL_PROTOCAL = "TLSv1.2";
    public static final String TEXT_HTML_CHARSET = "text/HTML; charset=UTF-8";
    public static final String FORMAT = "format";
    public static final String FLOWED = "flowed";
    public static final String CONTENT_TRANSFER_ENCODING = "Content-Transfer-Encoding";
    public static final String ENCODING = "8bit";
    public static final String UTF_8 = "UTF-8";
    public static final String NOTIFICATION = "NOTIFICATION";
    public static final String AUTH_API = "Auth API";
    public static final String VERSION = "1.0";
    public static final String DOCUMENTATION_AUTH_API = "Documentation Auth API v1.0";
    public static final String DOCUMENTATION_USER_API = "Documentation User API v1.0";
    public static final String USER_API = "User API";
    public static final String API_ROLES_MAP_CLEARED = "api permission map cleared";
    public static final String CREATE = "create";
    public static final String UPDATE = "update";
    public static final String UPDATED_AT = "updatedAt";
    public static final String TOKENS = "tokens";
    public static final String IS_ACTIVE = "isActive";
    public static final String SEARCH_TERM_FIELD = "searchTerm";
    public static final String TENANT_IDS = "tenantIds";
    public static final String COUNTRY_ID = "countryId";
    public static final String ENTITY_LIST = "entityList";
    public static final String VALUE = "value";
    public static final String COUNTRY_IDS = "countryIds";

    // Tenant
    public static final String AUTHORIZATION_HEADER = "Authorization";
    public static final String BEARER_SCHEMA = "Bearer ";
    public static final String TENANT_IDS_CLAIM = "tenantId";
    public static final String ROLE_ID_CLAIM = "roleId";
    public static final String ROLE_NAME_CLAIM = "roleName";
    public static final String HEADER_TENANT_ID = "TenantId";

    public static final String TENANT_FILTER_NAME = "tenantFilter";
    public static final String TENANT_PARAMETER_NAME = "tenantId";
    public static final String TENANT_PARAMETER_TYPE = "int";
    public static final String TENANT_COLUMN_NAME = "tenant_id";

    public static final int SUPER_ADMIN_ROLE_ID = 0;
    public static final String SUPER_ADMIN_ROLE_NAME = "ROLE_SUPER_ADMIN";
    public static final String ADMIN_ROLE_NAME = "ROLE_ADMIN";
    public static final String USER_ROLE_NAME = "ROLE_USER";

    public static final int SUPER_TENANT_ADMINISTRATOR_ID = 0;
    public static final String USER_SELECTED_TENANT = "userTenant";

    // Spring Components and warning
    public static final String SERIAL = "serial";
    public static final String ERROR_MESSAGE_PROPERTY = "classpath:error_messages.properties";
    public static final String RAWTYPES = "rawtypes";
    public static final String UNCHECKED = "unchecked";
    public static final String UNUSED = "unused";
    public static final String PASSWORD_ENCODER = "passwordEncoder";

    public static final String TEMPLATE_MESSAGE_PROPERTY = "classpath:template_message.properties";

    // Date Formate
    public static final String TEN_DIGITS_DECIMAL_FORMAT = "#.##########";
    public static final String COMMON_DATE_FORMAT = "yyyy-MM-dd";
    public static final String WEEK = "wk";
    public static final String DATE_COLUMN = "date";
    public static final String DAY = "d";
    public static final String MONTH = "mo";
    public static final String YEARS = "YEARS";
    public static final String END = "End";
    public static final String BEGIN = "Begin";
    public static final String MMM_YY_FORMAT = "MMM-yy";
    public static final String DD_MM_YYYY = "dd-mm-yyyy";
    public static final String TIMEZONE_UTC = "UTC";
    public static final String ACTIVE = "active";


    public static final String NEW = "NEW";
    public static final String MEDICAL_REVIEW_COMPLETED = "MEDICAL_REVIEW_COMPLETED";

    public static final String TABLE_USER_TOKEN = "user_token";
    public static final String LAST_SESSION_TIME = "last_session_time";
    public static final String TYPE = "type";
    public static final String APP_TYPE = "appType";
    public static final String FORMS = "forms";
    public static final String OPTIONS = "options";
    public static final String QUESTIONS = "questions";
    public static final String TEMPLATE_TYPE_ENROLL_PATIENT = "ENROLL_PATIENT";
    public static final Object ENTITY = "entity";
    public static final String TEMPLATE_TYPE_RED_RISK = "RED_RISK";

    // Lifestyle type
    public static final String SMOKE = "smoke";
    public static final String ALCOHOL = "alcohol";
    public static final String NUT = "nut";
    public static final String HOURS_PER_WEEK = "hrs/week";

    public static final String DISTRICT = "district";
    public static final String REGION = "Country";
    public static final String CHIEFDOM = "chiefdom";
    public static final String COUNTRY = "country";
    public static final String SESSION_ALIVE = "Session alive";
    public static final String COUNT = "count";
    public static final String DATA = "data";
    public static final String REFRESH_TOKEN = "RefreshToken";
    public static final String EMAIL_FORM_USER = "User";
    public static final String CREATED_AT = "createdAt";
    public static final String SEARCH_TERM = "[^a-zA-Z0-9-()\\s]*";
    public static final String USER_SEARCH_TERM = "[^[^_.@%]a-zA-Z0-9-\\s]*";

    public static final String CLIENT_WEB = "web";
    public static final String CLIENT_ADMIN = "admin";
    public static final String CLIENT_INSIGHTS = "insights";
    public static final String CLIENT_CFR = "cfr";
    public static final String CLIENT_CFR_USER = "cfr_user";
    public static final String CLIENT_CFR_ADMIN = "cfr_admin";
    public static final String CLIENT_CFR_QUICKSIGHT_ADMIN = "cfr_quicksight_admin";


    public static final String CLIENT_JOB = "job";
    public static final String CLIENT_SPICE_MOBILE = "mob";
    public static final String CLIENT_SPICE_WEB = "spice web";
    public static final String CLIENT_CFR_WEB = "cfr web";
    public static final List<String> WEB_CLIENTS = List.of(CLIENT_ADMIN, CLIENT_CFR, CLIENT_INSIGHTS, CLIENT_CFR_USER, CLIENT_CFR_ADMIN, CLIENT_CFR_QUICKSIGHT_ADMIN);
    public static final List<String> MOB_CLIENTS = List.of(Constants.CLIENT_SPICE_MOBILE);
    public static final List<String> JOB_CLIENTS = List.of(Constants.CLIENT_JOB);
    public static final String ROLE_HEALTH_COACH = "HEALTH_COACH";
    public static final String ROLE_HEALTH_SCREENER = "HEALTH_SCREENER";
    public static final String ROLE_HRIO = "HRIO";
    public static final String ROLE_LAB_TECHNICIAN = "LAB_TECHNICIAN";
    public static final String ROLE_NURSE = "NURSE";
    public static final String ROLE_NUTRITIONIST = "NUTRITIONIST";
    public static final String ROLE_PHARMACIST = "PHARMACIST";
    public static final String ROLE_PHYSICIAN_PRESCRIBER = "PHYSICIAN_PRESCRIBER";
    public static final String ROLE_PROVIDER = "PROVIDER";
    public static final String ROLE_RED_RISK_USER = "RED_RISK_USER";
    public static final String ROLE_COUNSELOR = "COUNSELOR";
    public static final String ROLE_CHP = "COMMUNITY_HEALTH_PROMOTER";
    public static final String ROLE_CHA = "COMMUNITY_HEALTH_ASSISTANT";
    public static final String ROLE_CHW = "CHW";

    public static final Map<String, String> SPICE_MOBILE_ROLES = Map.ofEntries(
            entry(ROLE_HEALTH_COACH, ROLE_HEALTH_COACH),
            entry(ROLE_HEALTH_SCREENER, ROLE_HEALTH_SCREENER),
            entry(ROLE_HRIO, ROLE_HRIO),
            entry(ROLE_LAB_TECHNICIAN, ROLE_LAB_TECHNICIAN),
            entry(ROLE_NURSE, ROLE_NURSE),
            entry(ROLE_NUTRITIONIST, ROLE_NUTRITIONIST),
            entry(ROLE_PHARMACIST, ROLE_PHARMACIST),
            entry(ROLE_PROVIDER, ROLE_PROVIDER),
            entry(ROLE_PHYSICIAN_PRESCRIBER, ROLE_PHYSICIAN_PRESCRIBER),
            entry(ROLE_COUNSELOR, ROLE_COUNSELOR),
            entry(ROLE_CHP, ROLE_CHP),
            entry(ROLE_CHW, ROLE_CHW),
            entry(ROLE_CHA, ROLE_CHA)
    );


    public static final String ROLE_COMMUNITY_HEALTH_ASSISTANT = "COMMUNITY_HEALTH_ASSISTANT";
    public static final String PEER_SUPERVISOR = "PEER_SUPERVISOR";
    public static final String ROLE_DISTRICT_ADMIN = "DISTRICT_ADMIN";
    public static final String ROLE_CHIEFDOM_ADMIN = "CHIEFDOM_ADMIN";
    public static final String ROLE_REGION_ADMIN = "REGION_ADMIN";
    public static final String ROLE_REPORT_ADMIN = "REPORT_ADMIN";
    public static final String ROLE_FACILITY_REPORT_ADMIN = "FACILITY_REPORT_ADMIN";
    public static final String ROLE_SUPER_ADMIN = "SUPER_ADMIN";
    public static final String ROLE_SYSTEM_ADMIN = "SYSTEM_ADMIN";
    public static final String ROLE_SUPER_USER = "SUPER_USER";
    public static final String ROLE_JOB_USER = "JOB_USER";
    public static final String ROLE_REPORT_SUPER_ADMIN = "REPORT_SUPER_ADMIN";
    public static final String CFR_DISTRICT_ADMIN = "CFR_DISTRICT_ADMIN";
    public static final String CFR_HEALTH_FACILITY_ADMIN = "CFR_HEALTH_FACILITY_ADMIN";
    public static final String CFR_CHIEFDOM_ADMIN = "CFR_CHIEFDOM_ADMIN";
    public static final String CFR_REGION_ADMIN = "CFR_REGION_ADMIN";
    public static final String CFR_REPORT_ADMIN = "CFR_REPORT_ADMIN";

    public static final String ROLE_HEALTH_FACILITY_ADMIN = "HEALTH_FACILITY_ADMIN";
    public static final Map<String, String> SPICE_WEB_ROLES = Map.of(
            ROLE_CHIEFDOM_ADMIN, ROLE_CHIEFDOM_ADMIN, ROLE_REGION_ADMIN, ROLE_REGION_ADMIN,
            ROLE_REPORT_ADMIN, ROLE_REPORT_ADMIN, ROLE_SUPER_ADMIN, ROLE_SUPER_ADMIN, ROLE_SYSTEM_ADMIN,
            ROLE_SYSTEM_ADMIN, ROLE_SUPER_USER, ROLE_SUPER_USER, ROLE_HEALTH_FACILITY_ADMIN, ROLE_HEALTH_FACILITY_ADMIN,
            ROLE_DISTRICT_ADMIN, ROLE_DISTRICT_ADMIN);

    public static final Map<String, String> SPICE_CFR_ROLES = Map.of(CFR_DISTRICT_ADMIN, CFR_DISTRICT_ADMIN, CFR_HEALTH_FACILITY_ADMIN, CFR_HEALTH_FACILITY_ADMIN, CFR_CHIEFDOM_ADMIN, CFR_CHIEFDOM_ADMIN,
            CFR_REGION_ADMIN, CFR_REGION_ADMIN, CFR_REPORT_ADMIN, CFR_REPORT_ADMIN);

    public static final String ROLE_CFR_REPORT_ADMIN = "CFR_REPORT_ADMIN";
    public static final String ROLE_CFR_REGION_ADMIN = "CFR_REGION_ADMIN";
    public static final String ROLE_CFR_DISTRICT_ADMIN = "CFR_DISTRICT_ADMIN";
    public static final String ROLE_CFR_CHIEFDOM_ADMIN = "CFR_CHIEFDOM_ADMIN";
    public static final String ROLE_CFR_HEALTH_FACILITY_ADMIN = "CFR_HEALTH_FACILITY_ADMIN";
    public static final String ROLE_CFR_QUICKSIGHT_ADMIN = "CFR_QUICKSIGHT_ADMIN";
    public static final String ROLE_CFR_USER_HEALTH_FACILITY_ADMIN = "CFR_USER_HEALTH_FACILITY_ADMIN";
    public static final String ROLE_CFR_HEALTH_FACILITY_USER = "CFR_HEALTH_FACILITY_USER";
    public static final Map<String, String> CFR_WEB_ROLES = Map.of(ROLE_CFR_REPORT_ADMIN, ROLE_CFR_REPORT_ADMIN,
            ROLE_CFR_REGION_ADMIN, ROLE_CFR_REGION_ADMIN, ROLE_CFR_DISTRICT_ADMIN, ROLE_CFR_DISTRICT_ADMIN, ROLE_CFR_CHIEFDOM_ADMIN, ROLE_CFR_CHIEFDOM_ADMIN, ROLE_CFR_HEALTH_FACILITY_ADMIN, ROLE_CFR_HEALTH_FACILITY_ADMIN,
            ROLE_CFR_QUICKSIGHT_ADMIN, ROLE_CFR_QUICKSIGHT_ADMIN, ROLE_CFR_USER_HEALTH_FACILITY_ADMIN, ROLE_CFR_USER_HEALTH_FACILITY_ADMIN, ROLE_CFR_HEALTH_FACILITY_USER, ROLE_CFR_HEALTH_FACILITY_USER);


    public static final String PARENT_ORGANIZATION_ID = "parentOrganizationId";
    public static final String INPUT_ID = "in_id";

    public static final String EXPIRES = "?expires=";
    public static final String RESET_PASSWORD = "&reset_password=true";




    public static final String SPICE = "spice";
    public static final String GROUP_NAME_SPICE = "SPICE";
    public static final String GROUP_NAME_SPICE_INSIGHTS = "SPICE INSIGHTS";
    public static final String GROUP_NAME_REPORTS = "REPORTS";
    public static final String LOGIN = "login";

    public static final String ACTIVATE = "Activate";
    public static final String DEACTIVATE = "Deactivate";
    public static final String LIST_ARRAY = "list-array";
    public static final String COLUMN_DEFINITION_TEXT = "text[]";
    public static final int MINUS_ONE = -1;
    public static final long MINUS_ONE_LONG = -1;

    public static final String CATEGORY_TWO = "category2";
    public static final String CATEGORY_THREE = "category3";
    public static final String CATEGORY_FOUR = "category4";
    public static final String CATEGORY_FIVE = "category5";
    public static final String CATEGORY_TWO_COUNT = "category2count";
    public static final String CATEGORY_THREE_COUNT = "category3count";
    public static final String CATEGORY_FOUR_COUNT = "category4count";
    public static final String CATEGORY_FIVE_COUNT = "category5count";
    public static final String COLUMN_DEFINITION_VARCHAR = "varchar[]";


    public static final String ZONED_UTC_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSxxx";
    public static final String PATIENT_SEARCH_REGEX = "^[0-9a-zA-Z@._-]+$";
    public static final String NUMBER_REGEX = "[0-9]+";
    public static final String NUMBER_ZERO_REGEX = "0+";
    public static final String EMAIL_REGEX = "^(.+)@(.+)$";
    public static final String PHONE_NUMBER_REGEX = "[0-9]+";
    public static final String SCREEN_TYPES = "screenTypes";
    public static final String CLINICAL_WORKFLOW_ID = "clinicalWorkflowId";
    public static final String REGEX_SEARCH_PATTERN = "[^a-zA-Z0-9]*";
    public static final String START_DATE = "startDate";
    public static final String END_DATE = "endDate";

    public static final String FORM = "form";
    public static final String DISPLAY_ORDER = "displayOrder";
    public static final String CATEGORY = "category";
    public static final String FORM_LAYOUT = "formLayout";
    public static final String FAMILY = "family";
    public static final String PARENT_FAMILY = "parentFamily";
    public static final String SITE_ID = "siteId";

    public static final String LOGOUT_MESSAGE = "Logout Successfully";
    public static final String IGNORE_TENANT = "ignoreTenantId";


    public static final String FORM_NAME = "formName";
    public static final String NAME = "name";
    public static final String STATUS = "status";
    public static final String CHILD_ORG_IDS = "childOrgIds";
    public static final String MATCH_LEVEL = "matchLevel";
    public static final String CITY = "city";
    public static final String STATE = "state";
    public static final String LOCATION_ID = "locationId";
    public static final String SUGGESTIONS = "suggestions";
    public static final String QUERY = "&q=";
    public static final String LOCATION_ID_URL = "&locationId=";
    public static final String LOCATION = "Location";
    public static final String MAX_RESULT_LIMIT = "&maxresults=20";
    public static final String RESPONSE = "Response";
    public static final String VIEW = "View";
    public static final String RESULT = "Result";
    public static final Number SUCCESS_STATUS_CODE = 200;
    public static final String DISPLAY_POSITION = "DisplayPosition";
    public static final String ADDRESS = "Address";
    public static final String GEOCODE_URL = "https://geocoder.ls.hereapi.com/6.2/geocode.json?apiKey=";
    public static final String HERE_MAP_AUTOCOMPLETE_URL = "https://autocomplete.search.hereapi.com/v1/autocomplete?apiKey=";
    public static final String RESULT_TYPE = "resultType";
    public static final String LABEL = "label";
    public static final String LOCALITY = "locality";
    public static final String CITY_ADDRESS = "address";
    public static final String ITEMS = "items";
    public static final String TYPES_QUERY_PARAM = "&types=area&limit=20";
    public static final List<String> IGNORE_LOGS = List.of("/static-data", "/timezone", "/static-data/medical-review");
    public static final String METHOD_POST = "POST";
    public static final String METHOD_PUT = "PUT";
    public static final String METHOD_PATCH = "PATCH";
    public static final String METHOD_GET = "GET";
    public static final String METHOD_DELETE = "DELETE";
    public static final String DATE_FORMAT_OLD_SCREENING_LOG = "yyyy-MM-dd'T'HH:mm:ss";
    public static final String USERNAME = "username";
    public static final String PASSWORD = "password";
    public static final String ROLE_LEVEL = "role level";
    public static final String LOGGED_USER_ROLE_NAME = "logged user name";
    public static final String ROLE_REDIS_KEY = "roles";
    public static final String VILLAGE = "village";
    public static final String USERS = "users";
    public static final String ORGANIZATION_REDIS_KEY = "organization";

    public static final String CREATED_BY = "createdBy";
    public static final String UPDATED_BY = "updatedBy";
    public static final String CREDENTIALS_EXPIRED = "credentialsExpired";
    public static final String CURRENT_DATE = "currentDate";
    public static final String DEVICE_INFO_ID = "deviceInfoId";
    public static final String IS_BLOCKED = "isBlocked";
    public static final String ACCOUNT_LOCKED = "accountLocked";
    public static final String COUNTRY_CODE = "countryCode";
    public static final String ACCOUNT_EXPIRED = "accountExpired";
    public static final String PARAM_PHONE_NUMBER = "phoneNumber";
    public static final String MIDDLE_NAME = "middleName";
    public static final String GENDER = "gender";
    public static final String OTHER = "other";
    public static final String ID = "id";
    public static final String CLINICAL = "clinical";

    public static final String IS_PASSWORD_SET = "isPasswordSet";
    public static final String SPICE_SERVICE = "SPICE_SERVICE";
    public static final String ADMIN_SERVICE = "ADMIN_SERVICE";
    public static final String USER_SERVICE = "USER_SERVICE";
    public static final String AUTH_SERVICE = "AUTH_SERVICE";
    public static final String NOTIFICATION_SERVICE = "NOTIFICATION_SERVICE";
    public static final String OFFLINE_SERVICE = "OFFLINE_SERVICE";
    public static final String FHIR_MAPPER_SERVICE = "FHIR_MAPPER_SERVICE";
    public static final String CQL_SERVICE = "CQL_SERVICE";
    public static final String CFR_SERVICE = "CFR_SERVICE";
    public static final String CONNECT_SERVICE = "CONNECT_SERVICE";
    public static final String OPEN_URI = "OPEN_URI";
    public static final String API_PERMISSION = "API_PERMISSION";
    public static final String API_PERMISSION_KEY = "api_permission";
    public static final String OPEN_URI_KEY = "open_uri";
    public static final String FORM_NAME_COUNTRY = "country";
    public static final String FORM_NAME_HEALTH_FACILITY  = "healthfacility";
    public static final String FHIR_MAPPER_OPENAPI_TITLE ="FhirMapper API";
    public static final String NOTIFICATION_SERVICE_OPENAPI_TITLE ="Notification Service API";
    public static final String OFFLINE_SERVICE_OPENAPI_TITLE ="Offline service API";
    public static final String SPICE_SERVICE_OPENAPI_TITLE ="Spice Service API";
    public static final String ADMIN_SERVICE_OPENAPI_TITLE="Admin Service API";
    public static final String USER_SERVICE_OPENAPI_TITLE="User Service API";
    public static final String CFR_SERVICE_OPENAPI_TITLE="Cfr Service API";
    public static final String CONNECT_SERVICE_OPENAPI_TITLE="Connect Service API";
    public static final String OPEN_API_VERSION="1.0.0";
    public static final String DISTRICT_ID="districtId";
    public static final String DISTRICT_IDS = "districtIds";
    public static final String FORM_NAME_DISTRICT  = "district";
    public static final String CHIEFDOM_IDS = "chiefdomIds";
    public static final String HEALTH_FACILITY_IDS = "healthFacilityIds";
    public static final String CHIEFDOM_ID = "chiefdomId";
    public static final int NUMBER_TEN = 10;
    public static final String FORM_NAME_CHIEFDOM  = "chiefdom";



    // Digimiles sms
    public static final String USERNAME_URL = "?username=";
    public static final String PASSWORD_URL = "&password=";
    public static final String TYPE_URL = "&type=";
    public static final String MESSAGE_URL = "&message=";
    public static final String DLR_URL = "&dlr=";
    public static final String DESTINATION_URL = "&destination=";
    public static final String SOURCE_URL = "&source=";
    public static final Map<String, String> RESPONSE_CODES = Map.ofEntries(
        Map.entry("1701", " Success, Message Submitted successfully"),
        Map.entry("1702", "Invalid URL Error"),
        Map.entry("1703", "Invalid value in username or password field"),
        Map.entry("1704", "Invalid value in \"type\" field"),
        Map.entry("1705", "Invalid Message"),
        Map.entry("1706", "Invalid Destination"),
        Map.entry("1707", "Invalid Source (Sender)"),
        Map.entry("1708", "Invalid value for \"dlr\" field"),
        Map.entry("1709", "User validation failed"),
        Map.entry("1710", "Internal Error"),
        Map.entry("1725", "Insufficient Credit"),
        Map.entry("1728", "Spam message content"),
        Map.entry("1742", "Number blocked at operator end")
    );
    public static final String SUCCESS_CODE_DIGIMILES = "1701";
    public static final String DIGIMILES_REGEX = "\\|";
    public static final String SMS = "sms";
    public static final String URL= "url";
    public static final String DEV_ENVIRONMENT = "dev";
    public static final String LATE_PRETERM = "Late Preterm";
    public static final String MODERATE_PRETERM = "Moderate Preterm";
    public static final String VERY_PRETERM = "Very Preterm";
    public static final String EXTREMELY_PRETERM = "Extremely Preterm";
    public static final String EARLY_TERM = "Early Term";
    public static final String FULL_TERM = "Full Term";
    public static final String LATE_TERM = "Late Term";
    public static final String POST_TERM = "Post Term";
    public static final String LBW = "Low Birth Weight";
    public static final String VLBW = "Very Low Birth Weight";
    public static final String ELBW = "Extremely Low Birth Weight";
    public static final String NBW = "Normal Birth Weight";
    public static final String HBW = "High Birth Weight";
    public static final String HEALTHFACILITY_ID = "healthFacilityId";
    public static final String PERFORMANCE_MONITORING = "Performance Monitoring";
    public static final String APP_VERSION = "App-Version";
    public static final String LETTER_TEXT ="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    public static final String U_UPDATED_AT = "u.updatedAt";

    public static final List<String> SPICE_ADMIN_ROLE_FILTER = List.of(ROLE_REGION_ADMIN, ROLE_DISTRICT_ADMIN, ROLE_CHIEFDOM_ADMIN, ROLE_HEALTH_FACILITY_ADMIN);
    public static final List<String> SPICE_MOBILE_ROLE_FILTER = List.of(ROLE_HEALTH_COACH, ROLE_HEALTH_SCREENER, ROLE_HRIO,
            ROLE_LAB_TECHNICIAN, ROLE_NURSE, ROLE_NUTRITIONIST, ROLE_PHARMACIST, ROLE_PROVIDER,
            ROLE_PHYSICIAN_PRESCRIBER, ROLE_COUNSELOR, ROLE_CHP, ROLE_CHA);
    public static final String CUSTOMIZED = "customized";
    public static final String SUPER_USER_USERNAME = "superuser@test.com";
    public static final String SHORT_URL ="shortUrl";
    public static final String APP ="SPICE SL";

    // Patient transfer
    public static final String INCOMINGPATIENTLIST = "incomingPatientList";
    public static final String OUTGOINGPATIENTLIST = "outgoingPatientList";
    public static final String TRANSFERSTATUS = "transferStatus";
    public static final String TRANSFERREASON = "transferReason";
    public static final String PATIENT = "patient";
    public static final String TRANSFERBY = "transferBy";
    public static final String TRANSFERTO = "transferTo";
    public static final String OLDSITE = "oldSite";
    public static final String TRANSFERSITE = "transferSite";
    public static final String PARAM_FIRST_NAME = "firstName";
    public static final String PARAM_LAST_NAME = "lastName";
    public static final String AGE = "age";
    public static final String IDENTITY_VALUE = "identityValue";
    public static final String CONFIRM_DIAGNOSIS = "confirmDiagnosis";
    public static final String PROGRAM_ID = "programId";
    public static final String CVD_RISK_LEVEL = "cvdRiskLevel";
    public static final String ENROLLMENT_AT = "enrollmentAt";
    public static final String ASSESSMENT = "assessment";

    public static final String APP_TYPE_COMMUNITY = "COMMUNITY";
    public static final String APP_TYPE_NON_COMMUNITY = "NON_COMMUNITY";
    public static final String SUPERSET_ACCESS_TOKEN_KEY = "access_token";
    public static final String SUPERSET_LOGIN_PROVIDER_NAME = "db";
    public static final String SUPERSET_ID_KEY = "supersetId";
    public static final String INSIGHTS = "INSIGHTS";

    // others
    public static final String INSTRUCTIONS = "instructions";
    public static final String APP_TYPES = "appTypes";
    public static final String OTHER_VIILAGE = "Other";

    public static final String AWS_ENVIRONMENT_EKS = "eks";
    public static final String CVD_RISK_SCORE_DISPLAY = "cvdRiskScoreDisplay";
    public static final String CVD_RISK_SCORE = "cvdRiskScore";
    public static final String PROVISIONAL_DIAGNOSIS = "provisionalDiagnosis";
    public static final String RED_RISK_PATIENT = "isRedRiskPatient";
    public static final String BMI = "bmi";
    public static final String PREGNANCY_DETAILS = "pregnancyDetails";

    public static final String OSM_CITY_NAME_URL = "https://nominatim.openstreetmap" +
            ".org/search?format=json&accept-language=en&q=";
    public static final String ADDRESSTYPE = "addresstype";
    public static final String DISPLAY_NAME = "display_name";
    public static final String PLACE_ID = "place_id";

}
