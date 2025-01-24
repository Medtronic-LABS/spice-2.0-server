CREATE TABLE organization (
    id SERIAL PRIMARY KEY,
    form_data_id BIGINT,
    name VARCHAR,
    sequence BIGINT,
    form_name VARCHAR,
    parent_organization_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE country (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    phone_number_code VARCHAR,
    region_code VARCHAR,
    unit_measurement VARCHAR,
    app_types VARCHAR [],
    tenant_id BIGINT,
	display_values jsonb NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE timezone (
    id SERIAL PRIMARY KEY,
    description VARCHAR,
    abbreviation VARCHAR,
    "offset" VARCHAR,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE district(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    code VARCHAR,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    status VARCHAR,
    reason VARCHAR,
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE chiefdom(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    code VARCHAR,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    district_id BIGINT,
    FOREIGN KEY (district_id) REFERENCES district(id),
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE culture (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    code VARCHAR,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE health_facility(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    address VARCHAR,
    phu_focal_person_name VARCHAR,
    phu_focal_person_number VARCHAR,
    latitude VARCHAR,
    longitude VARCHAR,
    postal_code VARCHAR,
    language VARCHAR,
    city_name VARCHAR,
    code VARCHAR,
    district_id BIGINT,
    FOREIGN KEY (district_id) REFERENCES district(id),
    chiefdom_id BIGINT,
    FOREIGN KEY (chiefdom_id) REFERENCES chiefdom(id),
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    tenant_id BIGINT,
    fhir_id VARCHAR,
    culture_id BIGINT,
    FOREIGN KEY (culture_id) REFERENCES culture(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE village(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    code VARCHAR,
    type VARCHAR,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    district_id BIGINT,
    FOREIGN KEY (district_id) REFERENCES district(id),
    chiefdom_id BIGINT,
    FOREIGN KEY (chiefdom_id) REFERENCES chiefdom(id),
    health_facility_id BIGINT,
    FOREIGN KEY (health_facility_id) REFERENCES health_facility(id),
    tenant_id BIGINT,
    member_sequence BIGINT,
    household_sequence BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE role (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    level BIGINT,
    display_name VARCHAR,
    suite_access_name VARCHAR,
    group_name VARCHAR,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE designation (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    country_id BIGINT,
    role_id BIGINT,
    FOREIGN KEY (role_id) REFERENCES role(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE "user" (
    id SERIAL PRIMARY KEY,
    first_name VARCHAR,
    last_name VARCHAR,
    middle_name VARCHAR,
    gender VARCHAR,
    phone_number VARCHAR,
    address VARCHAR,
    username VARCHAR,
    password VARCHAR,
    is_blocked BOOLEAN,
    blocked_date TIMESTAMP,
    country_code VARCHAR,
    forget_password_token VARCHAR,
    forget_password_short_token VARCHAR,
    forget_password_time TIMESTAMP WITH TIME ZONE DEFAULT (CURRENT_TIMESTAMP),
    forget_password_count INT,
    invalid_login_attempts INT,
    last_logged_in TIMESTAMP WITH TIME ZONE,
    suite_access VARCHAR [],
    invalid_login_time TIMESTAMP DEFAULT (CURRENT_TIMESTAMP),
    invalid_reset_time TIMESTAMP DEFAULT (CURRENT_TIMESTAMP),
    is_password_reset_enabled BOOLEAN,
    password_reset_attempts INT,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    timezone_id BIGINT,
    FOREIGN KEY (timezone_id) REFERENCES timezone(id),
    culture_id BIGINT,
    FOREIGN KEY (culture_id) REFERENCES culture(id),
    tenant_id BIGINT,
    fhir_id VARCHAR,
    community_unit_id BIGINT,
    insight_id BIGINT,
    designation_id BIGINT,
    FOREIGN KEY (designation_id) REFERENCES designation(id),
    is_terms_and_conditions_accepted BOOLEAN,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE user_role (
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    role_id BIGINT,
    FOREIGN KEY (role_id) REFERENCES role(id)
);

CREATE TABLE user_organization (
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    organization_id BIGINT,
    FOREIGN KEY (organization_id) REFERENCES organization(id)
);

CREATE TABLE user_village (
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    village_id BIGINT,
    FOREIGN KEY (village_id) REFERENCES village(id)
);

CREATE TABLE user_supervisor (
    id SERIAL PRIMARY KEY,
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    supervisor_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE user_token (
    id SERIAL PRIMARY KEY,
    user_id BIGINT,
    auth_token VARCHAR,
    client VARCHAR,
    app_version VARCHAR,
    last_logged_in TIMESTAMP WITH TIME ZONE,
    last_logged_out TIMESTAMP WITH TIME ZONE,
    created_by BIGINT,
    updated_by BIGINT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE health_facility_village(
    health_facility_id BIGINT,
    FOREIGN KEY (health_facility_id) REFERENCES health_facility(id),
    village_id BIGINT,
    FOREIGN KEY (village_id) REFERENCES village(id)
);

CREATE TABLE api_role_permission (
    id SERIAL PRIMARY KEY,
    method VARCHAR,
    api VARCHAR,
    roles VARCHAR [],
    type VARCHAR,
    service_name VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE clinical_workflow(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    module_type VARCHAR,
    workflow_name VARCHAR,
    display_order BIGINT,
    conditions jsonb,
    view_screens VARCHAR [],
    is_default BOOLEAN,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    group_name VARCHAR,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE health_facility_clinical_workflow(
    health_facility_id BIGINT,
    FOREIGN KEY (health_facility_id) REFERENCES health_facility(id),
    clinical_workflow_id BIGINT,
    FOREIGN KEY (clinical_workflow_id) REFERENCES clinical_workflow(id)
);

CREATE TABLE health_facility_customized_workflow(
    health_facility_id BIGINT,
    FOREIGN KEY (health_facility_id) REFERENCES health_facility(id),
    customized_workflow_id BIGINT,
    FOREIGN KEY (customized_workflow_id) REFERENCES clinical_workflow(id)
);

CREATE TABLE workflow_customization (
    id SERIAL PRIMARY KEY,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    district_id BIGINT,
    FOREIGN KEY (district_id) REFERENCES district(id),
    workflow VARCHAR,
    type VARCHAR,
    category VARCHAR,
    form_input VARCHAR,
    clinical_workflow_id BIGINT,
    FOREIGN KEY (clinical_workflow_id) REFERENCES clinical_workflow(id),
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE menu(
    id SERIAL PRIMARY KEY,
    role_name VARCHAR,
    menu jsonb,
    meta VARCHAR [],
    meta_forms VARCHAR [],
    json_display_values jsonb,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE form_meta (
    id SERIAL PRIMARY KEY,
    form_input VARCHAR,
    form_type VARCHAR,
    clinical_workflow_id BIGINT,
    FOREIGN KEY (clinical_workflow_id) REFERENCES clinical_workflow(id),
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE symptom (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    disease_type VARCHAR,
    value VARCHAR,
    categories jsonb,
    category VARCHAR,
    display_order BIGINT,
    display_values jsonb,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE meta_code_details (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    codes jsonb,
    text VARCHAR,
    workflow VARCHAR,
    resource VARCHAR,
    observation_type VARCHAR,
    resource_property VARCHAR,
    resource_property_value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE medication(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    classification_id BIGINT,
    brand_id BIGINT,
    dosage_form_id BIGINT,
    brand_name VARCHAR,
    classification_name VARCHAR,
    dosage_form_name VARCHAR,
    country_id BIGINT,
    codes jsonb,
    category VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE classification(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE brand(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE dosage_form(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order VARCHAR,
    display_values jsonb,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE country_classification(
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    classification_id BIGINT,
    FOREIGN KEY (classification_id) REFERENCES classification(id)
);

CREATE TABLE classification_brand(
    brand_id BIGINT,
    FOREIGN KEY (brand_id) REFERENCES brand(id),
    classification_id BIGINT,
    FOREIGN KEY (classification_id) REFERENCES classification(id)
);

CREATE TABLE health_facility_type(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE public.offline_sync (
    id SERIAL PRIMARY KEY,
    request_data jsonb,
    "type" VARCHAR(255),
    request_id VARCHAR(255),
    request_time TIMESTAMP WITH TIME ZONE,
    fhir_id VARCHAR(255),
    app_version VARCHAR(255),
    status VARCHAR(255),
    retry_attempts INT DEFAULT 0 NULL,
    error_message VARCHAR,
    reference_id VARCHAR(255),
    device_id VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE offline_sync_log (
    id SERIAL PRIMARY KEY,
    request_data jsonb,
    app_version VARCHAR(255),
    last_sync_time TIMESTAMP WITH TIME ZONE,
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    status VARCHAR(255),
    error_message VARCHAR,
    device_id VARCHAR,
    response_data jsonb,
    method_name VARCHAR(255),
    sync_mode VARCHAR(255),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE call_register (
    id SERIAL PRIMARY KEY,
    patient_id VARCHAR(255),
    patient_status VARCHAR(255),
    reason VARCHAR(255),
    encounter_type VARCHAR(255),
    household_id VARCHAR(255),
    member_id VARCHAR(255),
    type VARCHAR(255),
    encounter_id VARCHAR(255),
    encounter_name VARCHAR(255),
    village_id VARCHAR(255),
    referred_site_id VARCHAR(255),
    next_visit_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    encounter_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    called_at BIGINT,
    attempts BIGINT DEFAULT 0,
    visits BIGINT DEFAULT 0,
    is_wrong_number BOOLEAN DEFAULT false,
    is_completed BOOLEAN DEFAULT false,
    is_initiated BOOLEAN DEFAULT false,
    screening_date_time TIMESTAMP WITH TIME ZONE,
    next_bp_assessment_date TIMESTAMP WITH TIME ZONE,
    next_bg_assessment_date TIMESTAMP WITH TIME ZONE,
    next_medical_review_date TIMESTAMP WITH TIME ZONE,
    first_interaction_mode VARCHAR(255),
    last_interaction_mode VARCHAR(255),
    user_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE call_register_detail (
    id SERIAL PRIMARY KEY,
    call_register_id BIGINT,
    FOREIGN KEY (call_register_id) REFERENCES call_register(id),
    call_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    attempts BIGINT,
    duration double precision,
    status VARCHAR(255),
    reason VARCHAR(255),
    patient_status VARCHAR(255),
    latitude VARCHAR(255),
    longitude VARCHAR(255),
    visited_facility_id VARCHAR(255),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE cql_result (
    id SERIAL PRIMARY KEY NOT NULL,
    resource_id VARCHAR NOT NULL,
    patient_id VARCHAR NULL,
    member_id VARCHAR NULL,
    results json NOT NULL,
    village_id VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false,
    is_latest BOOLEAN NOT NULL
);

CREATE TABLE country_role (
    id SERIAL PRIMARY KEY,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    role_id BIGINT,
    FOREIGN KEY (role_id) REFERENCES role(id),
    display_name VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE user_preferences (
    id SERIAL PRIMARY KEY,
    type character varying NOT NULL,
    preference jsonb NULL,
    user_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE lab_test_customization (
    id SERIAL PRIMARY KEY,
    test_name VARCHAR NULL,
    unique_name VARCHAR NULL,
    form_input VARCHAR NULL,
    country_id INT NULL,
    tenant_id INT NULL,
    codes jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE model_questions (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    is_default BOOLEAN,
    is_mandatory BOOLEAN,
    "type" VARCHAR,
    workflow VARCHAR,
    display_values jsonb,
    country_id BIGINT,
    FOREIGN KEY(country_id) REFERENCES country(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE model_answers (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    is_default BOOLEAN,
    is_mandadory BOOLEAN,
    question_id BIGINT,
    FOREIGN KEY(question_id) REFERENCES model_questions(id),
    value INT,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE form_meta_ui (
    id SERIAL PRIMARY KEY,
    components jsonb,
    form_name VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE program (
    id SERIAL PRIMARY KEY,
    name VARCHAR NOT NULL,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE outbound_sms (
    id SERIAL PRIMARY KEY,
    username VARCHAR,
    form_data_id BIGINT,
    retry_attempts INT DEFAULT 0,
    phone_number VARCHAR,
    body VARCHAR,
    is_processed BOOLEAN DEFAULT false,
    notification_id BIGINT,
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE outbound_email (
    id SERIAL PRIMARY KEY,
    tenant_id BIGINT,
    is_processed BOOLEAN,
    "to" VARCHAR,
    retry_attempts INT,
    form_data_id BIGINT,
    form_name VARCHAR,
    type VARCHAR,
    body VARCHAR,
    subject VARCHAR,
    cc VARCHAR,
    bcc VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE sms_template (
    id SERIAL PRIMARY KEY,
    body VARCHAR,
    type VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE sms_template_values (
    id SERIAL PRIMARY KEY,
    key VARCHAR,
    sms_template_id INT,
    FOREIGN KEY (sms_template_id) REFERENCES sms_template(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE email_template (
    id SERIAL PRIMARY KEY,
    type VARCHAR,
    vm_content VARCHAR,
    body VARCHAR,
    title VARCHAR,
    app_url VARCHAR,
    app_type VARCHAR,
    subject VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE email_template_value (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    email_template_id BIGINT,
    FOREIGN KEY (email_template_id) REFERENCES email_template(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE shedlock (
    name VARCHAR not null,
    lock_until VARCHAR not null,
    locked_at VARCHAR not null,
    locked_by VARCHAR not null,
    PRIMARY KEY (name)
);

CREATE TABLE presenting_complaints (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    age_condition VARCHAR,
    value VARCHAR,
    display_values jsonb,
    type VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE health_facility_program (
    health_facility_id BIGINT,
    FOREIGN KEY (health_facility_id) REFERENCES health_facility(id),
    program_id BIGINT,
    FOREIGN KEY (program_id) REFERENCES program(id)
);

CREATE TABLE deleted_health_facility_program (
    health_facility_id BIGINT,
    FOREIGN KEY (health_facility_id) REFERENCES health_facility(id),
    program_id BIGINT,
    FOREIGN KEY (program_id) REFERENCES program(id)
);

CREATE TABLE red_risk_notification (
    id SERIAL NOT NULL,
    status VARCHAR NULL,
    encounter_id VARCHAR NULL,
    member_id VARCHAR NULL,
    patient_id VARCHAR NULL,
    tenant_id INT NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true NULL,
    is_deleted BOOLEAN DEFAULT false NULL
);

CREATE TABLE systemic_examinations (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    age_condition VARCHAR,
    value VARCHAR,
    type VARCHAR,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE disease_category (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    value VARCHAR,
    type VARCHAR,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE disease_condition (
    id SERIAL PRIMARY KEY,
    disease_id BIGINT,
    FOREIGN KEY (disease_id) REFERENCES disease_category(id),
    name VARCHAR,
    display_order INT,
    value VARCHAR,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE examination (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    age_condition VARCHAR,
    form_input VARCHAR,
    type VARCHAR,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE dosage_frequency (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    description VARCHAR(255),
    display_order BIGINT DEFAULT 0,
    quantity BIGINT,
    app_types VARCHAR [],
    display_values jsonb NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE obstetric_examination (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    type VARCHAR,
    value VARCHAR,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE meta (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    description VARCHAR,
    display_order INT,
    type VARCHAR,
    display_values jsonb,
    category VARCHAR,
    value VARCHAR NULL,
    app_types VARCHAR [],
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE audit (
    id SERIAL PRIMARY KEY,
    entity VARCHAR,
    action VARCHAR,
    entity_id BIGINT,
    column_name VARCHAR,
    old_value VARCHAR,
    new_value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE patient_transfer (
    id SERIAL PRIMARY KEY,
    transfer_to BIGINT,
    FOREIGN KEY (transfer_to) REFERENCES "user"(id),
    transfer_by BIGINT,
    FOREIGN KEY (transfer_by) REFERENCES "user"(id),
    transfer_site BIGINT,
    FOREIGN KEY (transfer_site) REFERENCES health_facility(id),
    old_site BIGINT,
    FOREIGN KEY (old_site) REFERENCES health_facility(id),
    patient_fhir_id VARCHAR,
    transfer_reason VARCHAR,
    transfer_status VARCHAR,
    reject_reason VARCHAR,
    old_program_id BIGINT,
    tenant_id BIGINT,
    is_show BOOLEAN DEFAULT true,
    member_id VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE OR REPLACE FUNCTION update_virtual_id(in_tenant_id BIGINT)
RETURNS BIGINT
LANGUAGE plpgsql
AS $function$
DECLARE
    out_virtualId BIGINT;
BEGIN
    UPDATE organization
    SET "sequence" = "sequence" + 1
    WHERE id = in_tenant_id;

    SELECT "sequence" INTO out_virtualId
    FROM organization
    WHERE id = in_tenant_id;

    RETURN out_virtualId;

EXCEPTION
    WHEN OTHERS THEN
        RAISE NOTICE 'Error in updating virtual id for patient';
        RETURN -1;
END;
$function$;

ALTER TABLE
    health_facility_program REPLICA IDENTITY FULL;

ALTER TABLE
    deleted_health_facility_program REPLICA IDENTITY FULL;

CREATE TABLE community_unit (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    parent_region_id BIGINT,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE complication (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    status BOOLEAN,
    display_order INT,
    display_values jsonb,
    value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE diagnosis (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    description VARCHAR,
    display_order INT,
    type VARCHAR,
    gender VARCHAR,
    display_values jsonb,
    app_types VARCHAR [],
    value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE comorbidity (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    status BOOLEAN,
    display_order INT,
    type VARCHAR,
    display_values jsonb,
    value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE current_medication (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    status BOOLEAN,
    display_order INT,
    display_values jsonb,
    value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE lifestyle (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    answers jsonb,
    is_answer_dependent BOOLEAN,
    display_order INT,
    display_values jsonb,
    json_display_values jsonb,
    value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE medical_compliance (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    status BOOLEAN,
    display_order INT,
    is_child_exists BOOLEAN,
    display_values jsonb,
    parent_compliance_id BIGINT,
    FOREIGN KEY (parent_compliance_id) REFERENCES medical_compliance(id),
    app_types VARCHAR [],
    value VARCHAR,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE physical_examination (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    description VARCHAR,
    display_order INT,
    display_values jsonb,
    "type" VARCHAR NULL,
    value VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE complaints (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    type VARCHAR,
    display_values jsonb,
    parent_compliance_id BIGINT,
    FOREIGN KEY (parent_compliance_id) REFERENCES medical_compliance(id),
    value VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE frequency (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    duration INT,
    period VARCHAR,
    risk_level VARCHAR,
    display_order INT,
    title VARCHAR,
    display_values jsonb,
    app_types VARCHAR [] NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE nutrition_lifestyle (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    display_order INT,
    display_values jsonb,
    app_types VARCHAR [] NULL,
    value VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE risk_algorithm(
    id SERIAL PRIMARY KEY,
    risk_algorithm jsonb,
    country_id BIGINT,
    FOREIGN KEY (country_id) REFERENCES country(id),
    tenant_id BIGINT,
    created_by BIGINT,
    updated_by BIGINT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    display_order INT,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE reason(
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    display_order INT,
    display_values jsonb,
    app_types VARCHAR [] NULL,
    value VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE unit (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    description VARCHAR,
    display_values jsonb,
    display_order INT,
    app_types VARCHAR [] NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE public.frequency_type (
    id SERIAL PRIMARY KEY,
    name VARCHAR NULL,
    display_order INT,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE message (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    category VARCHAR,
    type VARCHAR,
    display_values jsonb,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE OR REPLACE PROCEDURE get_household_sequence_by_village_id(
    IN village_id BIGINT,
    OUT sequence BIGINT
)
LANGUAGE plpgsql
AS $procedure$
BEGIN
    -- Update the sequence count for the given household
    UPDATE village
    SET household_sequence = household_sequence + 1
    WHERE id = village_id;

    -- Retrieve the updated sequence count into the OUT parameter
    SELECT household_sequence
    INTO sequence
    FROM village
    WHERE id = village_id;
END;
$procedure$;

CREATE PROCEDURE get_member_sequence_by_village_id(
    IN village_id BIGINT,
    OUT sequence BIGINT
)
LANGUAGE plpgsql
AS $procedure$
BEGIN
    -- Update the sequence count for the given village
    UPDATE village
    SET member_sequence = member_sequence + 1
    WHERE id = village_id;

    -- Retrieve the updated sequence count into the OUT parameter
    SELECT member_sequence
    INTO sequence
    FROM village
    WHERE id = village_id;
END;
$procedure$;

CREATE TABLE public.wgs_data (
    id numeric NOT NULL,
    "indicator" VARCHAR NOT NULL,
    sex numeric NOT NULL,
    given numeric NOT NULL,
    l numeric NOT NULL,
    m numeric NOT NULL,
    s numeric NOT NULL,
    CONSTRAINT wgs_data_pk PRIMARY KEY (id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE public.insight_user_organization (
    user_id INT NULL,
    organization_id INT NULL,
    CONSTRAINT superset_user_organization_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organization(id),
    CONSTRAINT superset_user_organization_user_id_fkey FOREIGN KEY (user_id) REFERENCES public."user"(id)
);

CREATE TABLE public.culture_values (
    id SERIAL NOT NULL,
    form_data_id INT NULL,
    form_name VARCHAR NULL,
    culture_id INT NULL,
    culture_value VARCHAR NULL,
    json_culture_value jsonb NULL,
    CONSTRAINT culture_values_pkey PRIMARY KEY (id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE public.category (
    id SERIAL NOT NULL,
    "name" VARCHAR NULL,
    display_order INT NULL,
    CONSTRAINT category_id PRIMARY KEY (id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE household_member_link (
    id SERIAL PRIMARY KEY,
    member_id VARCHAR,
    patient_id VARCHAR,
    village_id VARCHAR,
    household_id VARCHAR,
    name VARCHAR,
    status VARCHAR,
    is_show BOOLEAN DEFAULT true,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE user_terms_and_conditions (
    id SERIAL PRIMARY KEY,
    form_input VARCHAR,
    country_id BIGINT,
    FOREIGN KEY(country_id) REFERENCES country(id),
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE customized_module(
    id SERIAL PRIMARY KEY,
    member_id VARCHAR NOT NULL,
    patient_id VARCHAR,
    clinical_workflow_id BIGINT references clinical_workflow(id),
    module_value jsonb,
    screen_type VARCHAR,
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE report_user_organization (
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    organization_id BIGINT,
    FOREIGN KEY (organization_id) REFERENCES organization(id)
);

CREATE Table device_details (
    id SERIAL PRIMARY KEY,
    name VARCHAR,
    type VARCHAR,
    model VARCHAR,
    version VARCHAR,
    aes_key VARCHAR,
    rsa_private_key VARCHAR,
    auth_tag VARCHAR,
    device_id VARCHAR,
    rsa_public_key VARCHAR,
    last_logged_in TIMESTAMP WITH TIME ZONE,
    ref_id VARCHAR,
    user_id BIGINT,
    FOREIGN KEY (user_id) REFERENCES "user"(id),
    tenant_id BIGINT,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE country_customization (
    id SERIAL PRIMARY KEY,
    form_input VARCHAR NULL,
    "type" VARCHAR NULL,
    category VARCHAR NULL,
    is_default BOOLEAN DEFAULT false NULL,
    tenant_id INT NULL,
    country_id INT NULL,
    FOREIGN KEY (country_id) REFERENCES public.country(id),
    culture_id INT NULL,
    clinical_workflow_id INT NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE INDEX idx_device_id ON device_details (device_id);

CREATE TABLE public.user_report_log (
    id SERIAL PRIMARY KEY,
    "key" VARCHAR NOT NULL,
    report_type VARCHAR NOT NULL,
    user_id int8 NOT NULL,
    FOREIGN KEY (user_id) REFERENCES public."user"(id),
    job_status VARCHAR DEFAULT 'FAILED' :: character varying NOT NULL,
    message VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE public.health_facility_report_log (
    id SERIAL PRIMARY KEY,
    "key" VARCHAR NOT NULL,
    report_type VARCHAR NOT NULL,
    health_facility_id INT NOT NULL,
    FOREIGN KEY (health_facility_id) REFERENCES public.health_facility(id),
    job_status VARCHAR DEFAULT 'FAILED' :: character varying NOT NULL,
    message VARCHAR NULL,
    khis_status VARCHAR NULL,
    khis_message VARCHAR NULL,
    created_by BIGINT NOT NULL,
    updated_by BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_deleted BOOLEAN DEFAULT false
);

CREATE TABLE url_shortener (
    id SERIAL PRIMARY KEY,
    url VARCHAR(255) NOT NULL,
    token VARCHAR(255) NOT NULL UNIQUE,
    app VARCHAR,
    env VARCHAR(255),
    visits INT DEFAULT 0,
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);