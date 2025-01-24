package com.mdtlabs.coreplatform.cqlservice.util;

public class TestConstants {

    public static final String ONE = "1";

    public static final String URL = "http://test-url.com";

    public static final String CLIENT_VALUE = "test-client";

    public static final String TOKEN = "test-token";

    public static final String HISTORY_URL = "http://test-url.com/history/1";

    public static final String REFERENCE_URL = "http://test-url.com/reference/1";

    public static final String FILE_PATH = "test-path";

    public static final String TEST_BUNDLE = "{\"resourceType\":\"Bundle\",\"id\":" +
            "\"e3b18d4b-e117-4df0-9b4c-34a6070f2904\"," +
            "\"meta\":{\"lastUpdated\":\"2024-05-31T11:39:00.457+00:00\"},\"type\":\"searchset\"," +
            "\"total\":12,\"link\":[{\"relation\":\"self\",\"url\":\"http://192.168.21" +
            ".73/fhir/Encounter/26533/$everything\"}],\"entry\":[{\"fullUrl\":\"http://192.168.21" +
            ".73/fhir/Encounter/26533\",\"resource\":{\"resourceType\":\"Encounter\",\"id\":\"26533\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-05-31T07:34:43.855+00:00\"," +
            "\"source\":\"#B1ExqwtmiFQtVOGw\"},\"contained\":[{\"resourceType\":\"Location\",\"id\":\"1\"," +
            "\"position\":{\"longitude\":0.0,\"latitude\":0.0}}],\"identifier\":[{\"system\":\"http://mdtlabs" +
            ".com/EncounterType\",\"value\":\"ANC\"},{\"system\":\"http://mdtlabs.com/visit-count\"," +
            "\"value\":\"1\"}],\"status\":\"in-progress\",\"subject\":{\"reference\":\"Patient/26513\"}," +
            "\"participant\":[{\"individual\":{\"reference\":\"RelatedPerson/26509\"}}," +
            "{\"individual\":{\"reference\":\"Practitioner/9606\"}}]," +
            "\"period\":{\"start\":\"2024-05-31T07:33:56+00:00\",\"end\":\"2024-05-31T07:33:56+00:00\"}," +
            "\"location\":[{\"location\":{\"reference\":\"#1\"}}]," +
            "\"serviceProvider\":{\"reference\":\"Organization/8850\"}},\"search\":{\"mode\":\"match\"}}," +
            "{\"fullUrl\":\"http://192.168.21.73/fhir/Provenance/26532\"," +
            "\"resource\":{\"resourceType\":\"Provenance\",\"id\":\"26532\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-05-31T07:34:43.855+00:00\",\"source\":\"#B1ExqwtmiFQtVOGw\"}," +
            "\"target\":[{\"reference\":\"Encounter/26533\"}],\"recorded\":\"2024-05-31T07:33:47.000+00:00\"," +
            "\"agent\":[{\"role\":[{\"coding\":[{\"system\":\"http://terminology.hl7" +
            ".org/CodeSystem/contractsignertypecodes\",\"code\":\"TRANS\"}],\"text\":\"POST\"}]," +
            "\"who\":{\"reference\":\"Practitioner/9606\"}," +
            "\"onBehalfOf\":{\"reference\":\"Organization/8850\"}}]},\"search\":{\"mode\":\"match\"}}," +
            "{\"fullUrl\":\"http://192.168.21.73/fhir/Observation/26537\"," +
            "\"resource\":{\"resourceType\":\"Observation\",\"id\":\"26537\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-05-31T07:34:43.855+00:00\",\"source\":\"#B1ExqwtmiFQtVOGw\"}," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/observation-type\",\"value\":\"ANC\"}]," +
            "\"partOf\":[{\"reference\":\"MedicationDispense/26539\"}],\"status\":\"preliminary\"," +
            "\"code\":{\"text\":\"ANC\"},\"subject\":{\"reference\":\"Patient/26513\"}," +
            "\"encounter\":{\"reference\":\"Encounter/26533\"}," +
            "\"effectiveDateTime\":\"2024-05-31T07:33:47+00:00\"," +
            "\"performer\":[{\"reference\":\"RelatedPerson/26509\"}]," +
            "\"hasMember\":[{\"reference\":\"Observation/26541\"}]," +
            "\"component\":[{\"code\":{\"coding\":[{\"system\":\"http://fhir" +
            ".org/guides/who/anc-cds/CodeSystem/anc-custom-codes\",\"code\":\"ANC.B6.DE14\"," +
            "\"display\":\"Last menstrual period (LMP) date\"}],\"text\":\"lastMenstrualPeriod\"}," +
            "\"valueDateTime\":\"2024-03-10T00:00:00+00:00\"}," +
            "{\"code\":{\"coding\":[{\"system\":\"http://fhir" +
            ".org/guides/who/anc-cds/CodeSystem/anc-custom-codes\",\"code\":\"ANC.B6.DE22\"," +
            "\"display\":\"Expected date of delivery (EDD)\"}],\"text\":\"estimatedDeliveryDate\"}," +
            "\"valueDateTime\":\"2024-12-15T00:00:00+00:00\"},{\"code\":{\"text\":\"isMalePartnerPresent\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"N\",\"display\":\"NO\"}],\"text\":\"No\"}}," +
            "{\"code\":{\"text\":\"sleepsUnderBedNet\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"N\",\"display\":\"NO\"}],\"text\":\"No\"}}," +
            "{\"code\":{\"text\":\"eatsMoreThanBefore\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"N\",\"display\":\"NO\"}],\"text\":\"No\"}}," +
            "{\"code\":{\"text\":\"takesIronFloatTablets\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"N\",\"display\":\"NO\"}],\"text\":\"No\"}}," +
            "{\"code\":{\"text\":\"priorityPregnancy\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"N\",\"display\":\"NO\"}],\"text\":\"No\"}}," +
            "{\"code\":{\"text\":\"miscarriage\"},\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https" +
            "://fhir.loinc.org/ValueSet/LL3044-6,observation\",\"code\":\"N\",\"display\":\"NO\"}]," +
            "\"text\":\"No\"}},{\"code\":{\"text\":\"placeOfDelivery\"}," +
            "\"valueCodeableConcept\":{\"text\":\"Rosline HF\"}}," +
            "{\"code\":{\"text\":\"eats4GroupIronVitARichFoods\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"N\",\"display\":\"NO\"}],\"text\":\"No\"}}," +
            "{\"code\":{\"text\":\"gestationalAge\"},\"valueCodeableConcept\":{\"text\":\"11 Weeks\"}}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21" +
            ".73/fhir/MedicationDispense/26539\",\"resource\":{\"resourceType\":\"MedicationDispense\"," +
            "\"id\":\"26539\",\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-05-31T07:34:43" +
            ".855+00:00\",\"source\":\"#B1ExqwtmiFQtVOGw\"},\"status\":\"unknown\"," +
            "\"medicationCodeableConcept\":{\"text\":\"takesFancidarTablets\"}," +
            "\"subject\":{\"reference\":\"Patient/26513\"},\"context\":{\"reference\":\"Encounter/26533\"}," +
            "\"performer\":[{\"actor\":{\"reference\":\"RelatedPerson/26509\"}}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21.73/fhir/Observation/26541\"," +
            "\"resource\":{\"resourceType\":\"Observation\",\"id\":\"26541\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-05-31T07:34:43.855+00:00\",\"source\":\"#B1ExqwtmiFQtVOGw\"}," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/observation-type\",\"value\":\"Signs\"}]," +
            "\"status\":\"preliminary\",\"code\":{\"text\":\"Signs\"}," +
            "\"subject\":{\"reference\":\"Patient/26513\"},\"encounter\":{\"reference\":\"Encounter/26533\"}," +
            "\"component\":[{\"code\":{\"text\":\"Convulsions\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"Y\",\"display\":\"YES\"}],\"text\":\"Yes\"}}," +
            "{\"code\":{\"text\":\"Headache\"},\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https" +
            "://fhir.loinc.org/ValueSet/LL3044-6,observation\",\"code\":\"Y\",\"display\":\"YES\"}]," +
            "\"text\":\"Yes\"}}]},\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21" +
            ".73/fhir/Observation/26543\",\"resource\":{\"resourceType\":\"Observation\",\"id\":\"26543\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-05-31T07:34:43.855+00:00\"," +
            "\"source\":\"#B1ExqwtmiFQtVOGw\"},\"status\":\"preliminary\",\"code\":{\"text\":\"Summary\"}," +
            "\"subject\":{\"reference\":\"Patient/26513\"},\"encounter\":{\"reference\":\"Encounter/26533\"}," +
            "\"effectiveDateTime\":\"2024-05-31T07:33:47+00:00\"," +
            "\"performer\":[{\"reference\":\"RelatedPerson/26509\"}]," +
            "\"component\":[{\"code\":{\"text\":\"NearestPHURefferedTo\"}," +
            "\"valueCodeableConcept\":{\"text\":\"Bomali\"}},{\"code\":{\"text\":\"NextVisitDate\"}," +
            "\"valueDateTime\":\"2024-07-27T18:30:00+00:00\"}]},\"search\":{\"mode\":\"match\"}}," +
            "{\"fullUrl\":\"http://192.168.21.73/fhir/ServiceRequest/26546\"," +
            "\"resource\":{\"resourceType\":\"ServiceRequest\",\"id\":\"26546\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-05-31T07:34:43.855+00:00\"," +
            "\"source\":\"#B1ExqwtmiFQtVOGw\"},\"identifier\":[{\"system\":\"http://mdtlabs" +
            ".com/PatientStatus\",\"value\":\"Referred\"},{\"system\":\"http://mdtlabs" +
            ".com/PatientCurrentStatus\",\"value\":\"Referred\"},{\"system\":\"http://mdtlabs" +
            ".com/EncounterType\",\"value\":\"RMNCH\"}],\"requisition\":{\"system\":\"http://mdtlabs" +
            ".com/ticket-type\",\"value\":\"medicalReview\"},\"status\":\"active\",\"intent\":\"order\"," +
            "\"priority\":\"urgent\",\"subject\":{\"reference\":\"Patient/26513\"}," +
            "\"encounter\":{\"reference\":\"Encounter/26533\"}," +
            "\"occurrenceDateTime\":\"2024-07-27T18:30:00+00:00\"," +
            "\"authoredOn\":\"2024-05-31T07:33:47+00:00\"," +
            "\"requester\":{\"reference\":\"Organization/8850\"}," +
            "\"performer\":[{\"reference\":\"Practitioner/9606\"},{\"reference\":\"RelatedPerson/26509\"}," +
            "{\"reference\":\"Organization/8850\"}],\"patientInstruction\":\"ANCSigns\"}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21" +
            ".73/fhir/ServiceRequest/26549\",\"resource\":{\"resourceType\":\"ServiceRequest\"," +
            "\"id\":\"26549\",\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-05-31T07:34:43" +
            ".855+00:00\",\"source\":\"#B1ExqwtmiFQtVOGw\"},\"identifier\":[{\"system\":\"http://mdtlabs" +
            ".com/PatientStatus\",\"value\":\"Referred\"},{\"system\":\"http://mdtlabs" +
            ".com/PatientCurrentStatus\",\"value\":\"Referred\"},{\"system\":\"http://mdtlabs" +
            ".com/EncounterType\",\"value\":\"RMNCHVisitDate\"}],\"requisition\":{\"system\":\"http://mdtlabs" +
            ".com/ticket-type\",\"value\":\"assessment\"},\"status\":\"on-hold\",\"intent\":\"order\"," +
            "\"priority\":\"urgent\",\"subject\":{\"reference\":\"Patient/26513\"}," +
            "\"encounter\":{\"reference\":\"Encounter/26533\"}," +
            "\"occurrenceDateTime\":\"2024-07-27T18:30:00+00:00\"," +
            "\"authoredOn\":\"2024-05-31T07:33:47+00:00\"," +
            "\"requester\":{\"reference\":\"Organization/8850\"}," +
            "\"performer\":[{\"reference\":\"Practitioner/9606\"},{\"reference\":\"RelatedPerson/26509\"}," +
            "{\"reference\":\"Organization/8850\"}],\"patientInstruction\":\"ANC-1\"}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21.73/fhir/Patient/26513\"," +
            "\"resource\":{\"resourceType\":\"Patient\",\"id\":\"26513\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-05-31T07:34:07.863+00:00\",\"source\":\"#W2YFZ0dRJ5HAB0En\"}," +
            "\"extension\":[{\"url\":\"http://mdtlabs" +
            ".com/fhir/StructureDefinition/ExtensionIsPregnantYesorNo\",\"valueBoolean\":true}]," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/patient-id\",\"value\":\"0001000120014\"}," +
            "{\"system\":\"http://mdtlabs.com/village\",\"value\":\"1\"}],\"active\":true," +
            "\"name\":[{\"text\":\"AlaguSelvi\"}],\"telecom\":[{\"system\":\"phone\"," +
            "\"value\":\"80152421\"}],\"gender\":\"female\",\"birthDate\":\"1995-05-30\"," +
            "\"address\":[{\"city\":\"Guindy\"}],\"generalPractitioner\":[{\"reference\":\"Practitioner/9606" +
            "\"}],\"link\":[{\"other\":{\"reference\":\"RelatedPerson/26509\"},\"type\":\"seealso\"}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21.73/fhir/Organization/8850\"," +
            "\"resource\":{\"resourceType\":\"Organization\",\"id\":\"8850\",\"meta\":{\"versionId\":\"4\"," +
            "\"lastUpdated\":\"2024-05-28T09:15:34.625+00:00\",\"source\":\"#yE2Pste5UkCnBrnn\"}," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/organization\"}],\"active\":false," +
            "\"type\":[{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"Clinic\"}]}]," +
            "\"name\":\"General Health Facility\",\"telecom\":[{\"system\":\"phone\"," +
            "\"value\":\"8798798798\"}],\"address\":[{\"use\":\"work\",\"type\":\"both\",\"text\":\"21/3,West" +
            " Park Avenue\",\"district\":\"MADRAS\",\"postalCode\":\"600019\"}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21.73/fhir/Practitioner/9606\"," +
            "\"resource\":{\"resourceType\":\"Practitioner\",\"id\":\"9606\",\"meta\":{\"versionId\":\"17\"," +
            "\"lastUpdated\":\"2024-05-29T06:48:44.141+00:00\",\"source\":\"#HmBjK3plgXy0FyrF\"}," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/user\"}],\"active\":false," +
            "\"name\":[{\"text\":\"Test User\",\"family\":\"User\",\"given\":[\"Test\"]}]," +
            "\"telecom\":[{\"system\":\"phone\",\"value\":\"8263465301\"},{\"system\":\"email\"}]," +
            "\"gender\":\"other\"},\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://192.168.21" +
            ".73/fhir/RelatedPerson/26509\",\"resource\":{\"resourceType\":\"RelatedPerson\"," +
            "\"id\":\"26509\",\"meta\":{\"versionId\":\"3\",\"lastUpdated\":\"2024-05-31T07:34:07" +
            ".863+00:00\",\"source\":\"#W2YFZ0dRJ5HAB0En\"},\"extension\":[{\"url\":\"http://mdtlabs" +
            ".com/fhir/StructureDefinition/ExtensionIsPregnantYesorNo\",\"valueBoolean\":true}]," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/patient-id\",\"value\":\"0001000120014\"}," +
            "{\"system\":\"http://mdtlabs.com/village\",\"value\":\"1\"},{\"system\":\"http://mdtlabs" +
            ".com/household/id\",\"value\":\"26511\"}],\"active\":true," +
            "\"relationship\":[{\"coding\":[{\"system\":\"http://terminology.hl7" +
            ".org/CodeSystem/v3-RoleCode\",\"code\":\"Spouse / Partner\"}],\"text\":\"Spouse / Partner\"}]," +
            "\"name\":[{\"text\":\"AlaguSelvi\"}],\"telecom\":[{\"system\":\"phone\",\"value\":\"80152421\"," +
            "\"use\":\"home\"}],\"gender\":\"female\",\"birthDate\":\"1995-05-30\"," +
            "\"address\":[{\"city\":\"Guindy\"}]},\"search\":{\"mode\":\"match\"}}]}";

    public static final String TEST_BUNDLE_PATIENTS = "{\"resourceType\":\"Bundle\"," +
            "\"id\":\"989f6366-5e93-4ff3-906b-ad057b978d0e\",\"meta\":{\"lastUpdated\":\"2024-04-01T12:34:37" +
            ".419+00:00\"},\"type\":\"searchset\",\"total\":10,\"link\":[{\"relation\":\"self\",\"url\":\"http://13" +
            ".233.192.11:8090/fhir/Patient/$everything?_id=17361\"}],\"entry\":[{\"fullUrl\":\"http://13.233.192" +
            ".11:8090/fhir/Patient/17361\",\"resource\":{\"resourceType\":\"Patient\",\"id\":\"17361\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\"," +
            "\"source\":\"#5lAjBw0teLeivpHt\"},\"identifier\":[{\"system\":\"mdtlabs.com/national-id\"," +
            "\"value\":\"987987\"}],\"active\":true,\"name\":[{\"text\":\"Janaki\"}]," +
            "\"telecom\":[{\"system\":\"phone\",\"value\":\"234567876564\"}],\"gender\":\"female\"," +
            "\"birthDate\":\"1968-04-28\",\"address\":[{\"city\":\"Alandur\"}]," +
            "\"generalPractitioner\":[{\"reference\":\"Practitioner/2\"}]," +
            "\"link\":[{\"other\":{\"reference\":\"RelatedPerson/Son-59459b87-7138-43a2-9065-cfd06836e7e1\"}," +
            "\"type\":\"seealso\"}]},\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192" +
            ".11:8090/fhir/Patient/17361\",\"resource\":{\"resourceType\":\"Patient\",\"id\":\"17362\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\"," +
            "\"source\":\"#5lAjBw0teLeivpHt\"},\"identifier\":[{\"system\":\"mdtlabs.com/national-id\"," +
            "\"value\":\"987987\"}],\"active\":true,\"name\":[{\"text\":\"Janaki\"}]," +
            "\"telecom\":[{\"system\":\"phone\",\"value\":\"234567876564\"}],\"gender\":\"female\"," +
            "\"birthDate\":\"1968-04-28\",\"address\":[{\"city\":\"Alandur\"}]," +
            "\"generalPractitioner\":[{\"reference\":\"Practitioner/2\"}]," +
            "\"link\":[{\"other\":{\"reference\":\"RelatedPerson/Son-59459b87-7138-43a2-9065-cfd06836e7e1\"}," +
            "\"type\":\"seealso\"}]},\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192" +
            ".11:8090/fhir/Provenance/17360\",\"resource\":{\"resourceType\":\"Provenance\",\"id\":\"17360\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\"," +
            "\"source\":\"#5lAjBw0teLeivpHt\"},\"target\":[{\"reference\":\"Patient/17361\"}]," +
            "\"recorded\":\"2024-04-14T10:07:31.000+00:00\"," +
            "\"agent\":[{\"onBehalfOf\":{\"reference\":\"Organization/16047\"}}," +
            "{\"who\":{\"reference\":\"Practitioner/2\"}}]},\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13" +
            ".233.192.11:8090/fhir/Encounter/17363\",\"resource\":{\"resourceType\":\"Encounter\",\"id\":\"17363\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\"," +
            "\"source\":\"#5lAjBw0teLeivpHt\"},\"contained\":[{\"resourceType\":\"Location\",\"id\":\"1\"," +
            "\"position\":{\"longitude\":22.2,\"latitude\":11.1}}],\"identifier\":[{\"system\":\"mdtlabs" +
            ".com/Diarrhoea\",\"value\":\"ANC\"}],\"status\":\"in-progress\"," +
            "\"subject\":{\"reference\":\"Patient/17361\"}," +
            "\"participant\":[{\"individual\":{\"reference\":\"Practitioner/2\"}}]," +
            "\"period\":{\"start\":\"2024-03-14T10:07:31+00:00\",\"end\":\"2024-03-14T10:07:31+00:00\"}," +
            "\"location\":[{\"location\":{\"reference\":\"#1\"}}]," +
            "\"serviceProvider\":{\"reference\":\"Organization/16047\"}},\"search\":{\"mode\":\"match\"}}," +
            "{\"fullUrl\":\"http://13.233.192.11:8090/fhir/Observation/17365\"," +
            "\"resource\":{\"resourceType\":\"Observation\",\"id\":\"17365\",\"meta\":{\"versionId\":\"4\"," +
            "\"lastUpdated\":\"2024-03-27T13:26:35.781+00:00\",\"source\":\"#CHcLuZWZjtTD3msW\"}," +
            "\"identifier\":[{\"system\":\"mdtlabs.com/observation-type\",\"value\":\"ANC\"}]," +
            "\"partOf\":[{\"reference\":\"MedicationDispense/17367\"}],\"status\":\"final\"," +
            "\"code\":{\"coding\":[{\"system\":\"http://fhir.org/guides/who/anc-cds/CodeSystem/anc-custom-codes\"," +
            "\"code\":\"ANC.B6.DE14\",\"display\":\"Last menstrual period (LMP) date\"}]}," +
            "\"subject\":{\"reference\":\"Patient/17361\"},\"encounter\":{\"reference\":\"Encounter/17363\"}," +
            "\"performer\":[{\"reference\":\"RelatedPerson/Son-59459b87-7138-43a2-9065-cfd06836e7e1\"}]," +
            "\"valueDateTime\":\"2024-03-14T10:07:31+00:00\"},\"search\":{\"mode\":\"match\"}}," +
            "{\"fullUrl\":\"http://13.233.192.11:8090/fhir/MedicationDispense/17367\"," +
            "\"resource\":{\"resourceType\":\"MedicationDispense\",\"id\":\"17367\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\",\"source\":\"#5lAjBw0teLeivpHt\"}," +
            "\"status\":\"completed\",\"medicationCodeableConcept\":{\"text\":\"takesFancidarTablets\"}," +
            "\"subject\":{\"reference\":\"Patient/17361\"},\"context\":{\"reference\":\"Encounter/17363\"}," +
            "\"performer\":[{\"actor\":{\"reference\":\"RelatedPerson/Son-59459b87-7138-43a2-9065-cfd06836e7e1\"}}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192.11:8090/fhir/Observation/17371\"," +
            "\"resource\":{\"resourceType\":\"Observation\",\"id\":\"17371\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\",\"source\":\"#5lAjBw0teLeivpHt\"}," +
            "\"status\":\"final\",\"code\":{\"text\":\"Summary\"},\"subject\":{\"reference\":\"Patient/17361\"}," +
            "\"encounter\":{\"reference\":\"Encounter/17363\"}," +
            "\"performer\":[{\"reference\":\"RelatedPerson/Son-59459b87-7138-43a2-9065-cfd06836e7e1\"}]," +
            "\"component\":[{\"code\":{\"text\":\"Notes\"},\"valueString\":\"Serious Issue\"}," +
            "{\"code\":{\"text\":\"isAbleToGoClinic\"}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"https://fhir.loinc.org/ValueSet/LL3044-6," +
            "observation\",\"code\":\"Y\",\"display\":\"YES\"}]}},{\"code\":{\"coding\":[{\"system\":\"http://fhir" +
            ".org/guides/who/anc-cds/CodeSystem/anc-custom-codes\",\"code\":\"ANC.B6.DE22\",\"display\":\"Expected " +
            "date of delivery (EDD)\"}]},\"valueDateTime\":\"2024-12-19T10:07:31+00:00\"}," +
            "{\"code\":{\"coding\":[{\"system\":\"http://fhir.org/guides/who/anc-cds/CodeSystem/anc-custom-codes\"," +
            "\"code\":\"ANC.B5.DE48\",\"display\":\"Danger signs\"}]}," +
            "\"valueCodeableConcept\":{\"coding\":[{\"system\":\"http://fhir" +
            ".org/guides/who/anc-cds/CodeSystem/anc-custom-codes\",\"code\":\"ANC.B5.DE53\"," +
            "\"display\":\"Fever\"}]}},{\"code\":{\"text\":\"NearestPHURefferedTo\"},\"valueString\":\"Bommali\"}," +
            "{\"code\":{\"text\":\"NextVisitDate\"},\"valueDateTime\":\"2024-04-14T10:07:31+00:00\"}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192.11:8090/fhir/ServiceRequest/17373\"," +
            "\"resource\":{\"resourceType\":\"ServiceRequest\",\"id\":\"17373\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-03-27T08:57:20.299+00:00\",\"source\":\"#5lAjBw0teLeivpHt\"}," +
            "\"identifier\":[{\"system\":\"http://example.org\",\"value\":\"referred\"}],\"status\":\"active\"," +
            "\"intent\":\"order\",\"subject\":{\"reference\":\"Patient/17361\"}," +
            "\"encounter\":{\"reference\":\"Encounter/17363\"},\"occurrenceDateTime\":\"2024-04-14T10:07:31+00:00\"," +
            "\"authoredOn\":\"2024-03-14T10:07:31+00:00\",\"requester\":{\"reference\":\"Organization/16047\"}," +
            "\"performer\":[{\"reference\":\"Practitioner/2\"},{\"reference\":\"Organization/16047\"}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192.11:8090/fhir/Organization/16047\"," +
            "\"resource\":{\"resourceType\":\"Organization\",\"id\":\"16047\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2024-02-13T12:13:02.042+00:00\",\"source\":\"#5Hyyl5XRlqZj9yzP\"}," +
            "\"identifier\":[{\"system\":\"http://mdtlabs.com/Organization\"}],\"active\":false," +
            "\"type\":[{\"coding\":[{\"system\":\"http://loinc.org\",\"code\":\"Clinic\"}]}],\"name\":\"Nzeveni " +
            "health centre\",\"telecom\":[{\"system\":\"phone\",\"value\":\"725489844\"}]," +
            "\"address\":[{\"use\":\"work\",\"type\":\"physical\",\"text\":\"At Nzeveni Market, About 15 km from " +
            "Kambu Town.\",\"line\":[\"-2.482\",\"37.891\"],\"city\":\"Kenya, Makueni\",\"district\":\"Makueni\"," +
            "\"state\":\"Kibwezi East\",\"postalCode\":\"12692\",\"country\":\"Kenya\"}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192.11:8090/fhir/Practitioner/2\"," +
            "\"resource\":{\"resourceType\":\"Practitioner\",\"id\":\"2\",\"meta\":{\"versionId\":\"1\"," +
            "\"lastUpdated\":\"2023-06-13T06:31:17.501+00:00\",\"source\":\"#6QZc49HHOn3FHeUk\"}," +
            "\"text\":{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">\\n      " +
            "<p>Dr Adam Careful is a Referring Practitioner for Acme Hospital from 1-Jan 2012 to 31-Mar\\n        " +
            "2012</p>\\n    </div>\"},\"identifier\":[{\"system\":\"http://www.acme.org/practitioners\"," +
            "\"value\":\"23\"}],\"active\":true,\"name\":[{\"family\":\"Careful\",\"given\":[\"Adam\"]," +
            "\"prefix\":[\"Dr\"]}],\"address\":[{\"use\":\"home\",\"line\":[\"534 Erewhon St\"]," +
            "\"city\":\"PleasantVille\",\"state\":\"Vic\",\"postalCode\":\"3999\"}]," +
            "\"qualification\":[{\"identifier\":[{\"system\":\"http://example.org/UniversityIdentifier\"," +
            "\"value\":\"12345\"}],\"code\":{\"coding\":[{\"system\":\"http://terminology.hl7" +
            ".org/CodeSystem/v2-0360/2.7\",\"code\":\"BS\",\"display\":\"Bachelor of Science\"}],\"text\":\"Bachelor " +
            "of Science\"},\"period\":{\"start\":\"1995\"},\"issuer\":{\"display\":\"Example University\"}}]}," +
            "\"search\":{\"mode\":\"match\"}},{\"fullUrl\":\"http://13.233.192" +
            ".11:8090/fhir/RelatedPerson/Son-59459b87-7138-43a2-9065-cfd06836e7e1\"," +
            "\"resource\":{\"resourceType\":\"RelatedPerson\",\"id\":\"Son-59459b87-7138-43a2-9065-cfd06836e7e1\"," +
            "\"meta\":{\"versionId\":\"1\",\"lastUpdated\":\"2024-03-20T20:43:36.180+00:00\"," +
            "\"source\":\"#Gxc9qcbwqE2Z6JP4\"},\"active\":true," +
            "\"patient\":{\"reference\":\"Patient/59459b87-7138-43a2-9065-cfd06836e7e1\"}," +
            "\"relationship\":[{\"coding\":[{\"system\":\"http://terminology.hl7.org/CodeSystem/v3-RoleCode\"," +
            "\"code\":\"Son\"}]}]},\"search\":{\"mode\":\"match\"}}]}";
}
