package com.mdtlabs.coreplatform.cqlservice.cql.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.util.BundleUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.cqframework.cql.elm.execution.VersionedIdentifier;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.opencds.cqf.cql.engine.execution.EvaluationResult;
import org.opencds.cqf.cql.engine.execution.LibraryLoader;
import org.opencds.cqf.cql.engine.fhir.model.R4FhirModelResolver;
import org.opencds.cqf.cql.engine.model.ModelResolver;
import org.opencds.cqf.cql.engine.terminology.TerminologyProvider;
import org.opencds.cqf.cql.evaluator.CqlEvaluator;
import org.opencds.cqf.cql.evaluator.builder.CqlEvaluatorBuilder;
import org.opencds.cqf.cql.evaluator.builder.RetrieveProviderConfig;
import org.opencds.cqf.cql.evaluator.builder.data.RetrieveProviderConfigurer;
import org.opencds.cqf.cql.evaluator.engine.model.CachingModelResolverDecorator;
import org.opencds.cqf.cql.evaluator.engine.retrieve.BundleRetrieveProvider;
import org.opencds.cqf.cql.evaluator.engine.terminology.BundleTerminologyProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.BadRequestException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.cqlservice.constants.CqlConstants;
import com.mdtlabs.coreplatform.cqlservice.cql.repository.CqlResultRepository;
import com.mdtlabs.coreplatform.cqlservice.cql.service.CqlService;
import com.mdtlabs.coreplatform.cqlservice.model.dto.AncResultDTO;
import com.mdtlabs.coreplatform.cqlservice.model.dto.CqlRequestDTO;
import com.mdtlabs.coreplatform.cqlservice.model.entity.CqlResult;
import com.mdtlabs.coreplatform.cqlservice.util.CqlUtils;

/**
 * <p>
 * This class is a service class to perform Cql evaluation operations.
 * </p>
 *
 * @author Vishwaeaswaran M created on May 20, 2024
 */
@Service
public class CqlServiceImpl implements CqlService {

    private final CqlResultRepository cqlResultRepository;

    private LibraryLoader libraryLoader;

    private TerminologyProvider terminologyProvider;

    private ModelResolver modelResolver;

    @Value("${app.fhir-server-url}")
    private String fhirServerUrl;

    @Value("${app.cql-path}")
    private String cqlPath;

    @Value("${app.valueset-path}")
    private String valuesetPath;

    CqlServiceImpl(CqlResultRepository cqlResultRepository) {
        this.cqlResultRepository = cqlResultRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> evaluatePatientByLibrary(CqlRequestDTO requestDTO) {
        IParser iParser = CqlUtils.getFhirContext().newJsonParser();
        Bundle resourceBundle = iParser.parseResource(Bundle.class, requestDTO.getResourceBundle());
        Bundle formatedBundle = getFormatedResourceBundle(resourceBundle);
        return evaluate(formatedBundle, requestDTO.getLibraries(), null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> evaluatePatient(CqlRequestDTO requestDTO) {
        IParser iParser = CqlUtils.getFhirContext().newJsonParser();
        Bundle resourceBundle = iParser.parseResource(Bundle.class, requestDTO.getResourceBundle());
        Bundle formatedBundle = getFormatedResourceBundle(resourceBundle);
        return evaluate(formatedBundle, Set.of(CqlConstants.ANC_DT_01, CqlConstants.ANC_S_01), null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> evaluatePatientByLibraryAndExpressions(CqlRequestDTO requestDTO) {
        IParser iParser = CqlUtils.getFhirContext().newJsonParser();
        Bundle resourceBundle = iParser.parseResource(Bundle.class, requestDTO.getResourceBundle());
        Bundle formatedBundle = getFormatedResourceBundle(resourceBundle);
        return evaluate(formatedBundle, requestDTO.getLibraries(), requestDTO.getExpressions());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getResultByPatientId(String patientId) {
        Logger.logInfo(StringUtil.concatString("Retrieving result for the Patient id: ", patientId));
        CqlResult cqlResult = cqlResultRepository.findByPatientIdAndIsLatestTrue(patientId);
        if (Objects.isNull(cqlResult)) {
            throw new DataNotFoundException(1011);
        }
        return cqlResult.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getAncResultByPatientId(String patientId) {
        Logger.logInfo(StringUtil.concatString("Retrieving anc result for the Patient id: ", patientId));
        CqlResult cqlResult = cqlResultRepository.findByPatientIdAndIsLatestTrue(patientId);
        if (Objects.isNull(cqlResult)) {
            return Collections.emptyMap();
        }
        return mapAncContactResults(cqlResult.getResults());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<AncResultDTO> getAncResultByVillageIdsAndCreatedAtDate(CqlRequestDTO cqlRequestDTO) {
        Logger.logInfo("Retrieving anc result for the Villages");
        List<CqlResult> cqlResults = Objects.isNull(cqlRequestDTO.getLastSyncTime())
                ? cqlResultRepository.findByVillageIdInAndUpdatedAtLessThanEqualAndIsLatestTrue(cqlRequestDTO.getVillageIds(),
                cqlRequestDTO.getCurrentSyncTime()) : cqlResultRepository.findByVillageIdInAndUpdatedAtGreaterThanAndUpdatedAtLessThanEqualAndIsLatestTrue(
                cqlRequestDTO.getVillageIds(), cqlRequestDTO.getLastSyncTime(), cqlRequestDTO.getCurrentSyncTime());
        return mapListAncContactResults(cqlResults);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> evaluateByEncounter(String encounterId) {
        Logger.logInfo(StringUtil.concatString("Evaluating anc result for the Encounter id: ", encounterId));
        IGenericClient client =
                CqlUtils.getFhirClient(fhirServerUrl, CommonUtil.getClient(), CommonUtil.getAuthToken());
        String searchUrl = StringUtil.concatString(fhirServerUrl, CqlConstants.ENCOUNTER_SLASH, encounterId,
                CqlConstants.EVERYTHING_URL);
        Bundle bundle = client.search().byUrl(searchUrl).returnBundle(Bundle.class).execute();
        CqlRequestDTO cqlRequestDTO = new CqlRequestDTO();
        cqlRequestDTO.setResourceBundle(CqlUtils.getFhirContext().newJsonParser().encodeResourceToString(bundle));
        cqlRequestDTO.setLibraries(Set.of(CqlConstants.ANC_DT_01, CqlConstants.ANC_S_01));
        Map<String, Object> results = evaluatePatientByLibrary(cqlRequestDTO);
        return mapAncContactResults(results);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearCqlAncTerminology() {
        Logger.logInfo("Clearing the CQL builder");
        libraryLoader = null;
        terminologyProvider = null;
        modelResolver = null;
    }

    /**
     * <p>
     * This method evaluates the given resource with libraries and expressions.
     * </p>
     *
     * @param resourceBundle the resource bundle which is being evaluated.
     * @param libraryIds     the list of libraries.
     * @param expressions    the list of expressions.
     * @return the evaluated result for the given resource.
     */
    private Map<String, Object> evaluate(Bundle resourceBundle, Set<String> libraryIds, Set<String> expressions) {
        FhirContext fhirContext = CqlUtils.getFhirContext();

        // Parameters for evaluation
        Map<String, Object> parameters = getParameters(resourceBundle);

        // Evaluate the expression
        CqlEvaluator cqlEvaluator = buildCqlEvaluator(fhirContext, resourceBundle);
        Logger.logInfo("Evaluate the expressions");
        IParser parser = fhirContext.newJsonParser();
        Map<String, Object> evaluatedResults = new LinkedHashMap<>();
        Map<String, Object> parsedResults = new LinkedHashMap<>();
        EvaluationResult evalResult;
        if (Objects.nonNull(expressions) && !expressions.isEmpty()) {
            for (String libraryId : libraryIds) {
                evalResult =
                        cqlEvaluator.evaluate(new VersionedIdentifier().withId(libraryId), expressions, parameters);
                evaluatedResults.putAll(evalResult.expressionResults);
            }
        } else {
            for (String libraryId : libraryIds) {
                evalResult = cqlEvaluator.evaluate(new VersionedIdentifier().withId(libraryId), parameters);
                evaluatedResults.putAll(evalResult.expressionResults);
            }
        }
        Logger.logInfo("Parsing the results");
        for (Map.Entry<String, Object> entry : evaluatedResults.entrySet()) {
            Object value = entry.getValue();
            if (value instanceof Resource resource) {
                parsedResults.put(entry.getKey(), parser.encodeResourceToString(resource));
            } else if (value instanceof Boolean aBoolean) {
                parsedResults.put(entry.getKey(), aBoolean);
            } else if (value instanceof DateTimeType dateTime) {
                parsedResults.put(entry.getKey(), dateTime.getValueAsString());
            } else if (value instanceof ArrayList<?> list) {
                for (Object o : list) {
                    if (o instanceof CodeableConcept codeableConcept) {
                        parsedResults.put(entry.getKey(), codeableConcept.getText());
                    }
                }
            } else if (Objects.nonNull(value)) {
                parsedResults.put(entry.getKey(), value.toString());
            }
        }
        saveCqlResult(parameters, parsedResults);
        return parsedResults;
    }

    /**
     * <p>
     * This method is used to save the evaluated results for the resource.
     * </p>
     *
     * @param parameters the patient's paramters.
     * @param ancResults the results for the resource.
     */
    private void saveCqlResult(Map<String, Object> parameters, Map<String, Object> ancResults) {
        // Save the results
        Map<String, Object> results = new LinkedHashMap<>(ancResults);
        results.keySet().retainAll(CqlConstants.ANC_CONTACT_DATA);
        CqlResult existingCqlResult =
                cqlResultRepository.findByResourceIdAndIsLatestTrue(
                        parameters.get(CqlConstants.PATIENT_FHIR_ID).toString());
        CqlResult cqlResult = new CqlResult();
        cqlResult.setResourceId(parameters.get(CqlConstants.PATIENT_FHIR_ID).toString());
        cqlResult.setResults(new ObjectMapper().convertValue(results, Map.class));
        cqlResult.setMemberId(parameters.getOrDefault(CqlConstants.MEMBER_IDENTIFIER, Constants.EMPTY).toString());
        cqlResult.setVillageId(parameters.getOrDefault(CqlConstants.VILLAGE_IDENTIFIER, Constants.EMPTY).toString());
        cqlResult.setPatientId(parameters.getOrDefault(CqlConstants.PATIENT_IDENTIFIER, Constants.EMPTY).toString());
        cqlResult.setIsLatest(Boolean.TRUE);
        Logger.logInfo("Saving the ANC data to db...");
        if (Objects.nonNull(existingCqlResult)) {
            existingCqlResult.setIsLatest(Boolean.FALSE);
            cqlResultRepository.saveAll(List.of(existingCqlResult, cqlResult));
        } else {
            cqlResultRepository.save(cqlResult);
        }
    }

    /**
     * Extracts parameters from a FHIR Bundle.
     * <p>
     * This method iterates through the entries in a FHIR Bundle to extract and construct a map of parameters
     * relevant for CQL evaluation. It specifically looks for Patient and Encounter resources within the bundle.
     * For Patient resources, it extracts the FHIR ID, patient identifier, village identifier, and member identifier
     * (if the patient is linked to a related person). For Encounter resources, it extracts the encounter ID.
     * </p>
     *
     * @param bundle The FHIR Bundle containing resources to extract parameters from.
     * @return A map of extracted parameters, including patient FHIR ID, patient identifier, village identifier,
     * member identifier, and encounter ID.
     */
    private Map<String, Object> getParameters(Bundle bundle) {
        Map<String, Object> parameters = new HashMap<>();
        bundle.getEntry().forEach(entry -> {
            if (String.valueOf(ResourceType.Patient).equals(entry.getResource().getResourceType().toString())) {
                Patient patient = (Patient) entry.getResource();
                parameters.put(CqlConstants.PATIENT_FHIR_ID, CqlUtils.getIdFromHistoryUrl(patient.getId()));
                patient.getIdentifier().forEach(identifier -> {
                    if (Objects.nonNull(identifier.getSystem()) && Objects.nonNull(identifier.getValue())) {
                        if (identifier.getSystem().endsWith(CqlConstants.PATIENT_IDENTIFIER)) {
                            parameters.put(CqlConstants.PATIENT_IDENTIFIER, identifier.getValue());
                        } else if (identifier.getSystem().endsWith(CqlConstants.VILLAGE_IDENTIFIER)) {
                            parameters.put(CqlConstants.VILLAGE_IDENTIFIER, identifier.getValue());
                        }
                    }
                });
                patient.getLink().forEach(patientLinkComponent -> {
                    if (patientLinkComponent.getOther() instanceof Reference reference) {
                        String valueReference = reference.getReference();
                        if (valueReference.startsWith(String.valueOf(ResourceType.RelatedPerson))) {
                            parameters.put(CqlConstants.MEMBER_IDENTIFIER, CqlUtils.getIdFromReference(valueReference));
                        }
                    }
                });
            } else if (String.valueOf(ResourceType.Encounter)
                    .equals(entry.getResource().getResourceType().toString())) {
                Encounter encounter = (Encounter) entry.getResource();
                parameters.put(CqlConstants.ENCOUNTER, CqlUtils.getIdFromHistoryUrl(encounter.getId()));
            }
        });
        return parameters;
    }

    /**
     * <p>
     * This method builds the CqlEvaluator with given fhir context and resource bundle.
     * </p>
     *
     * @param fhirContext    the fhir context to build the CqlEvaluator.
     * @param resourceBundle the resource bundle to build the CqlEvaluator.
     * @return the CqlEvaluator.
     */
    private CqlEvaluator buildCqlEvaluator(FhirContext fhirContext, Bundle resourceBundle) {
        //Build the CqlEvaluator
        BundleRetrieveProvider bundleRetrieveProvider = new BundleRetrieveProvider(fhirContext, resourceBundle);
        CqlEvaluatorBuilder builder =
                new CqlEvaluatorBuilder(new RetrieveProviderConfigurer(RetrieveProviderConfig.defaultConfig()));
        builder.withLibraryLoader(getLibraryLoader());
        builder.withTerminologyProvider(getTerminologyProvider(fhirContext));
        builder.withModelResolverAndRetrieveProvider(CqlConstants.HL7_URL, getModelResolver(), bundleRetrieveProvider);
        return builder.build();
    }

    /**
     * <p>
     * This method returns the built library loader.
     * </p>
     *
     * @return the built library loader.
     */
    private LibraryLoader getLibraryLoader() {
        // Set up LibraryLoader that translates from CQL
        if (Objects.isNull(libraryLoader)) {
            Logger.logInfo("Start to build LibraryLoader");
            libraryLoader = CqlUtils.buildLibraryLoader(CqlUtils.getCqlFiles(cqlPath));
        }
        Logger.logInfo("Return built LibraryLoader");
        return libraryLoader;
    }

    /**
     * <p>
     * This method returns the {@link TerminologyProvider} for the given fhir context.
     * </p>
     *
     * @param fhirContext the fhir context to build the terminology provider.
     * @return the {@link TerminologyProvider}.
     */
    private TerminologyProvider getTerminologyProvider(FhirContext fhirContext) {
        // Set up a terminology provider that reads from a bundle
        if (Objects.isNull(terminologyProvider)) {
            Logger.logInfo("Start to build TerminologyProvider");
            terminologyProvider =
                    new BundleTerminologyProvider(fhirContext, CqlUtils.getTerminologyResourceBundle(valuesetPath));
        }
        Logger.logInfo("Return built TerminologyProvider");
        return terminologyProvider;
    }

    /**
     * <p>
     * This method returns the {@link ModelResolver} for fhir.
     * </p>
     *
     * @return the built {@link ModelResolver}.
     */
    private ModelResolver getModelResolver() {
        /* Set up model resolution (this is this bit that translates between CQL types
         and the HAPI FHIR java structures) */
        if (Objects.isNull(modelResolver)) {
            modelResolver = new CachingModelResolverDecorator(new R4FhirModelResolver());
        }
        return modelResolver;
    }

    /**
     * <p>
     * This method is used to validate and format the resource bundle.
     * </p>
     *
     * @param resourceBundle the resource bundle containing the resource and components.
     * @return the formatted resource bundle.
     */
    private Bundle getFormatedResourceBundle(Bundle resourceBundle) {
        validateBundle(resourceBundle);
        Bundle bundle = new Bundle();
        for (Bundle.BundleEntryComponent resource : resourceBundle.getEntry()) {
            if (resource.getResource() instanceof Patient patient) {
                bundle.addEntry().setResource(patient);
            } else if (resource.getResource() instanceof Encounter encounter) {
                bundle.addEntry().setResource(encounter);
            } else if (resource.getResource() instanceof Observation observation) {
                createObservation(bundle, observation);
            }
        }
        return bundle;
    }

    /**
     * <p>
     * This method is used to validate the resource bundle.
     * </p>
     *
     * @param resourceBundle the resource bundle to validate.
     */
    private void validateBundle(Bundle resourceBundle) {
        if (resourceBundle.getEntry().isEmpty()) {
            throw new BadRequestException(1002);
        }
        List<Patient> patients = BundleUtil.toListOfResourcesOfType(CqlUtils.getFhirContext(), resourceBundle,
                Patient.class);
        if (patients.size() != 1) {
            throw new BadRequestException(1003);
        }
        Patient patient = patients.getFirst();
        if (Objects.isNull(patient)) {
            throw new BadRequestException(1004);
        }
    }

    /**
     * <p>
     * This method is used to extract the code-able concepts from the component
     * and add it to a separate Observations for the patient.
     * </p>
     *
     * @param bundle      the bundle to add the observations.
     * @param observation the observation to extract the components.
     */
    private void createObservation(Bundle bundle, Observation observation) {
        observation.setStatus(Observation.ObservationStatus.FINAL);
        if (CqlConstants.PATIENT_VITALS.equals(observation.getIdentifierFirstRep().getValue())) {
            return;
        }
        if (CqlConstants.SIGNS.equals(observation.getCode().getText()) && observation.hasComponent()) {
            for (Observation.ObservationComponentComponent component : observation.getComponent()) {
                Observation newObservation = observation.copy();
                newObservation.setComponent(null);
                newObservation.setValue(component.getCode());
                bundle.addEntry().setResource(newObservation);
            }
        } else if (observation.hasComponent()) {
            for (Observation.ObservationComponentComponent component : observation.getComponent()) {
                Observation newObservation = observation.copy();
                newObservation.setComponent(null);
                newObservation.setCode(component.getCode());
                newObservation.setValue(component.getValue());
                bundle.addEntry().setResource(newObservation);
            }
        } else {
            bundle.addEntry().setResource(observation);
        }
    }

    /**
     * Maps ANC (Antenatal Care) contact results to a structured format.
     * <p>
     * This method takes a map of raw ANC contact results and transforms it into a structured map
     * where each key represents a specific ANC contact detail (e.g., estimated date of conception,
     * gestational age, estimated delivery date, dates of ANC visits) and the corresponding value from
     * the input results. This structured format facilitates easier access and manipulation of ANC contact
     * details in subsequent processes.
     * </p>
     *
     * @param results The raw results map containing ANC contact details, where each key is a specific detail
     *                identifier and the value is the detail's value.
     * @return A structured map of ANC contact details, with predefined keys for easier access.
     */
    private Map<String, Object> mapAncContactResults(Map<String, Object> results) {
        Map<String, Object> smartAncContactDetails = new LinkedHashMap<>();
        if (Objects.nonNull(results) && !results.isEmpty()) {
            smartAncContactDetails.put(CqlConstants.ANC_ESTIMATED_DATE_OF_CONCEPTION,
                    results.get(CqlConstants.ESTIMATED_DATE_OF_CONCEPTION));
            smartAncContactDetails.put(CqlConstants.ANC_GESTATIONAL_AGE_GA,
                    results.get(CqlConstants.GESTATIONAL_AGE));
            smartAncContactDetails.put(CqlConstants.ANC_ESTIMATED_DELIVERY_DATE,
                    results.get(CqlConstants.ESTIMATED_DUE_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_FIRST_VISIT_DATE,
                    results.get(CqlConstants.ANC_FIRST_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_SECOND_VISIT_DATE,
                    results.get(CqlConstants.ANC_SECOND_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_THIRD_VISIT_DATE,
                    results.get(CqlConstants.ANC_THIRD_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_FOURTH_VISIT_DATE,
                    results.get(CqlConstants.ANC_FOURTH_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_FIFTH_VISIT_DATE,
                    results.get(CqlConstants.ANC_FIFTH_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_SIXTH_VISIT_DATE,
                    results.get(CqlConstants.ANC_SIXTH_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_SEVENTH_VISIT_DATE,
                    results.get(CqlConstants.ANC_SEVENTH_CONTACT_DATE));
            smartAncContactDetails.put(CqlConstants.ANC_EIGHTH_VISIT_DATE,
                    results.get(CqlConstants.ANC_EIGHTH_CONTACT_DATE));
        }
        return smartAncContactDetails;
    }

    /**
     * <p>
     * This method maps the results of CQL evaluation to a list of AncResultDTO objects,
     * each containing the patient ID, member ID, and the ANC contact details in JSON format.
     * </p>
     *
     * @param cqlResults The list of CqlResult objects containing the evaluation results.
     * @return A list of AncResultDTO objects.
     * @throws ServicesException If there is an error while converting the ANC contact details to JSON.
     */
    private List<AncResultDTO> mapListAncContactResults(List<CqlResult> cqlResults) {
        List<AncResultDTO> ancResultDTOS = new ArrayList<>();
        if (Objects.nonNull(cqlResults)) {
            for (CqlResult result : cqlResults) {
                AncResultDTO ancResultDTO = new AncResultDTO();
                try {
                    ancResultDTO.setSmartAncContactDetails(
                            Objects.nonNull(result.getResults()) ? new ObjectMapper().writeValueAsString(
                                    mapAncContactResults(result.getResults())) : null);
                } catch (JsonProcessingException exception) {
                    throw new ServicesException(1005);
                }
                ancResultDTO.setPatientId(result.getPatientId());
                ancResultDTO.setMemberId(result.getMemberId());
                ancResultDTOS.add(ancResultDTO);
            }
        }
        return ancResultDTOS;
    }
}
