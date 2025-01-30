package com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hl7.fhir.r4.model.Appointment;
import org.hl7.fhir.r4.model.Appointment.AppointmentParticipantComponent;
import org.hl7.fhir.r4.model.Appointment.AppointmentStatus;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Bundle.HTTPVerb;
import org.hl7.fhir.r4.model.CarePlan;
import org.hl7.fhir.r4.model.CarePlan.CarePlanActivityComponent;
import org.hl7.fhir.r4.model.CarePlan.CarePlanActivityDetailComponent;
import org.hl7.fhir.r4.model.CarePlan.CarePlanActivityKind;
import org.hl7.fhir.r4.model.CarePlan.CarePlanIntent;
import org.hl7.fhir.r4.model.CarePlan.CarePlanStatus;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.Timing;
import org.hl7.fhir.r4.model.Timing.UnitsOfTime;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.util.DateUtil;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;
import com.mdtlabs.coreplatform.fhirmapper.common.Constants;
import com.mdtlabs.coreplatform.fhirmapper.common.FhirIdentifierConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.constants.FhirConstants;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.FrequencyDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.PregnancyDetailsDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.ProvenanceDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.TreatmentPlanResponseDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.dto.pregnancy.PregnancySymptomDTO;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.FhirUtils;
import com.mdtlabs.coreplatform.fhirmapper.common.utils.RestApiUtil;
import com.mdtlabs.coreplatform.fhirmapper.converter.SpiceConverter;
import com.mdtlabs.coreplatform.fhirmapper.patienttreatmentplan.service.PatientTreatmentPlanService;

/**
 * <p>
 * This class is a service implementation class to perform operation on Patient treatmentplan.
 * </p>
 *
 * @author Karthick M created on Aug 14, 2024
 */
@Service
public class PatientTreatmentPlanServiceImpl implements PatientTreatmentPlanService {

    private final FhirUtils fhirUtils;

    private final RestApiUtil restApiUtil;

    private final SpiceConverter spiceConverter;

    @Autowired
    public PatientTreatmentPlanServiceImpl(FhirUtils fhirUtils, RestApiUtil restApiUtil, SpiceConverter spiceConverter) {
        this.fhirUtils = fhirUtils;
        this.restApiUtil = restApiUtil;
        this.spiceConverter = spiceConverter;
    }

    private final String GET_CAREPLAN_PLAN_MEMBER = "CarePlan?contributor=RelatedPerson/%s&_sort=-_lastUpdated";
    private final String GET_CAREPLAN_BY_ID = "CarePlan?_id=%s";
    private final String GET_APPOINTMENT_BY_IDS = "Appointment?_id=%s";
    private final String GET_APPOINTMENT_BY_RELATED_PERSON_ID =
            "Appointment?identifier=" + FhirIdentifierConstants.APPOINTMENT_TYPE_SYSTEM_URL + Constants.VERTICAL_BAR + Constants.STRING_FORMAT_SPECIFIER + "&related-person=%s";
    private final String GET_PREGNANCY_DETAILS = "Observation?performer=RelatedPerson/%s&code:text=%s&_sort=-_lastUpdated&_count=1";

    /**
     * {@inheritDoc}
     */
    public TreatmentPlanResponseDTO createProvisionalPlan(TreatmentPlanDTO treatmentplan, Bundle transactioBundle,
                                                          CarePlan existingCarePlan, List<String> details) {
        if (Objects.isNull(transactioBundle)) { 
            transactioBundle = new Bundle().setType(BundleType.TRANSACTION);
        }
        List<FrequencyDTO> frequenciesByRisk = getFrequencies(treatmentplan);
        CarePlan carePlan;
        if (Objects.isNull(existingCarePlan)) {
            carePlan = createCarePlan(frequenciesByRisk, transactioBundle, treatmentplan, true);
        } else {
            carePlan = updateCarePlan(frequenciesByRisk, transactioBundle, treatmentplan,details, existingCarePlan);
        }
        TreatmentPlanResponseDTO response = constructTreatmentPlanRespose(carePlan);
        if (carePlan.hasActivity()) {
            restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transactioBundle));
        }
        return response;
    }

    /**
     * Creats and maps the careplan resource with details from frequencies.
     * 
     * @param frequencies
     * @param transactionBundle
     * @param treatmentplan
     * @return CarePlan resource
     */
    private CarePlan createCarePlan(List<FrequencyDTO> frequencies, Bundle transactionBundle, TreatmentPlanDTO treatmentplan, boolean isProvisonal) {
        String uuid =  fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.CarePlan), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);

        CarePlan carePlan = new CarePlan();
        carePlan.addIdentifier().setSystem(FhirIdentifierConstants.CARE_PLAN_SYSTEM_URL).setValue(Constants.PATIENT_TREATMENT_PLAN);
        carePlan.setStatus(isProvisonal ? CarePlanStatus.DRAFT : CarePlanStatus.COMPLETED);
        carePlan.setIntent(isProvisonal ? CarePlanIntent.PROPOSAL: CarePlanIntent.PLAN);
        carePlan.setTitle(Constants.PATIENT_TREATMENT_PLAN_TITLE);
        carePlan.setCreated(new Date());
        if (!Objects.isNull(treatmentplan.getPatientReference())) {
            carePlan.setSubject(new Reference(StringUtil.concatString(String.valueOf(ResourceType.Patient), Constants.FORWARD_SLASH, treatmentplan.getPatientReference())));
        }
        carePlan.setContributor(List.of(new Reference(StringUtil.concatString(String.valueOf(ResourceType.RelatedPerson), Constants.FORWARD_SLASH, treatmentplan.getMemberReference()))));
        for (FrequencyDTO frequency : frequencies) {
            carePlan.addActivity(createCarePlanActivity(frequency, treatmentplan, transactionBundle, isProvisonal));
        }
        fhirUtils.setBundle(url, fullUrl, HTTPVerb.POST, carePlan, transactionBundle, treatmentplan.getProvenance());
        return carePlan;
    }

    /**
     * Creates careplan activity component object.
     * 
     * @param frequency
     * @param treatmentplan
     * @param transactionBundle
     * @return CarePlanActivityComponent 
     */
    private CarePlanActivityComponent createCarePlanActivity(FrequencyDTO frequency, TreatmentPlanDTO treatmentplan, Bundle transactionBundle, boolean isProvisonal) {
        CarePlanActivityComponent activityComponent = new CarePlanActivityComponent().setDetail(mapActivityDetailComponent(new CarePlanActivityDetailComponent(), frequency));

        if (isProvisonal) {
            if (Objects.nonNull(frequency) && Objects.nonNull(frequency.getType()) &&
                    Constants.FREQUENCY_MEDICAL_REVIEW.equals(frequency.getType())) {
                Date date = DateUtil.getTreatmentPlanFollowupDate(frequency.getPeriod(), frequency.getDuration(), null);
                treatmentplan.setNextMedicalReviewDate(createOrUpdateAppointment(frequency.getType(), date, treatmentplan.getMemberReference(),
                        treatmentplan.getPatientReference(),
                    transactionBundle, treatmentplan.getProvenance(), Boolean.TRUE));
            } else if (Objects.nonNull(frequency) && !Objects.isNull(frequency.getPeriod()) && !Objects.isNull(frequency.getDuration())){
                Date date = DateUtil.getTreatmentPlanFollowupDate(frequency.getPeriod(), frequency.getDuration(), null);
                if (Constants.FREQUENCY_BP_CHECK.equals(frequency.getType())) {
                    treatmentplan.setNextBpAssessmentDate(date);
                }
                if (Constants.FREQUENCY_BG_CHECK.equals(frequency.getType())) {
                    treatmentplan.setNextBgAssessmentDate(date);
                }
            }
        }
        return activityComponent;
    }

    /**
     * Maps the careplan activity component from frequency. 
     * 
     * @param activityDetailComponent
     * @param frequency
     * @return CarePlanActivityDetailComponent
     */
    private CarePlanActivityDetailComponent mapActivityDetailComponent(CarePlanActivityDetailComponent activityDetailComponent ,FrequencyDTO frequency) {
        if (!Objects.isNull(frequency)) {     
            activityDetailComponent.setKind(CarePlanActivityKind.APPOINTMENT);
            if (!Objects.isNull(frequency.getPeriod()) || !Objects.isNull(frequency.getDuration())) {
                activityDetailComponent.setScheduled(createTiming(frequency.getDuration(), frequency.getPeriod()));
            }
            CodeableConcept codeableConcept = new CodeableConcept();
            codeableConcept.setText(frequency.getType());
            activityDetailComponent.setCode(codeableConcept);
            activityDetailComponent.setDescription(frequency.getName());
        }
        return activityDetailComponent;
    }

    /**
     * Creates Timing with frequecy and duration unit.
     * 
     * @param duration
     * @param period
     * @return Timing object.
     */
    private Timing createTiming(Integer duration, String period) {
        Timing timing = new Timing();
        timing.getRepeat().setFrequency(duration);
        timing.getRepeat().setDurationUnit(UnitsOfTime.fromCode(period));
        return timing;
    }

    /**
     * Create appointment for patient.
     * 
     * @param assessmentType
     * @param nextVisitDate
     * @param memberRef
     * @param patientRef
     * @param transactionBundle
     * @param provenance
     * @return appointment url
     */
    private String createAppointment(String assessmentType, Date nextVisitDate, String memberRef, String patientRef,
                               Bundle transactionBundle,
            ProvenanceDTO provenance) {

        String uuid =  fhirUtils.getUniqueId();
        String fullUrl = StringUtil.concatString(Constants.FHIR_BASE_URL, uuid);
        String url = StringUtil.concatString(String.valueOf(ResourceType.Appointment), Constants.FORWARD_SLASH, Constants.FHIR_BASE_URL, uuid);
        Appointment appointment = new Appointment();
        appointment.addIdentifier().setSystem(FhirIdentifierConstants.APPOINTMENT_TYPE_SYSTEM_URL).setValue(assessmentType);
        appointment.setStatus(AppointmentStatus.PROPOSED);
        appointment.setCreated(new Date());
        AppointmentParticipantComponent participantComponent = new AppointmentParticipantComponent();
        participantComponent.setActor(fhirUtils.getReferenceUrl(ResourceType.RelatedPerson, memberRef));
        appointment.addParticipant(participantComponent);
        if (Objects.nonNull(patientRef)) {
            participantComponent = new AppointmentParticipantComponent();
            participantComponent.setActor(fhirUtils.getReferenceUrl(ResourceType.Patient, patientRef));
            appointment.addParticipant(participantComponent);
        }
        appointment.setStart(nextVisitDate);
        fhirUtils.setBundle(url, fullUrl, HTTPVerb.POST, appointment, transactionBundle, provenance);
        return fullUrl;

    }

    public void updateAppointment(Appointment appointment, Bundle transactionBundle, ProvenanceDTO provenance,
                                   Date toUpdateDate, String patientRef) {
        appointment.setStart(toUpdateDate);
        if (Objects.nonNull(patientRef)
                && !appointment.getParticipant().isEmpty()
                && appointment.getParticipant().size() <= Constants.ONE) {
            AppointmentParticipantComponent participantComponent = new AppointmentParticipantComponent();
            participantComponent.setActor(fhirUtils.getReferenceUrl(ResourceType.Patient, patientRef));
            appointment.addParticipant(participantComponent);
        }
        fhirUtils.setBundle(StringUtil.concatString(ResourceType.Appointment.toString(), Constants.FORWARD_SLASH, appointment.getIdPart()),
                        Constants.EMPTY_SPACE,
                        HTTPVerb.PUT, appointment, transactionBundle, provenance);

    }

    /**
     * Gets frequency for patient based on cvd risk level.
     * 
     * @param treatmentPlan
     * @return List<FrequencyDTO> list of frequency
     */
    private List<FrequencyDTO> getFrequencies(TreatmentPlanDTO treatmentPlan) {        
        List<FrequencyDTO> frequencyByRiskLevel = new ArrayList<>();
        if (Objects.nonNull(treatmentPlan.getCvdRiskLevel())) {
            treatmentPlan.getFrequencies().forEach(frequency -> {
                if (treatmentPlan.getCvdRiskLevel().equals(frequency.getRiskLevel())) {
                    frequencyByRiskLevel.add(frequency);
                }
            });
        }
        if (treatmentPlan.isHba1c()) {
            FrequencyDTO hba1cFrequency = treatmentPlan.getFrequencies().stream().filter(frequency ->
                    Constants.HBA1C_DEFAULT_FREQUENCY.equals(frequency.getName()) && Constants.FREQUENCY_HBA1C_CHECK.equals(frequency.getType())).findAny().orElse(null);
            frequencyByRiskLevel.add(hba1cFrequency);
        } else if (!treatmentPlan.isPregnancyAnc() && Objects.nonNull(treatmentPlan.getCvdRiskLevel())){
            FrequencyDTO frequency = new FrequencyDTO();
            frequency.setType(Constants.FREQUENCY_HBA1C_CHECK);
            frequencyByRiskLevel.add(frequency);
        }
        if (treatmentPlan.isBGDefaultFrequency()) {
            frequencyByRiskLevel.removeIf(frequency -> Constants.FREQUENCY_BG_CHECK.equals(frequency.getType()));
            FrequencyDTO frequency = new FrequencyDTO();
            frequency.setType(Constants.FREQUENCY_BG_CHECK);
            frequency.setDuration(Constants.BG_DEFAULT_FRQUENCY_DURATION);
            frequency.setPeriod(Constants.BG_DEFAULT_FREQUENCY_PERIOD);
            frequency.setName(Constants.BG_DEFAULT_FREQUENCY);
            frequencyByRiskLevel.add(frequency);
        } else if (!treatmentPlan.isPregnancyAnc() && Objects.nonNull(treatmentPlan.getCvdRiskLevel())){
            frequencyByRiskLevel.removeIf(frequency -> Constants.FREQUENCY_BG_CHECK.equals(frequency.getType()));
            FrequencyDTO frequency = new FrequencyDTO();
            frequency.setType(Constants.FREQUENCY_BG_CHECK);
            frequency.setName(Constants.BG_PROVISIONAL_FREQUENCY_NAME);
            frequencyByRiskLevel.add(frequency);
        }
        if (treatmentPlan.isPregnancyAnc()) {
            createPregnancyFrequecy(treatmentPlan, frequencyByRiskLevel);
        }
        return frequencyByRiskLevel;

    }

    /**
     * Creates and adds pregnancy frequecy in frequency list.
     * 
     * @param treatmentPlan
     * @param frequencyByRisk
     */
    private void createPregnancyFrequecy(TreatmentPlanDTO treatmentPlan, List<FrequencyDTO> frequencyByRisk) {
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_PREGNANCY_DETAILS,
                treatmentPlan.getMemberReference(), FhirConstants.PREGNANCY));

        if (!bundle.getEntry().isEmpty()) {
            Observation pregancyObservation = (Observation) bundle.getEntry().getFirst().getResource();
            Date lastMenstrualDate = null;
            PregnancyDetailsDTO pregnancyAncDTO = new PregnancyDetailsDTO();
            spiceConverter.setPregnancySymptomDetails(pregancyObservation, pregnancyAncDTO);
            lastMenstrualDate = pregnancyAncDTO.getLastMenstrualPeriod();
            List<PregnancySymptomDTO> dangerSymptoms = pregnancyAncDTO.getPregnancySymptoms();

            boolean isDangerSymptom = !Objects.isNull(dangerSymptoms) && !dangerSymptoms.isEmpty()
                    && (!(dangerSymptoms.size() == 1 && dangerSymptoms.getFirst().getName().equals(Constants.NO_SYMPTOMS)))
            ? Boolean.TRUE: Boolean.FALSE;

            if (Objects.nonNull(lastMenstrualDate)) {
                int gestationalWeeks = DateUtil.getWeeks(lastMenstrualDate);
                if (gestationalWeeks >= 4 && gestationalWeeks <= 40) {
                    List<FrequencyDTO> defaultFrequencies = treatmentPlan.getFrequencies().stream()
                            .filter(frequency -> frequency.getType().equals(Constants.TREATMENT_PLAN_DEFAULT)).toList();

                    List<String> treatmentPlans = Constants.PREGNANCY_TREATMENT_PLANS.entrySet().stream()
                            .filter(plan -> plan.getValue().contains(gestationalWeeks))
                            .map(Map.Entry::getKey).toList();

                    List<FrequencyDTO> medicalReviewFrequencies = defaultFrequencies.stream().filter(frequency -> frequency
                                    .getName().equals(isDangerSymptom ? Constants.EVERY_TWO_WEEKS : treatmentPlans.get(0)))
                            .toList();
                    frequencyByRisk.removeIf(
                            frequency -> frequency.getType().equalsIgnoreCase(Constants.FREQUENCY_MEDICAL_REVIEW));
                    FrequencyDTO clonedFrequency = medicalReviewFrequencies.getFirst();
                    FrequencyDTO medicalReviewFrequency = clonedFrequency.clone();
                    medicalReviewFrequency.setType(Constants.FREQUENCY_MEDICAL_REVIEW);
                    frequencyByRisk.add(medicalReviewFrequency);
                    ModelMapper mapper = new ModelMapper();
                    List<FrequencyDTO> choFrequencies = defaultFrequencies.stream()
                            .filter(frequency -> frequency.getName().equals(Constants.EVERY_ONE_MONTH)).toList();
                    FrequencyDTO choFrequency = mapper.map(choFrequencies.get(0), FrequencyDTO.class);
                    choFrequency.setType(Constants.FREQUENCY_CHO_CHECK);
                    frequencyByRisk.add(choFrequency);
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void updateTreatmentPlanData(TreatmentPlanDTO treatmentplan) {
        Bundle transactionBundle = new Bundle().setType(BundleType.TRANSACTION);
        
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_CAREPLAN_BY_ID, treatmentplan.getCarePlanId()));
        if (!bundle.getEntry().isEmpty()) {
            CarePlan carePlan = (CarePlan) bundle.getEntry().getFirst().getResource();
            carePlan.setStatus(CarePlanStatus.COMPLETED);

            for (CarePlanActivityComponent carePlanActivityComponent : carePlan.getActivity()) {
                CarePlanActivityDetailComponent detailComponent = carePlanActivityComponent.getDetail();
                switch (detailComponent.getCode().getText()) {
                    case Constants.FREQUENCY_BP_CHECK:
                        mapActivityDetailComponent(detailComponent, treatmentplan.getBpCheckFrequency());
                        break;
                    case Constants.FREQUENCY_BG_CHECK:
                        mapActivityDetailComponent(detailComponent, treatmentplan.getBgCheckFrequency()); 
                        break;
                    case Constants.FREQUENCY_MEDICAL_REVIEW:
                        mapActivityDetailComponent(detailComponent, treatmentplan.getMedicalReviewFrequency());                  
                        break;
                    case Constants.FREQUENCY_HBA1C_CHECK:
                        mapActivityDetailComponent(detailComponent, treatmentplan.getHba1cCheckFrequency());               
                        break;
                    case Constants.FREQUENCY_CHO_CHECK:
                        mapActivityDetailComponent(detailComponent, treatmentplan.getChoCheckFrequency());
                        break;
                    default:
                        break;
                }   
            }
            fhirUtils.setBundle(StringUtil.concatString(ResourceType.CarePlan.toString(), Constants.FORWARD_SLASH, carePlan.getIdPart()),
                        Constants.EMPTY_SPACE,
                        HTTPVerb.PUT, carePlan, transactionBundle, treatmentplan.getProvenance());

        } else {
            List<FrequencyDTO> frequencies = treatmentplan.getMedicalReviewFrequencies();
            createCarePlan(frequencies, transactionBundle, treatmentplan, false);      
        }
        restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transactionBundle));
    }

    /**
     * {@inheritDoc}
     */
    public Date createOrUpdateAppointment(String assessmentType,
                                          Date nextVisitDate,
                                          String memberRef,
                                          String patientRef,
                                          Bundle transactionBundle,
                                          ProvenanceDTO provenance, boolean isProvisional) {
        boolean isBundleSave = false;
        Date appointmentDate = null;
        if (Objects.isNull(transactionBundle)) {
            transactionBundle = new Bundle().setType(BundleType.TRANSACTION);
            isBundleSave = true;
        }
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_APPOINTMENT_BY_RELATED_PERSON_ID,
                Constants.FREQUNCY_TYPE_VALUE_MAP.get(assessmentType), memberRef));
        if (Objects.isNull(bundle) || bundle.getEntry().isEmpty()) {
            appointmentDate = nextVisitDate;
            createAppointment(Constants.FREQUNCY_TYPE_VALUE_MAP.get(assessmentType), nextVisitDate, memberRef, patientRef, transactionBundle, provenance);
        } else if (!isProvisional) {
            Appointment appointment = null;
            for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
                if (entry.getResource() instanceof Appointment appointmentResource) {
                    appointment = appointmentResource;
                }
            }
            if (Objects.nonNull(appointment)) {
                updateAppointment(appointment, transactionBundle, provenance, nextVisitDate, patientRef);
            }
        }
        if (isBundleSave) {
            restApiUtil.postBatchRequest(fhirUtils.getFhirBaseUrl(), restApiUtil.constructRequestEntity(transactionBundle));
        }
        return appointmentDate;
    }

    /**
     * {@inheritDoc}
     */
    public TreatmentPlanResponseDTO getPatientTreatmentPlanDetails(RequestDTO request) {
        if (Objects.isNull(request.getMemberReference())) {
            throw new DataNotFoundException(2004);
        }
        TreatmentPlanResponseDTO response = new TreatmentPlanResponseDTO();
        CarePlan carePlan = getCarePlanForPatient(request.getMemberReference());
        if (!Objects.isNull(carePlan)) {
            response = constructTreatmentPlanRespose(carePlan);
        }
        return response;
    }

    /**
     * Constructs treatment plan response.
     * 
     * @param carePlan Careplan obeject
     * @return TreatmentPlanResponseDTO response
     */
    private TreatmentPlanResponseDTO constructTreatmentPlanRespose(CarePlan carePlan) {
        TreatmentPlanResponseDTO response = new TreatmentPlanResponseDTO();
        response.setCarePlanId(carePlan.getIdPart());
        for (CarePlanActivityComponent carePlanActivityComponent : carePlan.getActivity()) {
            CarePlanActivityDetailComponent detailComponent = carePlanActivityComponent.getDetail();

            switch (detailComponent.getCode().getText()) {
                case Constants.FREQUENCY_BP_CHECK:
                    response.setBpCheckFrequency(detailComponent.getDescription());
                    break;
                case Constants.FREQUENCY_BG_CHECK:
                    response.setBgCheckFrequency(detailComponent.getDescription());
                    break;
                case Constants.FREQUENCY_MEDICAL_REVIEW:
                    response.setMedicalReviewFrequency(detailComponent.getDescription());
                    break;
                case Constants.FREQUENCY_HBA1C_CHECK:
                    response.setHba1cCheckFrequency(detailComponent.getDescription());
                    break;
                case Constants.FREQUENCY_CHO_CHECK:
                    response.setChoCheckFrequency(detailComponent.getDescription());
                    break;
                default:
                    break;
            }   
        }
        return response;
    }

    /**
     * {@inheritDoc}
     */
    public CarePlan getCarePlanForPatient(String memeberReference) {
        CarePlan carePlan = null;
        Bundle bundle = restApiUtil.getBatchRequest(String.format(GET_CAREPLAN_PLAN_MEMBER, memeberReference));
        if (!bundle.getEntry().isEmpty()) {
            carePlan = (CarePlan) bundle.getEntry().getFirst().getResource();
        }
        return carePlan;
    }

    /**
     * {@inheritDoc}
     */
    public Date updateNextVisitDateForPatient(String memberReference,String patientReference, String assessmentType, CarePlan carePlan, ProvenanceDTO provenance, Date assessmentDate) {
        String period = null;
        Integer duration = null;
        Timing timing = null;
        Date visitDate = null;
        for (CarePlanActivityComponent activityComponent : carePlan.getActivity()) {
            if (assessmentType.equals(activityComponent.getDetail().getCode().getText())) {
                timing = activityComponent.getDetail().getScheduledTiming();
                if (Constants.ZERO != timing.getRepeat().getFrequency() && !Objects.isNull(timing.getRepeat().getDurationUnit())) {
                    duration = timing.getRepeat().getFrequency();
                    period = timing.getRepeat().getDurationUnit().toCode();
                }
            }
        }
        if (!Objects.isNull(period) && !Objects.isNull(duration)) {
            visitDate = DateUtil.getTreatmentPlanFollowupDate(period, duration, assessmentDate);
            createOrUpdateAppointment(assessmentType, visitDate, memberReference, patientReference, null, provenance, Boolean.FALSE);
        }
        return visitDate;
    }

    /**
     * <p>
     * The function updates a care plan by adding activities based on provided frequencies
     * and setting its status to completed or draft.
     * </p>
     *
     * @param frequencies       A list of FrequencyDTO objects containing information about the frequencies.
     * @param transactionBundle The bundle to be saved
     * @param treatmentplan     TreatmentPlanDTO treatmentplan to be constructed,
     * @param details           It contains information about the details of the care plan activities.
     * @param carePlan          It represents the care plan that needs to be updated based on the provided
     *                          frequencies, treatment plan, transaction bundle, and details.
     * @return The updated care plan is returned.
     */
    private CarePlan updateCarePlan(List<FrequencyDTO> frequencies, Bundle transactionBundle, TreatmentPlanDTO treatmentplan, List<String> details, CarePlan carePlan) {
        for (FrequencyDTO frequency : frequencies) {
            if (!details.contains(frequency.getType())) {
                carePlan.addActivity(createCarePlanActivity(frequency, treatmentplan, transactionBundle, Boolean.TRUE));
                details.add(frequency.getType());
            }
        }
        carePlan.setStatus(new HashSet<>(details).containsAll(Constants.FREQUENCIES) ? CarePlanStatus.COMPLETED : CarePlanStatus.DRAFT);
        fhirUtils.setBundle(StringUtil.concatString(ResourceType.CarePlan.toString(), Constants.FORWARD_SLASH, carePlan.getIdPart()),
                Constants.EMPTY_SPACE,
                HTTPVerb.PUT, carePlan, transactionBundle, treatmentplan.getProvenance());
        return carePlan;
    }
}