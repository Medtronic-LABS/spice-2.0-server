package com.mdtlabs.coreplatform.spiceservice.staticdata.controller;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.annotations.ConfigureAppType;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FormMetaDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewStaticDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MenuDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MetaFormDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.StaticMetaDataResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.StaticUserDataResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Frequency;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessCode;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.spiceservice.staticdata.service.StaticDataService;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * StaticDataController provides endpoints for getting static data like forms, etc.
 * It interacts with the service layer to perform necessary business logic and
 * returns appropriate responses to the client
 */
@RestController
@RequestMapping(value = "/static-data")
@Validated
public class StaticDataController {

    private final StaticDataService staticDataService;

    public StaticDataController(StaticDataService staticDataService) {
        this.staticDataService = staticDataService;
    }

    /**
     * Gets a user static data
     *
     * @return SuccessResponse<StaticUserDataResponseDTO>
     */
    @ConfigureAppType
    @PostMapping("/user-data")
    public SuccessResponse<StaticUserDataResponseDTO> getUserStatiCData() {
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getUserStaticData(), HttpStatus.OK);
    }

    /**
     * It provides end point to get the form data of workflow.
     *
     * @return SuccessResponse<List < FormMetaDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/form-data")
    public SuccessResponse<List<FormMetaDTO>> getFormData(@RequestBody SearchRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getWorkflowFormDataForStaticData(request), HttpStatus.OK);
    }

    /**
     * It provides end point to get the meta data of workflow.
     *
     * @return SuccessResponse<List < FormMetaDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data")
    public SuccessResponse<StaticMetaDataResponseDTO> getMetaData(@RequestBody SearchRequestDTO request) {
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA,
                staticDataService.getMetadata(request.getMetaNames()), HttpStatus.OK);
    }

    /**
     * Set metadata cache and returns a success response with
     * static metadata.
     *
     * @return A SuccessResponse object containing a StaticMetaDataResponseDTO object is being returned.
     */
    @PostMapping("/set-meta-data-cache")
    public SuccessResponse<StaticMetaDataResponseDTO> setMetaData() {
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA,
                staticDataService.setMetaDateCache(), HttpStatus.OK);
    }

    /**
     * It provides end point to get the static meta data for both above and under 5Y patients.
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/iccm-abovefive")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaData() {
        List<String> metaNames = List.of(Constants.SYSTEMIC_EXAMINATION, Constants.DISEASE, Constants.PRESENTING_COMPLAINTS,
                Constants.MEDICAL_SUPPLIES, Constants.COST, Constants.PATIENT_STATUS, Constants.DOSAGE_FREQUENCY);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.ABOVE_FIVE_YEARS),
                HttpStatus.OK);
    }

    /**
     * It provides end point to get the static meta data for both above and under 5Y patients.
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/iccm-under-two-months")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaDataForICCMUnderTwoMonths() {
        List<String> metaNames = List.of(Constants.EXAMINATION, Constants.DISEASE, Constants.PATIENT_STATUS,
                Constants.COUNSELLED_ON, Constants.DOSAGE_FREQUENCY, Constants.IMMUNISATION_STATUS);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.UNDER_TWO_MONTHS),
                HttpStatus.OK);
    }

    /**
     * It provides end point to get the static meta data for both above and under 5Y patients.
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/iccm-under-five-years")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaDataForICCMUnderFiveYears() {
        List<String> metaNames = List.of(Constants.EXAMINATION, Constants.SYSTEMIC_EXAMINATION,
                Constants.SYMPTOMS_BY_CATEGORY, Constants.DOSAGE_FREQUENCY, Constants.DISEASE,
                Constants.PATIENT_STATUS, Constants.IMMUNISATION_STATUS, Constants.MUAC);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.UNDER_FIVE_YEARS),
                HttpStatus.OK);
    }

    /**
     * It provides end point to get the static meta data for mother and neonate ANC .
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/mother-neonate-anc")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaDataForMotherNeonateANC() {
        List<String> metaNames = List.of(Constants.PRESENTING_COMPLAINTS, Constants.OBSTETRIC_EXAMINATION,
                Constants.PREGNANCY_HISTORY, Constants.PATIENT_STATUS, Constants.BLOOD_GROUP,
                Constants.DOSAGE_FREQUENCY, Constants.DISEASE);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.ANC_REVIEW),
                HttpStatus.OK);
    }

    /**
     * It provides end point to get the static meta data for mother and neonate PNC Mother.
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/mother-neonate-pnc-mother")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaDataForMotherNeonatePNCMother() {
        List<String> metaNames = List.of(Constants.PRESENTING_COMPLAINTS, Constants.SYSTEMIC_EXAMINATION,
                Constants.DOSAGE_FREQUENCY, Constants.DISEASE, Constants.PATIENT_STATUS);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.PNC_MOTHER_REVIEW),
                HttpStatus.OK);
    }

    /**
     * It provides end point to get the static meta data for mother and neonate PNC Baby.
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/mother-neonate-pnc-baby")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaDataForMotherNeonatePNCBaby() {
        List<String> metaNames = List.of(Constants.PRESENTING_COMPLAINTS, Constants.SYSTEMIC_EXAMINATION,
                Constants.OBSTETRIC_EXAMINATION, Constants.PATIENT_STATUS, Constants.DOSAGE_FREQUENCY, Constants.DISEASE);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.PNC_CHILD_REVIEW),
                HttpStatus.OK);
    }

    /**
     * It provides end point to get the static metadata for mother delivery.
     *
     * @return SuccessResponse<List < StaticMetaDataResponseDTO>> Response of the end point.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/mother-delivery")
    public SuccessResponse<StaticMetaDataResponseDTO> getStaticMetaDataForMotherDelivery() {
        List<String> metaNames = List.of(Constants.DELIVERY_AT, Constants.DELIVERY_BY, Constants.DELIVERY_TYPE,
                Constants.DELIVERY_STATUS, Constants.NEONATE_OUTCOME, Constants.RISK_FACTORS,
                Constants.CONDITION_OF_MOTHER, Constants.NEONATE_OUTCOME, Constants.MOTHER_DELIVERY_STATUS,
                Constants.SYMPTOMS_BY_CATEGORY, Constants.DOSAGE_FREQUENCY,
                Constants.STATE_OF_PERINEUM, Constants.PNC_NEONATE_OUTCOME);
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getNonNCDMetaData(metaNames,
                Constants.MOTHER_DELIVERY_REVIEW),
                HttpStatus.OK);
    }

    /**
     * Fetches mobile users and facilities data.
     *
     * @return SuccessResponse<StaticUserDataResponseDTO> - The response object containing the status code,
     * the data (mobile users and facilities), and the HTTP status.
     */
    @ConfigureAppType
    @PostMapping("/meta-data/users-and-facilities")
    public SuccessResponse<StaticUserDataResponseDTO> getMobileUsersAndFacilities() {
        return new SuccessResponse<>(SuccessCode.GET_USER_STATIC_DATA, staticDataService.getMobileUsersAndFacilities(), HttpStatus.OK);
    }

    /**
     * <p>
     * Validate app version for mobile API
     * </p>
     *
     * @param appVersion - app version from mobile
     * @return User - response of the updated user
     */
    @PostMapping("/app-version")
    public SuccessResponse<Object> checkAppVersion(@RequestHeader("App-Version") String appVersionReq) {
        boolean response = staticDataService.checkAppVersion(appVersionReq);
        if (response) {
            return new SuccessResponse<>(SuccessCode.APP_VERSION_UPTODATE, response, HttpStatus.OK);
        }
        return new SuccessResponse<>(SuccessCode.UPDATE_APP_VERSION, response
                , HttpStatus.OK, appVersionReq);
    }

    /**
	 * <p>
	 * Fetch all the medical review related data for spice service.
	 * </p>
	 * 
	 * @return Medical Review Static data
	 */
	@PostMapping("/ncd-medical-review")
	public SuccessResponse<MedicalReviewStaticDataDTO> getMedicalReviewStaticData() {
		return new SuccessResponse<>(SuccessCode.GET_MEDICAL_REVIEW_STATIC_DATA,
				staticDataService.getNCDMedicalReviewStaticData(), HttpStatus.OK);
	}

    /**
     * <p>
     * Fetch meta form data based on form name.
     * </p>
     *
     * @param form - form name
     * @return Meta form Static data
     */
    @GetMapping("/get-meta-form")
    public SuccessResponse<MetaFormDTO> getMetaFormData(@RequestParam(Constants.FORM) String form) {
        return new SuccessResponse<>(SuccessCode.GET_META_DATA,
                staticDataService.getMetaFormData(form), HttpStatus.OK);
    }

    /**
     * <p>
     * Fetches the menu based on the provided search request.
     * </p>
     *
     * @param searchRequestDTO {@link SearchRequestDTO} the search request containing criteria for fetching the menu
     * @return {@link SuccessResponse<MenuDTO>} the response containing the menu data and HTTP status
     */
    @PostMapping("/menu")
    public SuccessResponse<MenuDTO> getMenu(@RequestBody SearchRequestDTO searchRequestDTO) {
        return new SuccessResponse<>(SuccessCode.GET_MENU,
                staticDataService.getMenu(searchRequestDTO), HttpStatus.OK);
    }

    /**
     * Gets all frequencies.
     *
     * @return List<Frequency>
     */
    @PostMapping("/frequencies")
    public List<Frequency> getAllFrequencies() {
        return staticDataService.getAllFrequencies();
    }
}
