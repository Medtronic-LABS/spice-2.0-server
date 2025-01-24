package com.mdtlabs.coreplatform.spiceservice.staticdata.service;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.MetaDataDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Culture;
import com.mdtlabs.coreplatform.spiceservice.common.dto.DiseaseCategoryDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.FormMetaDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MedicalReviewStaticDataDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MenuDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MetaDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.MetaFormDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Frequency;
import com.mdtlabs.coreplatform.spiceservice.common.dto.StaticMetaDataResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.StaticUserDataResponseDTO;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;


/**
 * <p>
 * This an interface class for static data module you can implemented this class in any
 * class.
 * </p>
 */
public interface StaticDataService {


    /**
     * Getting form data based on the workflow and form type, after getting form meta performing business logics.
     *
     * @param formType form type as a string used to identify the form meta.
     * @return FormMetaDTO object.
     */
    FormMetaDTO getFormDataByFormType(String formType);

    /**
     * Getting form data based on the workflow and form type, after getting form meta performing business logics.
     *
     * @param workflowName workflow name as a string used to identify the form meta.
     * @param formType     form type as a string used to identify the form meta.
     * @return FormMetaDTO object.
     */
    FormMetaDTO getFormDataByFormTypeAndWorkflow(String workflowName, String formType);

    /**
     * Gets a user data like villages and healthfacility and user profile.
     *
     * @return StaticUserDataResponseDTO
     */
    StaticUserDataResponseDTO getUserStaticData();

    /**
     * Gets form datas based on workflows.
     *
     * @param workflowIds
     * @return List<FormMetaDTO>
     */
    MetaDTO getWorkflowFormDataForStaticData(SearchRequestDTO request);

    /**
     * Gets a meta data based in names.
     *
     * @param metaNames meta names
     * @param type      type of the review
     * @return StaticMetaDataResponseDTO
     */
    StaticMetaDataResponseDTO getMetadata(List<String> metaNames);

    /**
     * Fetches all mobile users and health facilities.
     *
     * @return StaticUserDataResponseDTO - This return object contains the list of all mobile users and health facilities.
     */
    StaticUserDataResponseDTO getMobileUsersAndFacilities();

    /**
     * to get all diagnosis information.
     *
     * @return list
     */
    List<DiseaseCategoryDTO> getAllDiagnosis();

    /**
     * Gets NCD releted meta datas like comobidity, complications ..etc.
     * 
     * @return MedicalReviewStaticDataDTO
     */
    MedicalReviewStaticDataDTO getNCDMedicalReviewStaticData();

    /**
     * Gets Non-NCD  meta data like presenting complaints, examinations and others
     * 
     * @param metaNames name of the meta that is to be retrived
     * @param type type of meta
     * @return StaticMetaDataResponseDTO
     */
    StaticMetaDataResponseDTO getNonNCDMetaData(List<String> metaNames, String type);

    /**
     * Validate app version for mobile API
     *
     * @param appVersion - app version from mobile
     * @return Boolean
     */
    boolean checkAppVersion(String appVersionReq);

    /**
     * <p>
     * Fetch meta form data based on form name.
     * </p>
     *
     * @param form - form name
     * @return Meta form Static data
     */
    MetaFormDTO getMetaFormData(String form);

    /**
     * <p>
     * Retrieves the menu based on the search request.
     * </p>
     *
     * @param searchRequestDTO the search request containing criteria for fetching the menu
     * @return the MenuDTO object that matches the search criteria
     */
    MenuDTO getMenu(SearchRequestDTO searchRequestDTO);

        /**
     * <p>
     * This method is used to returns a list of Symptom
     * </p>
     *
     * @return {@link List<Symptom>} The List of Symptom is being returned.
     */
    public List<Symptom> getSymptoms();

    /**
     * <p>
     *     Used to find the culture for given name.
     * <p/>
     *
     * @param name Name of the culture.
     * @return {@link Culture} Culture object for given name.
     */
    public Culture findCulture(String name);

    /**
     * Gets all frequencies.
     * 
     * @return List<Frequency>
     */
    public List<Frequency> getAllFrequencies();

    /**
     * Gets all lifestyles.
     * 
     * @return List<Frequency>
     */
    List<MetaDataDTO> getLifestyles();

    /**
     * Gets all culture values
     *
     * @return List<Frequency>
     */
    List<MetaDataDTO> getMessageMetaData();
}
