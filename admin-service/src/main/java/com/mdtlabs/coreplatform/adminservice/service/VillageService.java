package com.mdtlabs.coreplatform.adminservice.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;

/**
 * <p>
 * This an interface class for Village module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Karthick M created on Dec 30, 2023
 */
public interface VillageService {

    /**
     * <p>
     * Adds a list of villages to a specified chiefdom.
     * </p>
     *
     * @param chiefdom {@link Chiefdom} the chiefdom to which the villages will be added
     * @param villages {@link Village} the list of villages to be added to the chiefdom
     */
    void addVillagesToChiefdom(Chiefdom chiefdom, List<Village> villages);

    /**
     * <p>
     *     This method used to get and update member sequence village details based on the villageId.
     * </p>
     *
     * @param id the villageId
     * @return Long  village member sequence details.
     */
     Long getMemberSequenceByVillageId(Long id);

    /**
     * <p>
     *     This method used to get village details based on the villageId and update household sequence based on village id.
     * </p>
     *
     * @param id the villageId
     * @return Long village household sequence details.
     */
    Long getHouseholdSequenceByVillageId(Long id);

    /**
     * <p>
     *     Returns the village details based on respective village id.
     * </p>
     *
     * @param id - the village id.
     * @return   - Village details of village.
     */
    VillageDTO getVillageDetailsByVillageId(Long id);

    /*
     * Downloads data as a file.
     * <p>
     * This method facilitates the downloading of data by generating a byte array that represents the file content.
     * The content could be dynamically generated based on the provided countryId, which acts as a filter or identifier
     * for the data to be included in the file. The byte array can then be sent to the client, allowing them to download
     * the file.
     * </p>
     *
     * @param countryId The identifier for the country, which is used to filter or determine the data to be included in the
     *                  downloaded file.
     * @param appTypes A flag that represent it is Community or not.
     * @return A byte array representing the content of the file to be downloaded.
     */
    byte[] downloadRegionFile(Long countryId, List<String> appTypes);

    /**
     * <p>
     * The function `uploadFile` reads an Excel file, extracts data from it, and saves the data into a
     * database.
     * </p>
     *
     * @param multipartFile The "multipartFile" parameter is of type MultipartFile, which is a Spring framework class used
     *                      for handling file uploads. It represents a file that has been uploaded by a user through a form
     *                      or API request. In this code snippet, the "file" parameter is used to read the contents of the
     *                      uploaded.
     * @param appTypes A flag that represent it is community or not.
     */
    void uploadRegionFile(MultipartFile multipartFile, List<String> appTypes);

}
