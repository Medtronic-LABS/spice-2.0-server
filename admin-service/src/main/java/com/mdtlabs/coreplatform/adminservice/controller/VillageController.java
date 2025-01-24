package com.mdtlabs.coreplatform.adminservice.controller;

import java.util.List;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.adminservice.message.SuccessCode;
import com.mdtlabs.coreplatform.adminservice.message.SuccessResponse;
import com.mdtlabs.coreplatform.adminservice.service.VillageService;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.SearchRequestDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;

/**
 * <p>
 * The VillageController class is a REST controller that handles requests related to uploading village
 * data from a file.
 * </p>
 *
 * @author Divya S
 */
@RestController
@RequestMapping()
public class VillageController {

    private final VillageService villageService;

    public VillageController(VillageService villageService) {
        this.villageService = villageService;
    }

    /**
     * <p>
     * The function `downloadRegionFile` is a POST request handler that downloads a file and returns a success
     * response with the file bytes.
     * </p>
     *
     * @return The method is returning a SuccessResponse object with a byte array and HttpStatus.OK.
     */
    @PostMapping("/region-details/download-file")
    public byte[] downloadRegionFile(HttpServletResponse response, @RequestBody SearchRequestDTO request) {
        byte[] bytes = villageService.downloadRegionFile(request.getCountryId(), request.getAppTypes());
        response.setContentType("applicatioapplication/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-disposition", "attachment;filename=" + "region.xlsx");
        response.setHeader("charset", "iso-8859-1");
        response.setContentLength(bytes.length);
        return bytes;
    }

    /**
     * <p>
     * The function`uploadRegionFile` in a Java controller class handles the uploading of a file containing
     * village data and returns a success response.
     * </p>
     *
     * @param multipartFile The `multipartFile` parameter is of type `MultipartFile` and represents the
     *                      file that is being uploaded. It is annotated with `@RequestParam` to indicate that it is a request
     *                      parameter and should be obtained from the request's multipart form data.
     * @param appTypes The appTypes is the List of String that represent Community or Non-Community.
     * @return The method is returning a SuccessResponse object with a list of Village objects.
     */
    @PostMapping("/region-details/upload-file")
    public SuccessResponse<List<Village>> uploadRegionFile(@RequestParam("file") MultipartFile multipartFile, @RequestParam(Constants.APP_TYPES) List<String> appTypes) {
        villageService.uploadRegionFile(multipartFile, appTypes);
        return new SuccessResponse<>(SuccessCode.FILE_UPLOAD, HttpStatus.OK);
    }

    /**
     * <p>
     *     Returns the village member sequence count based on the respective village id.
     * </p>
     *
     * @param requestDTO request with village id
     * @return sequence number of patient.
     */
    @PostMapping("/village/member-sequence")
    public Long getMemberSequenceByVillageId(@RequestBody SearchRequestDTO requestDTO) {
        return villageService.getMemberSequenceByVillageId(requestDTO.getId());
    }

    /**
     * <p>
     *     Returns the village household sequence count based on the respective village id.
     * </p>
     *
     * @param requestDTO request with village id.
     * @return sequence number of household
     */
    @PostMapping("/village/household-sequence")
    public Long getHouseholdSequenceByVillageId(@RequestBody SearchRequestDTO requestDTO) {
        return villageService.getHouseholdSequenceByVillageId(requestDTO.getId());
    }

    /**
     * <p>
     *     Returns the village details based on the respective village id.
     * </p>
     *
     * @param requestDTO request with village id.
     * @return village details.
     */
    @PostMapping("/village")
    public VillageDTO getVillageDetails(@RequestBody SearchRequestDTO requestDTO) {
        return villageService.getVillageDetailsByVillageId(requestDTO.getId());
    }
}
