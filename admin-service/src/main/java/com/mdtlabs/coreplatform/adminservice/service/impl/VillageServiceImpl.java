package com.mdtlabs.coreplatform.adminservice.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.VillageDTO;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.mdtlabs.coreplatform.adminservice.AdminConstants;
import com.mdtlabs.coreplatform.adminservice.repository.DistrictRepository;
import com.mdtlabs.coreplatform.adminservice.repository.ChiefdomRepository;
import com.mdtlabs.coreplatform.adminservice.repository.VillageRepository;
import com.mdtlabs.coreplatform.adminservice.service.VillageService;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotAcceptableException;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.District;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Organization;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Chiefdom;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Village;
import com.mdtlabs.coreplatform.commonservice.common.repository.OrganizationRepository;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

/**
 * <p>
 * The VillageServiceImpl class is a service implementation that handles the uploading and downloading of a file.
 * </p>
 *
 * @author Divya S
 */
@Service
public class VillageServiceImpl implements VillageService {

    private final ChiefdomRepository chiefdomRepository;

    private final DistrictRepository districtRepository;

    private final VillageRepository villageRepository;

    private final OrganizationRepository organizationRepository;

    private final RedisTemplate<String, Map<Long, List<Long>>> redisTemplate;

    public VillageServiceImpl(ChiefdomRepository chiefdomRepository, DistrictRepository districtRepository, VillageRepository villageRepository, OrganizationRepository organizationRepository, RedisTemplate<String, Map<Long, List<Long>>> redisTemplate) {
        this.chiefdomRepository = chiefdomRepository;
        this.districtRepository = districtRepository;
        this.villageRepository = villageRepository;
        this.organizationRepository = organizationRepository;
        this.redisTemplate = redisTemplate;
    }

    /**
     * Populates metadata maps with region details.
     * <p>
     * This method retrieves region details from the repository and populates four maps with hierarchical geographical data:
     * countries, districts within countries, chiefdoms within districts, and villages within chiefdoms. It ensures that each
     * map contains unique entries and establishes parent-child relationships between different administrative levels by setting
     * parent tenant IDs where applicable.
     * </p>
     * <p>
     * The method iterates over each region detail, checking and inserting data into the respective maps. For countries, it checks
     * if the country already exists in the countryDistrict map; if not, it adds the country. Similar checks and insertions are done
     * for districts, chiefdoms, and villages. Additionally, it sets the parent tenant ID for districts, chiefdoms, and villages to
     * maintain the hierarchy.
     * </p>
     *
     * @param country          A map to hold country names and their corresponding IDs.
     * @param countryDistrict  A map to hold country names as keys and another map as value, which contains district names and their IDs.
     * @param districtChiefdom A map to hold district names as keys and another map as value, which contains chiefdom names and their IDs.
     * @param chiefdomVillage  A map to hold chiefdom names as keys and another map as value, which contains village names and their IDs.
     */
    private void setMetaData(Map<String, Long> country,
                             Map<String, Map<String, Long>> countryDistrict,
                             Map<String, Map<String, Long>> districtChiefdom,
                             Map<String, Map<String, Long>> chiefdomVillage) {

        List<Map<String, Object>> regionDetails = villageRepository.getRegionDetails();
        if (!regionDetails.isEmpty()) {
            regionDetails.forEach(details -> {
                if (!Objects.isNull(details.get(AdminConstants.KEY_COUNTRY_NAME)) && !countryDistrict.containsKey(details.get(AdminConstants.KEY_COUNTRY_NAME))) {
                    countryDistrict.put(String.valueOf(details.get(AdminConstants.KEY_COUNTRY_NAME)), new HashMap<>());
                    country.put(String.valueOf(details.get(AdminConstants.KEY_COUNTRY_NAME)), Long.valueOf(details.get(AdminConstants.KEY_COUNTRY_ID).toString()));
                    countryDistrict.get(details.get(AdminConstants.KEY_COUNTRY_NAME))
                            .put(AdminConstants.KEY_PARENT_TENANT_ID, Long.valueOf(details.get(AdminConstants.KEY_COUNTRY_TENANT_ID).toString()));
                }
                if (!Objects.isNull(details.get(AdminConstants.KEY_DISTRICT_NAME)) && !districtChiefdom.containsKey(details.get(AdminConstants.KEY_DISTRICT_NAME))) {
                    districtChiefdom.put(String.valueOf(details.get(AdminConstants.KEY_DISTRICT_NAME)), new HashMap<>());
                }
                if (!Objects.isNull(details.get(AdminConstants.KEY_CHIEFDOM_NAME)) && !chiefdomVillage.containsKey(details.get(AdminConstants.KEY_CHIEFDOM_NAME))) {
                    chiefdomVillage.put(String.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_NAME)), new HashMap<>());
                }
                if (!Objects.isNull(details.get(AdminConstants.KEY_DISTRICT_NAME))) {
                    countryDistrict.get(details.get(AdminConstants.KEY_COUNTRY_NAME))
                            .put(String.valueOf(details.get(AdminConstants.KEY_DISTRICT_NAME)), Long.valueOf(details.get(AdminConstants.KEY_DISTRICT_ID).toString()));
                }

                if (!Objects.isNull(details.get(AdminConstants.KEY_CHIEFDOM_NAME))) {
                    districtChiefdom.get(details.get(AdminConstants.KEY_DISTRICT_NAME))
                            .put(String.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_NAME)), Long.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_ID).toString()));
                    districtChiefdom.get(details.get(AdminConstants.KEY_DISTRICT_NAME))
                            .put(AdminConstants.KEY_PARENT_TENANT_ID, Long.valueOf(details.get(AdminConstants.KEY_DISTRICT_TENANT_ID).toString()));
                }

                if (!Objects.isNull(details.get(AdminConstants.KEY_VILLAGE_NAME))) {
                    chiefdomVillage.get(details.get(AdminConstants.KEY_CHIEFDOM_NAME))
                            .put(String.valueOf(details.get(AdminConstants.KEY_VILLAGE_NAME)), Long.valueOf(details.get(AdminConstants.KEY_VILLAGE_ID).toString()));
                    chiefdomVillage.get(details.get(AdminConstants.KEY_CHIEFDOM_NAME))
                            .put(AdminConstants.KEY_PARENT_TENANT_ID, Long.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_TENANT_ID).toString()));
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    public void uploadRegionFile(MultipartFile file, List<String> appTypes) {
        try (XSSFWorkbook book = new XSSFWorkbook(file.getInputStream())) {
            XSSFSheet sheet = book.getSheetAt(0);
            DataFormatter formatter = new DataFormatter();
            Map<String, Long> country = new HashMap<>();
            Map<String, Map<String, Long>> countryDistrict = new HashMap<>();
            Map<String, Map<String, Long>> districtChiefdom = new HashMap<>();
            Map<String, Map<String, Long>> chiefdomVillage = new HashMap<>();
            setMetaData(country, countryDistrict, districtChiefdom, chiefdomVillage);
            boolean isCommunity = appTypes.stream().allMatch(Constants.APP_TYPE_COMMUNITY :: equals);

            if (isCommunity) {
                validateHeaders(sheet, Set.of(
                        AdminConstants.COUNTRY, AdminConstants.COUNTRY_CODE, AdminConstants.DISTRICT, AdminConstants.DISTRICT_CODE,
                        AdminConstants.CHIEFDOM, AdminConstants.CHIEFDOM_CODE, AdminConstants.VILLAGE, AdminConstants.VILLAGE_CODE,
                        AdminConstants.VILLAGE_TYPE
                ), AdminConstants.NINE);
            }

            for (Row row : sheet) {
                try {
                    if (row.getRowNum() == 0 || row.getLastCellNum() == -1) continue;

                    Map<String, String> rowMap = new HashMap<>();
                    for (Cell cell : row) {
                        String cellValue = formatter.formatCellValue(cell);
                        rowMap.put(getHeader(cell, formatter), cellValue.strip());
                    }

                    Long countryId = null;
                    Long districtId = null;
                    Long chiefdomId = null;
                    Long villageId = null;

                    String countryName = null;
                    String districtName = null;
                    String chiefdomName = null;
                    String villageName = null;
                    if (isCommunity) {
                        countryName = rowMap.get(AdminConstants.COUNTRY);
                        districtName = rowMap.get(AdminConstants.DISTRICT);
                        chiefdomName = rowMap.get(AdminConstants.CHIEFDOM);
                        villageName = rowMap.get(AdminConstants.VILLAGE);
                        String chiefdomCode = rowMap.get(AdminConstants.CHIEFDOM_CODE);
                        String villageCode = rowMap.get(AdminConstants.VILLAGE_CODE);

                        if (!isValidCode(chiefdomCode, villageCode)) {
                            Logger.logWarn("Invalid code(s) in row " + row.getRowNum());
                            continue;
                        }
                    } else {
                        countryName = Objects.nonNull(rowMap.get(AdminConstants.COUNTRY)) ? rowMap.get(AdminConstants.COUNTRY).strip() : rowMap.get(AdminConstants.COUNTRY);
                        districtName = Objects.nonNull(rowMap.get(AdminConstants.COUNTY)) ? rowMap.get(AdminConstants.COUNTY).strip() : rowMap.get(AdminConstants.VILLAGE);
                        chiefdomName = Objects.nonNull(rowMap.get(AdminConstants.SUB_COUNTY)) ? rowMap.get(AdminConstants.SUB_COUNTY).strip() : rowMap.get(AdminConstants.VILLAGE);
                        villageName = Objects.nonNull(rowMap.get(AdminConstants.VILLAGE)) ? rowMap.get(AdminConstants.VILLAGE).strip() : rowMap.get(AdminConstants.VILLAGE);
                    }

                    if (StringUtils.isEmpty(countryName) || StringUtils.isEmpty(districtName) ||
                            StringUtils.isEmpty(chiefdomName) || StringUtils.isEmpty(villageName)) {
                        Logger.logError("Required field(s) missing in row " + row.getRowNum());
                        throw new DataNotFoundException(10001);
                    }

                    countryId = country.get(countryName);
                    if (countryId == null) {
                        Logger.logError("Country name is unavailable");
                        throw new DataNotFoundException(10001);
                    }

                    Organization organization;
                    Map<String, Long> districts = countryDistrict.get(countryName);
                    if (districts.containsKey(districtName)) {
                        districtId = districts.get(districtName);
                    } else {
                        organization = createTenant("district", districts.get(AdminConstants.KEY_PARENT_TENANT_ID), districtName);
                        districtId = districtRepository.save(
                                isCommunity
                                        ? new District(districtName, rowMap.get(AdminConstants.DISTRICT_CODE), countryId, organization.getId())
                                        : new District(districtName, countryId, organization.getId())
                        ).getId();
                        updateOrganization(organization, districtId);
                        updateMap(districts, districtName, districtId);
                        createMap(districtChiefdom, districtName, organization.getId());
                    }
                    Map<String, Long> chiefdoms = districtChiefdom.get(districtName);
                    if (chiefdoms.containsKey(chiefdomName)) {
                        chiefdomId = chiefdoms.get(chiefdomName);
                    } else {
                        organization = createTenant("chiefdom", chiefdoms.get(AdminConstants.KEY_PARENT_TENANT_ID), chiefdomName);
                        chiefdomId = chiefdomRepository.save(
                                isCommunity
                                        ? new Chiefdom(chiefdomName, rowMap.get(AdminConstants.CHIEFDOM_CODE), countryId, districtId, organization.getId())
                                        : new Chiefdom(chiefdomName, countryId, districtId, organization.getId())
                        ).getId();
                        updateOrganization(organization, chiefdomId);
                        updateMap(chiefdoms, chiefdomName, chiefdomId);
                        createMap(chiefdomVillage, chiefdomName, organization.getId());
                    }
                    Map<String, Long> villages = chiefdomVillage.get(chiefdomName);
                    if (villages.containsKey(villageName)) {
                        villageId = villages.get(villageName);
                    } else {
                        villageId = villageRepository.save(
                                isCommunity
                                        ? new Village(villageName, rowMap.get(AdminConstants.VILLAGE_CODE), rowMap.get(AdminConstants.VILLAGE_TYPE), countryId, districtId, chiefdomId)
                                        : new Village(villageName, countryId, districtId, chiefdomId)
                        ).getId();
                        updateMap(villages, villageName, villageId);
                    }
                    rowMap.clear();
                } catch (DataNotFoundException | DataNotAcceptableException exception) {
                    Logger.logError(StringUtil.constructString(ErrorConstants.INVALID_DATA));
                }
            }
            redisTemplate.delete(Constants.ORGANIZATION_REDIS_KEY);

        } catch (DataNotFoundException | IOException ioException) {
            Logger.logError(StringUtil.constructString(ErrorConstants.INVALID_FILE));
            throw new DataNotFoundException(10004);
        }
    }

    /**
     * <p>
     * This method is used to validate the code.
     * </p>
     *
     * @param chiefdomCode
     * @param villageCode
     */
    private boolean isValidCode(String chiefdomCode, String villageCode) {
        return AdminConstants.THREE >= chiefdomCode.length() && AdminConstants.FOUR >= villageCode.length();
    }

    /**
     * Creates a organization.
     *
     * @param formName
     * @param parantTenantId
     * @param name
     * @return Organization
     */
    private Organization createTenant(String formName, Long parantTenantId, String name) {
        Organization organization = new Organization(formName, name, parantTenantId);
        organization = organizationRepository.save(organization);
        return organization;
    }

    /**
     * Updates the organization's form data ID and persists the changes.
     * <p>
     * This method sets the provided form data ID to the specified organization entity and saves the updated entity
     * to the repository. It is primarily used to link an organization with a specific form data, such as a district,
     * chiefdom, or village, by updating the organization's form data ID to the ID of the newly created or updated entity.
     * </p>
     *
     * @param organization The organization entity to be updated.
     * @param formDataId   The ID of the form data to be associated with the organization.
     * @return The updated organization entity after saving it to the repository.
     */
    private Organization updateOrganization(Organization organization, Long formDataId) {
        organization.setFormDataId(formDataId);
        return organizationRepository.save(organization);
    }

    /**
     * Updates the specified map by associating the given name with the specified ID.
     * <p>
     * This method is used to update a map that holds a collection of names and their corresponding IDs.
     * It inserts a new entry into the map or updates the existing entry for the given name with the new ID.
     * </p>
     *
     * @param dataMap The map to be updated.
     * @param name    The name to be added or updated in the map.
     * @param id      The ID to be associated with the given name.
     */
    private void updateMap(Map<String, Long> dataMap, String name, Long id) {
        dataMap.put(name, id);
    }

    /**
     * Creates a map.
     *
     * @param dataMap
     * @param name
     * @param tenantId
     */
    private void createMap(Map<String, Map<String, Long>> dataMap, String name, Long tenantId) {
        dataMap.put(name, new HashMap<>());
        updateMap(dataMap.get(name), AdminConstants.KEY_PARENT_TENANT_ID, tenantId);
    }

    /**
     * <p>
     * The function "getHeader" returns the lowercase formatted value of the cell in the first row and
     * same column as the given cell.
     * </p>
     *
     * @param cell      The "cell" parameter is the cell for which we want to retrieve the header.
     * @param formatter The formatter is an instance of the DataFormatter class, which is used to format
     *                  the cell values as strings. It provides methods to format the cell values based on their data
     *                  type and formatting applied to them. In this case, the formatCellValue() method is used to get
     *                  the formatted value of the cell
     * @return The method is returning a string value.
     */
    private String getHeader(Cell cell, DataFormatter formatter) {
        return
                formatter.formatCellValue(cell.getSheet().getRow(0).getCell(cell.getColumnIndex())).toLowerCase();
    }

    /**
     * <p>
     * This method is used to validate the headers in sheet and its count.
     * </p>
     *
     * @param sheet
     * @param expectedHeaders
     * @param expectedHeaderCount
     * @throws DataNotFoundException
     */
    private void validateHeaders(XSSFSheet sheet, Set<String> expectedHeaders, int expectedHeaderCount) throws DataNotFoundException {
        XSSFRow headerRow = sheet.getRow(AdminConstants.ZERO);
        //rows != expectedRowCount ||
        if (Objects.isNull(headerRow)) {
            throw new DataNotFoundException();
        }
        Set<String> headersFound = new HashSet<>();
        int headerCount = AdminConstants.ZERO;
        for (int cellIndex = AdminConstants.ZERO; cellIndex < headerRow.getLastCellNum(); cellIndex++) {
            XSSFCell cell = headerRow.getCell(cellIndex);
            // Add cell value to headersFound set
            if (cell == null || cell.getCellType() == CellType.BLANK) {
                continue; // Skip blank cells
            }
            String cellValue = cell.getStringCellValue().trim(); // Read and trim the value
            if (!cellValue.isEmpty()) {
                headersFound.add(cellValue);
                headerCount++;
            }
        }
        if (headerCount != expectedHeaderCount || !headersFound.containsAll(expectedHeaders)) {
            throw new DataNotFoundException();
        }
    }
    
    /**
     * {@inheritDoc}
     */
    public void addVillagesToChiefdom(Chiefdom chiefdom, List<Village> villages) {
        List<Village> existingVillages = villageRepository.getVillages(chiefdom.getCountryId(), chiefdom.getDistrictId(), chiefdom.getId(), null);
        Map<String, Village> metaVillages = new HashMap<>();

        if (Objects.nonNull(existingVillages)) {
            existingVillages.forEach(village -> metaVillages.put(village.getName(), village));
        }

        villages.forEach(village -> {
            if (!metaVillages.containsKey(village.getName().strip())) {
                villageRepository.save(new Village(village.getName().strip(), village.getCode(), village.getType(), chiefdom.getCountryId(), chiefdom.getDistrictId(), chiefdom.getId()));
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    public byte[] downloadRegionFile(Long countryId, List<String> appTypes) {
        List<Map<String, Object>> regionDetails = villageRepository.getRegionDetails(countryId);

        try (Workbook workbook = new XSSFWorkbook();
             ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
            Sheet sheet = workbook.createSheet(AdminConstants.WORKSHEET);
            boolean isCommunity = appTypes.stream().allMatch(Constants.APP_TYPE_COMMUNITY :: equals);

            // Set headers based on the type
            if (!isCommunity) {
                setNonCommunityHeaders(sheet);
            } else {
                setCommunityHeaders(sheet);
            }

            // Populate data rows
            int rowNum = 1;
            for (Map<String, Object> details : regionDetails) {
                Row row = sheet.createRow(rowNum++);
                if (!isCommunity) {
                    populateNonCommunityRow(row, details);
                } else {
                    populateCommunityRow(row, details);
                }
            }
            workbook.write(byteArrayOutputStream);
            Logger.logInfo("Excel file generated successfully.");
            return byteArrayOutputStream.toByteArray();

        } catch (IOException e) {
            throw new SpiceValidation(10004);
        }
    }

    /**
     * <p>
     * This method is set the headers based on the type that isCommunity is true.
     * </p>
     *
     * @param sheet
     */
    private void setNonCommunityHeaders(Sheet sheet) {
        // Create header row
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue(StringUtils.capitalize(AdminConstants.COUNTRY));
        headerRow.createCell(1).setCellValue(StringUtils.capitalize(AdminConstants.COUNTY));
        headerRow.createCell(2).setCellValue(StringUtil.capitalizeEachWord(AdminConstants.SUB_COUNTY));
        headerRow.createCell(3).setCellValue(StringUtils.capitalize(AdminConstants.VILLAGE));
    }

    /**
     * <p>
     * This method is set the headers based on the type that isCommunity is false.
     * </p>
     *
     * @param sheet
     */
    private void setCommunityHeaders(Sheet sheet) {
        // Create header row
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue(AdminConstants.COUNTRY);
        headerRow.createCell(1).setCellValue(AdminConstants.COUNTRY_CODE);
        headerRow.createCell(2).setCellValue(AdminConstants.DISTRICT);
        headerRow.createCell(3).setCellValue(AdminConstants.DISTRICT_CODE);
        headerRow.createCell(4).setCellValue(AdminConstants.CHIEFDOM);
        headerRow.createCell(5).setCellValue(AdminConstants.CHIEFDOM_CODE);
        headerRow.createCell(6).setCellValue(AdminConstants.VILLAGE);
        headerRow.createCell(7).setCellValue(AdminConstants.VILLAGE_CODE);
        headerRow.createCell(8).setCellValue(AdminConstants.VILLAGE_TYPE);
    }

    /**
     * <p>
     * This method is populate the rows based on the type that isCommunity is true.
     * </p>
     *
     * @param row
     * @param details
     */
    private void populateNonCommunityRow(Row row, Map<String, Object> details) {
        String countryName = Objects.nonNull(details.get(AdminConstants.KEY_COUNTRY_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_COUNTRY_NAME)) : Constants.EMPTY;
        String districtName = Objects.nonNull(details.get(AdminConstants.KEY_DISTRICT_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_DISTRICT_NAME)) : Constants.EMPTY;
        String chiefdomName = Objects.nonNull(details.get(AdminConstants.KEY_CHIEFDOM_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_NAME)) : Constants.EMPTY;
        String villageName = Objects.nonNull(details.get(AdminConstants.KEY_VILLAGE_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_VILLAGE_NAME)) : Constants.EMPTY;
        row.createCell(0).setCellValue(countryName);
        row.createCell(1).setCellValue(districtName);
        row.createCell(2).setCellValue(chiefdomName);
        row.createCell(3).setCellValue(villageName);
    }

    /**
     * <p>
     * This method is populate the rows based on the type that isCommunity is false.
     * </p>
     *
     * @param row
     * @param details
     */
    private void populateCommunityRow(Row row, Map<String, Object> details) {
        String countryName = Objects.nonNull(details.get(AdminConstants.KEY_COUNTRY_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_COUNTRY_NAME)) : Constants.EMPTY;
        String districtName = Objects.nonNull(details.get(AdminConstants.KEY_DISTRICT_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_DISTRICT_NAME)) : Constants.EMPTY;
        String chiefdomName = Objects.nonNull(details.get(AdminConstants.KEY_CHIEFDOM_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_NAME)) : Constants.EMPTY;
        String villageName = Objects.nonNull(details.get(AdminConstants.KEY_VILLAGE_NAME))
                ? String.valueOf(details.get(AdminConstants.KEY_VILLAGE_NAME)) : Constants.EMPTY;
        String countryCode = Objects.nonNull(details.get(AdminConstants.KEY_COUNTRY_CODE))
                ? String.valueOf(details.get(AdminConstants.KEY_COUNTRY_CODE)) : Constants.EMPTY;
        String districtCode = Objects.nonNull(details.get(AdminConstants.KEY_DISTRICT_CODE))
                ? String.valueOf(details.get(AdminConstants.KEY_DISTRICT_CODE)) : Constants.EMPTY;
        String chiefdomCode = Objects.nonNull(details.get(AdminConstants.KEY_CHIEFDOM_CODE))
                ? String.valueOf(details.get(AdminConstants.KEY_CHIEFDOM_CODE)) : Constants.EMPTY;
        String villageCode = Objects.nonNull(details.get(AdminConstants.KEY_VILLAGE_CODE))
                ? String.valueOf(details.get(AdminConstants.KEY_VILLAGE_CODE)) : Constants.EMPTY;
        String villageType = Objects.nonNull(details.get(AdminConstants.KEY_VILLAGE_TYPE))
                ? String.valueOf(details.get(AdminConstants.KEY_VILLAGE_TYPE)) : Constants.EMPTY;
        row.createCell(0).setCellValue(countryName);
        row.createCell(1).setCellValue(countryCode);
        row.createCell(2).setCellValue(districtName);
        row.createCell(3).setCellValue(districtCode);
        row.createCell(4).setCellValue(chiefdomName);
        row.createCell(5).setCellValue(chiefdomCode);
        row.createCell(6).setCellValue(villageName);
        row.createCell(7).setCellValue(villageCode);
        row.createCell(8).setCellValue(villageType);
    }


    /**
     * <p>
     * This method used to get village details based on the villageId.
     * </p>
     *
     * @param id the villageId
     * @return Long village memberSequence details.
     */
    @Override
    public Long getMemberSequenceByVillageId(Long id) {
        return villageRepository.getMemberSequenceByVillageId(id);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getHouseholdSequenceByVillageId(Long id) {
        return villageRepository.getHouseholdSequenceByVillageId(id);
    }

    /**
     * {@inheritDoc}
     *
     */
    @Override
    public VillageDTO getVillageDetailsByVillageId(Long id) {
        ModelMapper mapper = new ModelMapper();
        return mapper.map(villageRepository.findByIdAndIsDeletedFalseAndIsActiveTrue( id), VillageDTO.class);
    }
}
