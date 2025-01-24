package com.mdtlabs.coreplatform.commonservice.common;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.mdtlabs.coreplatform.commonservice.common.model.dto.RoleDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.TimezoneDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;
import com.mdtlabs.coreplatform.commonservice.common.model.entity.Timezone;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;

import com.mdtlabs.coreplatform.commonservice.common.contexts.UserContextHolder;
import com.mdtlabs.coreplatform.commonservice.common.exception.SpiceValidation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.commonservice.common.util.StringUtil;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * Common utils for date diff, data validation etc.
 * </p>
 * 
 * @author Karthick Murugesan created on Jan 11, 2024
 *
 */

@Component
@NoArgsConstructor(access= AccessLevel.PRIVATE)
public class CommonUtil {

    public static String jobUserName;

    /**
     * This setter is used to set job username
     */
    @Value("${app.jobUserName:}")
    private static void setJobUserName(String userName) {
        jobUserName = userName;
    }

    /**
     * <p>
     * Used to check null condition
     * </p>
     *
     * @param pObj object to be checked
     * @return boolean  true or false on null check
     */
    public static boolean isNull(Object... pObj) {
        Boolean isNull = false;
        if (pObj == null) {
            isNull = true;
        } else {
            for (Object lObj : pObj) {
                if (lObj == null) {
                    isNull = true;
                } else if (lObj instanceof String string) {
                    isNull = string.trim().equals(Constants.EMPTY);
                } else if (lObj instanceof Collection<?> collection) {
                    isNull = collection.isEmpty();
                } else if (lObj instanceof Map<?, ?> map) {
                    isNull = map.size() == Constants.ZERO;
                } else {
                    isNull = false;
                }
                if (Boolean.TRUE.equals(isNull)) {
                    break;
                }
            }
        }
        return isNull;
    }

    /**
     * <p>
     * Used to check not null condition
     * </p>
     *
     * @param pObj object to be checked
     * @return boolean  true or false on null check
     */
    public static boolean isNotNull(Object... pObj) {
        return !isNull(pObj);
    }

    /**
     * Converts a comma-separated string into a list of strings.
     *
     * @param commaSepratedValues The comma-separated string to be converted.
     * @return A list of strings obtained by splitting the input string at each comma.
     *         If the provided string is null, returns null.
     */
    public static List<String> getListOfStringValues(String commaSepratedValues) {
        List<String> listOfValues = null;
        if (!isNull(commaSepratedValues)) {
            listOfValues = Arrays.asList(commaSepratedValues.split(Constants.SEPARATOR));
        }
        return listOfValues;
    }

    /**
     * Rounds a given double value to a specified number of decimal places.
     *
     * @param value The double value to be rounded.
     * @param precision The number of decimal places to which the value should be rounded.
     * @return The rounded double value. If an exception occurs during rounding, the original value is returned.
     */
    public static double getRoundedValue(double value, int precision) {
        try {
            int scale = (int) Math.pow(10, precision);
            return (double) Math.round(value * scale) / scale;
        } catch (Exception ex) {
            return value;
        }
    }

    /**
     * Rounds a given double value to one decimal place.
     *
     * @param value The double value to be rounded.
     * @return The rounded double value. If an exception occurs during rounding, the original value is returned.
     */
    public static double getRoundedValue(double value) {
        return getRoundedValue(value, Constants.ONE);
    }

    /**
     * Retrieves the current timestamp in a specific format.
     *
     * @return A string representing the current timestamp in the format specified by Constants.CURRENT_TIMESTAMP_DATE_FORMAT.
     */
    public static String getCurrentTimeStamp() {
        return new SimpleDateFormat(Constants.CURRENT_TIMESTAMP_DATE_FORMAT).format(new Date());
    }

    /**
     * <p>
     * Converts a time string into a double representation.
     * The method replaces ":30" with ".5" and ":00" with ".0" in the input string, and then parses the result as a double.
     * This is useful for converting time strings of the format "HH:MM" into decimal hours.
     * </p>
     *
     * @param time The time string to be converted. Must be in the format "HH:MM".
     * @return A double representation of the time. If the input string is "HH:30", the returned value will be "HH.5".
     *         If the input string is "HH:00", the returned value will be "HH.0".
     *         If an exception occurs during parsing, a NumberFormatException will be thrown.
     */
    public static Double convertTimeStringToDouble(String time) {
        return Double.parseDouble(time.replace(":30", ".5").replace(":00", ".0"));
    }

    /**
     * <p>
     * Get the logged in employee from the spring {@code SecurityContextHolder} and
     * construct the string with format '[employeeId  employeeCompanyId]'.
     * </p>
     *
     * @return String  Logged in employee as string with format '[employeeId
     * employeeCompanyId]'.
     */
    public static String getLoggedInEmployeeLog() {
        UserContextDTO user = getLoggedInUser();
        String userId = String.valueOf(user.getId());
        String username = user.getUsername();
        
        return StringUtil.constructString(Constants.OPEN_BRACKET, userId, Constants.HYPHEN, username,
                Constants.CLOSE_BRACKET);
    }

    /**
     * <p>
     * Get the logged in employee from the spring
     * </p>
     *
     * @return UserDTO  Logged in employee object.
     */
    public static UserContextDTO getLoggedInUser() {
        return Objects.isNull(UserContextHolder.getUserDto()) ? new UserContextDTO() : UserContextHolder.getUserDto();
    }

    /**
     * Constructs an HttpEntity object with the current user's authorization token as a header.
     * The constructed HttpEntity can be used for making authenticated HTTP requests.
     *
     * @return An HttpEntity object with the current user's authorization token as a header.
     *         The header key is HttpHeaders.AUTHORIZATION.
     *         The header value is a concatenation of Constants.BEARER and the current user's authorization token.
     */
    public static HttpEntity<String> getCurrentEntity() {
        HttpHeaders headers = new HttpHeaders();
        headers.add(HttpHeaders.AUTHORIZATION,
                StringUtil.concatString(Constants.BEARER, UserContextHolder.getUserDto().getAuthorization()));
        return new HttpEntity<>(headers);
    }

    /**
     * Creates pagination for a given count of items and a specified grid display value.
     *
     * @param count The total count of items to be paginated.
     * @param gridDisplayValue The number of items to be displayed per page.
     * @return The total number of pages required to display all items. If there is a remainder when dividing the total count by the grid display value, an additional page is added.
     */
    public static int createPagination(int count, int gridDisplayValue) {
        int totalPageNumber = count / gridDisplayValue;
        int remainder = count % gridDisplayValue;
        return (remainder == Constants.ZERO) ? (totalPageNumber + Constants.ZERO) : (totalPageNumber + Constants.ONE);
    }

    /**
     * <p>
     * Validate the given string has only letters and numbers
     * </p>
     *
     * @param values listValues
     * @return Boolean
     */
    public static boolean validatePatientSearchData(List<String> values) {
        return values.stream()
                .allMatch(value -> (!Objects.isNull(value) && !value.isEmpty() && !Pattern.matches(Constants.PATIENT_SEARCH_REGEX, value)));
    }

    /**
     * Validates an email address against a predefined regular expression.
     *
     * @param email The email address to be validated.
     * @return true if the email address is valid according to the regular expression defined in Constants.EMAIL_REGEX.
     *         false otherwise.
     */
    public static boolean validateEmail(String email) {
        Pattern pattern = Pattern.compile(Constants.EMAIL_REGEX);
        Matcher matcher = pattern.matcher(email);
        return matcher.matches();
    }

    /**
     * Validates a phone number against a predefined regular expression.
     *
     * @param phoneNo The phone number to be validated.
     * @return true if the phone number is valid according to the regular expression defined in Constants.PHONE_NUMBER_REGEX.
     *         false otherwise.
     */
    public static boolean validatePhoneNumber(String phoneNo) {
        Pattern pattern = Pattern.compile(Constants.PHONE_NUMBER_REGEX);
        Matcher matcher = pattern.matcher(phoneNo);
        return matcher.matches();
    }

    /**
     * <p>
     * To get the user auth token
     * </p>
     *
     * @return String
     */
    public static String getAuthToken() {
        return Constants.BEARER + UserContextHolder.getUserDto().getAuthorization();
    }

    /**
     * <p>
     * To get the user auth cookie
     * </p>
     *
     * @return String
     */
    public static String getAuthCookie() {
        return UserContextHolder.getUserDto().getCookie();
    }

    /**
     * <p>
     * To get the user client
     * </p>
     *
     * @return String
     */
    public static String getClient() {
        return UserContextHolder.getUserDto().getClient();
    }

    /**
     * <p>
     * To get the user tenantId
     * </p>
     *
     * @return tenant id of the user
     */
    public static Long getTenantId() {
        return UserContextHolder.getUserDto().getTenantId();
    }

    /**
     * <p>
     * Validate the given string with the given regex
     * </p>
     *
     * @param searchTerm  searchTerm
     * @param searchRegex regEx
     * @return Boolean
     */
    public static boolean isValidSearchData(String searchTerm, String searchRegex) {
        boolean isMatches = true;
                if(StringUtils.isNotBlank(searchTerm)) {
                        Pattern pattern = Pattern.compile(searchRegex);
            Matcher matcher = pattern.matcher(searchTerm);
            isMatches = !matcher.matches();
        }
        return isMatches;
    }

    /**
     * <p>
     * Checks whether the input string contains only alphabetic characters.
     *  </p>
     *
     * @param searchText The string to be checked.
     * @return true if the string contains only alphabetic characters.
     *         false if the string is empty or contains non-alphabetic characters.
     */
    public static boolean isAllAlphabetic(String searchText) {
        if (searchText.isEmpty()) {
            return false;
        }
        return searchText.chars().allMatch(Character::isLetter);
    }

    /**
     * <p>
     * Checks whether the input string contains only numeric characters.
     * </p>
     *
     * @param searchText The string to be checked.
     * @return true if the string contains only numeric characters.
     *         false if the string is empty or contains non-numeric characters.
     */
    public static boolean isAllNumeric(String searchText) {
        if (searchText.isEmpty()) {
            return false;
        }
        return searchText.chars().allMatch(Character::isDigit);
    }

    public static boolean isCultureCodeNull() {
        return !Objects.isNull(UserContextHolder.getUserDto().getCulture()) 
                && !Objects.isNull(UserContextHolder.getUserDto().getCulture().getCode());
    }

    /**
     * <p>
     * Converts the multipart file to file.
     *  </p>
     *
     * @param file The multipart file to be converted.
     * @return Converted file is returned
     */
    public static File convertMultipartFileToFile(MultipartFile file) throws IOException {
        File convertedFile = null;
        if (Objects.nonNull(file) && Objects.nonNull(file.getOriginalFilename())) {
            convertedFile = new File(file.getOriginalFilename());
            try (FileOutputStream fos = new FileOutputStream(convertedFile)) {
                fos.write(file.getBytes());
            } catch (IOException e) {
                Logger.logError(e);
                throw new SpiceValidation(1513, e.getMessage());
            }
        }
        return convertedFile;
    }

    /**
     * This method is used for checking the appTypes
     *
     * @param appTypes The list of app types to check (can be null).
     * @return Returns true if the list is not null, contains exactly one element,
     *         and that element is equal to COMMUNITY. Otherwise, returns false.
     */
    public static boolean isCommunityApp(List<String> appTypes) {
        return appTypes != null && appTypes.size() == Constants.ONE && Constants.APP_TYPE_COMMUNITY.equals(appTypes.getFirst());
    }

    /**
     * This method maps a Timezone entity to a TimezoneDTO.
     *
     * @param timezone The Timezone entity to be converted (can be null).
     * @return Returns a TimezoneDTO if the input entity is not null.
     *         If the input is null, returns null.
     */
    public static TimezoneDTO getTimezone(Timezone timezone) {
        ModelMapper modelMapper = new ModelMapper();
        if (Objects.isNull(timezone)) {
            return null;
        }
        return modelMapper.map(timezone, TimezoneDTO.class);
    }

}
