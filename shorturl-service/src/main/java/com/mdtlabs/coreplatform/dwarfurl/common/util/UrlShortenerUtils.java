package com.mdtlabs.coreplatform.dwarfurl.common.util;

import com.mdtlabs.coreplatform.dwarfurl.common.Constants;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 * The UrlShortenerUtils class is a utility class for URL shortening related operations.
 * </p>
 */
@Component
public class UrlShortenerUtils {

    /**
     * Generate expiration time based on the given limit.
     * If the expire limit is null, it defaults to a predefined constant value.
     *
     * @param expireLimit the expiration limit in minutes
     * @return the calculated expiration date
     */
    public static Date generateExpireTime(Long expireLimit) {
        if (!Objects.isNull(expireLimit)) {
            // Convert the Long value (which represents minutes) to milliseconds
            long millisecondsToAdd = TimeUnit.MINUTES.toMillis(expireLimit);
            // Get the current date and add the milliseconds to it
            return new Date(System.currentTimeMillis() + millisecondsToAdd);
        } else {
            long millisecondsToAdd = TimeUnit.MINUTES.toMillis(Constants.DAY_TO_MINUTES);
            return new Date(System.currentTimeMillis() + millisecondsToAdd);
        }
    }

    /**
     * Generates an array of ASCII values representing the characters '0-9', 'A-Z', and 'a-z'.
     *
     * @return a char array containing the ASCII values for '0-9', 'A-Z', and 'a-z'
     */
    public static char[] generateAsciiValues() {
        char[] asciiValue = new char[Constants.SIXTY_TWO];
        //The below logics is used to set the ascii values '0-9' 'A-Z' 'a-z' in the char[]
        for (int i = Constants.ZERO; i < Constants.SIXTY_TWO; i++) {
            int j;
            if (i < Constants.TEN) {
                j = i + Constants.FORTY_EIGHT;
            } else if (i > Constants.NINE && i <= Constants.THIRTY_FIVE) {
                j = i + Constants.FIFTY_FIVE;
            } else {
                j = i + Constants.SIXTY_ONE;
            }
            asciiValue[i] = (char) j;
        }
        return asciiValue;
    }
}
