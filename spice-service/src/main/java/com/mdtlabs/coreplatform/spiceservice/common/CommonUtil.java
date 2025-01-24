package com.mdtlabs.coreplatform.spiceservice.common;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * <p>
 * Common utils for date diff, data validation etc.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Aug 08, 2024
 *
 */
@NoArgsConstructor(access= AccessLevel.PRIVATE)
public class CommonUtil {

    private static CommonLists commonLists = null;

    /**
     * Create a singleton class for CommonLists
     * @return instance of CommonLists class
     */
    public static CommonLists getCommonListsInstance() {
        if (commonLists == null) {
            commonLists = new CommonLists();
        }
        return commonLists;
    }
}
