package com.mdtlabs.coreplatform.cqlservice.constants;

/**
 * Utility class containing constants for database table names.
 * <p>
 * This class provides a centralized repository of constants used to refer to database table names
 * within the application. It prevents the hard-coding of table names across the application, facilitating
 * easier maintenance and readability.
 * </p>
 * <p>
 * The {@code private} constructor is used to prevent instantiation of this utility class.
 * </p>
 */
public class TableConstants {

    /**
     * <p>
     * Private constructor to prevent instantiation of this utility class.
     * </p>
     */
    private TableConstants() {
    }

    public static final String TABLE_CQL_RESULT = "cql_result";
}
