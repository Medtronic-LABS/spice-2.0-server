package com.mdtlabs.coreplatform.commonservice.common.contexts;

import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * <p>
 * This class is used to hold the context of the current app types in a ThreadLocal variable.
 * ThreadLocal is used to create variables that can only be read and written by the same thread.
 * Thus, even if two threads are executing the same code, and the code has a reference to a ThreadLocal variable,
 * then the two threads cannot see each other's ThreadLocal variables.
 * </p>
 *
 * <p>
 * The class provides static methods to set, get, and clear the current app types context.
 * The context is stored as a String object representing appTypes.
 * </p>
 *
 * <p>
 * This class is particularly useful in situations where you have objects that are not thread-safe,
 * but you wish to use them in a thread-safe manner without synchronizing access to the objects.
 * </p>
 *
 * <p>
 * Note: This class has a private constructor to prevent instantiation. It's a utility class and should be used in a
 * static context.
 * </p>
 *
 * @author Vishwaeaswaran created on Oct 1, 2024
 */

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AppTypesContextHolder {

    private static final ThreadLocal<List<String>> APP_TYPES_CONTEXT = new ThreadLocal<>();

    /**
     * <p>
     * This static method is used to set the appTypes object in the current thread's context.
     * </p>
     *
     * @param appTypes object that needs to be set in the current thread's context
     */
    public static void set(List<String> appTypes) {
        APP_TYPES_CONTEXT.set(appTypes);
    }

    /**
     * <p>
     * This static method is used to get the appTypes object from the current thread's context.
     * The appTypes object contains the details of the current user.
     * </p>
     *
     * @return appTypes object that is set in the current thread's context. If no appTypes object is
     * set, it returns null.
     */
    public static List<String> get() {
        return APP_TYPES_CONTEXT.get();
    }

    /**
     * <p>
     * This static method is used to clear the appTypes object from the current thread's context.
     * After this method is called, the appTypes object will no longer be available in the current thread's
     * context.
     * </p>
     */
    public static void clear() {
        APP_TYPES_CONTEXT.remove();
    }
}




