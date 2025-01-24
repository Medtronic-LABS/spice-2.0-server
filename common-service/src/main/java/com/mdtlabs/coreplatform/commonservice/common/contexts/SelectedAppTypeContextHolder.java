package com.mdtlabs.coreplatform.commonservice.common.contexts;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * <p>
 * This class is used to hold the context of the current app type in a ThreadLocal variable.
 * ThreadLocal is used to create variables that can only be read and written by the same thread.
 * Thus, even if two threads are executing the same code, and the code has a reference to a ThreadLocal variable,
 * then the two threads cannot see each other's ThreadLocal variables.
 * </p>
 *
 * <p>
 * The class provides static methods to set, get, and clear the current app type context.
 * The context is stored as a String object representing appType.
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
public class SelectedAppTypeContextHolder {

    private static final ThreadLocal<String> APP_TYPE_CONTEXT = new ThreadLocal<>();

    /**
     * <p>
     * This static method is used to set the selectedAppType object in the current thread's context.
     * </p>
     *
     * @param appType object that needs to be set in the current thread's context
     */
    public static void set(String appType) {
        APP_TYPE_CONTEXT.set(appType);
    }

    /**
     * <p>
     * This static method is used to get the selectedAppType object from the current thread's context.
     * The appType object contains the details of the current user.
     * </p>
     *
     * @return appType object that is set in the current thread's context. If no appType object is
     * set, it returns null.
     */
    public static String get() {
        return APP_TYPE_CONTEXT.get();
    }

    /**
     * <p>
     * This static method is used to clear the selectedAppType object from the current thread's context.
     * After this method is called, the appType object will no longer be available in the current thread's
     * context.
     * </p>
     */
    public static void clear() {
        APP_TYPE_CONTEXT.remove();
    }
}




