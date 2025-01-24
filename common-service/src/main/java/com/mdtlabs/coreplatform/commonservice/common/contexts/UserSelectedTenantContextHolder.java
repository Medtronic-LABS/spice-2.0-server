package com.mdtlabs.coreplatform.commonservice.common.contexts;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * <p>
 * This class is used to hold the context of the current user's selected tenant in a ThreadLocal variable.
 * ThreadLocal is used to create variables that can only be read and written by the same thread.
 * Thus, even if two threads are executing the same code, and the code has a reference to a ThreadLocal variable,
 * then the two threads cannot see each other's ThreadLocal variables.
 * </p>
 *
 * <p>
 * The class provides static methods to set, get, and clear the current user's selected tenant context.
 * The context is stored as a Long object representing the tenant ID.
 * </p>
 *
 * <p>
 * This class is particularly useful in situations where you have objects that are not thread-safe,
 * but you wish to use them in a thread-safe manner without synchronizing access to the objects.
 * </p>
 *
 * <p>
 * Note: This class has a private constructor to prevent instantiation. It's a utility class and should be used in a static context.
 * </p>
 * 
 * @author Prabu created on Feb 16, 2022
 */

@NoArgsConstructor(access= AccessLevel.PRIVATE)
public class UserSelectedTenantContextHolder {

	private static final ThreadLocal<Long> USER_TENANT_CONTEXT = new ThreadLocal<>();

	/**
	 * <p>
	 * This static method is used to set the tenant ID in the current thread's context.
	 * </p>
	 *
	 * @param tenantId the tenant ID that needs to be set in the current thread's context
	 */
	public static void set(Long tenantId) {
		USER_TENANT_CONTEXT.set(tenantId);
	}

	/**
	 * <p>
	 * This static method is used to get the tenant ID from the current thread's context.
	 * </p>
	 *
	 * @return the tenant ID that is set in the current thread's context. If no tenant ID is set, it returns null.
	 */
	public static Long get() {
		return USER_TENANT_CONTEXT.get();
	}

	/**
	 * <p>
	 * This static method is used to clear the tenant ID from the current thread's context.
	 * After this method is called, the tenant ID will no longer be available in the current thread's context.
	 * </p>
	 */
	public static void clear() {
		USER_TENANT_CONTEXT.remove();
	}
}
