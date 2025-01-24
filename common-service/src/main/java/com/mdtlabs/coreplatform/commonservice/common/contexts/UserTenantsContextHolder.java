package com.mdtlabs.coreplatform.commonservice.common.contexts;

import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * <p>
 * This class is used to hold the context of the current user's tenant IDs in a ThreadLocal variable.
 * ThreadLocal is used to create variables that can only be read and written by the same thread.
 * Thus, even if two threads are executing the same code, and the code has a reference to a ThreadLocal variable,
 * then the two threads cannot see each other's ThreadLocal variables.
 * </p>
 *
 * <p>
 * The class provides static methods to set, get, and clear the current user's tenant IDs context.
 * The context is stored as a List of Long objects representing the tenant IDs.
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
public class UserTenantsContextHolder {

	private static final ThreadLocal<List<Long>> USER_TENANTS_CONTEXT = new ThreadLocal<>();

	/**
	 * <p>
	 * This static method is used to set the tenant IDs in the current thread's context.
	 * </p>
	 *
	 * @param tenantIds the tenant IDs that need to be set in the current thread's context
	 */
	public static void set(List<Long> tenantIds) {
		USER_TENANTS_CONTEXT.set(tenantIds);
	}

	/**
	 * <p>
	 * This static method is used to get the tenant IDs from the current thread's context.
	 * </p>
	 *
	 * @return the tenant IDs that are set in the current thread's context. If no tenant IDs are set, it returns null.
	 */
	public static List<Long> get() {
		return USER_TENANTS_CONTEXT.get();
	}

	/**
	 * <p>
	 * This static method is used to clear the tenant IDs from the current thread's context.
	 * After this method is called, the tenant IDs will no longer be available in the current thread's context.
	 * </p>
	 */
	public static void clear() {
		USER_TENANTS_CONTEXT.remove();
	}
}
