package com.mdtlabs.coreplatform.commonservice.common.contexts;


import com.mdtlabs.coreplatform.commonservice.common.model.dto.UserContextDTO;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * <p>
 * This class is used to hold the context of the current user in a ThreadLocal variable.
 * ThreadLocal is used to create variables that can only be read and written by the same thread.
 * Thus, even if two threads are executing the same code, and the code has a reference to a ThreadLocal variable,
 * then the two threads cannot see each other's ThreadLocal variables.
 * </p>
 *
 * <p>
 * The class provides static methods to set, get, and clear the current user context.
 * The context is stored as a UserDTO object.
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
 * @author Prabu created on Feb 16, 2023
 */

@NoArgsConstructor(access= AccessLevel.PRIVATE)
public class UserContextHolder {

	private static final ThreadLocal<UserContextDTO> USER_CONTEXT = new ThreadLocal<>();

	/**
	 * <p>
	 * This static method is used to set the UserDTO object in the current thread's context.
	 * The UserDTO object contains the details of the current user.
	 * </p>
	 *
	 * @param userDto the UserDTO object that needs to be set in the current thread's context
	 */
	public static void setUserDto(UserContextDTO userDto) {
		USER_CONTEXT.set(userDto);
	}

	/**
	 * <p>
	 * This static method is used to get the UserDTO object from the current thread's context.
	 * The UserDTO object contains the details of the current user.
	 * </p>
	 *
	 * @return the UserDTO object that is set in the current thread's context. If no UserDTO object is set, it returns null.
	 */
	public static UserContextDTO getUserDto() {
		return USER_CONTEXT.get();
	}

	/**
	 * <p>
	 * This static method is used to clear the UserDTO object from the current thread's context.
	 * After this method is called, the UserDTO object will no longer be available in the current thread's context.
	 * </p>
	 */
	public static void clear() {
		USER_CONTEXT.remove();
	}
}
