package com.mdtlabs.coreplatform.commonservice.common.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to mark methods that configure the application type.
 * It is intended to be used in conjunction with other annotations that provide
 * specific configuration details for different application types.
 *
 * @author Vishwaeaswaran created on Oct 01, 2024
 */
@Target(value = ElementType.METHOD)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface ConfigureAppType {

}
