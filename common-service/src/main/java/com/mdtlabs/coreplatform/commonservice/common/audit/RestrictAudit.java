package com.mdtlabs.coreplatform.commonservice.common.audit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to restrict audit for entity.
 *
 * @author Tamilarasi Shanmugasundaram
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface RestrictAudit {
}
