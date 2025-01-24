package com.mdtlabs.coreplatform.commonservice.common.domain;

import java.util.List;

/**
 * <p>
 * Paged Interface
 * </p>
 *
 * @author Jeyaharini Ananthakrishnan created on Feb 08, 2024
 */
public interface Paged<T> {

    /**
     * Get list of generic object T.
     *
     * @return List - list of entity
     */
    List<T> getList();

    /**
     * Total count
     *
     * @return - count as a long type
     */
    long getCount();

    /**
     * gets Generic object T
     *
     * @return Object T - entity object
     */
    T getObject();

}
