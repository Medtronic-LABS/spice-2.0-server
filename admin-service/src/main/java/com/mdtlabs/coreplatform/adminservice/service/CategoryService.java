package com.mdtlabs.coreplatform.adminservice.service;

import com.mdtlabs.coreplatform.adminservice.model.entity.Category;

import java.util.List;

/**
 * <p>
 * This an interface class for category module you can implemented this class in any
 * class.
 * </p>
 *
 * @author Gokul created on Oct 17, 2024
 */
public interface CategoryService {
    
    /**
     * Retrieves category list for medication
     *
     */    
    public List<Category> getCategories();
}
