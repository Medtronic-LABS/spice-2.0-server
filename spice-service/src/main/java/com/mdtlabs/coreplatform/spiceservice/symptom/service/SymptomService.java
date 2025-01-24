package com.mdtlabs.coreplatform.spiceservice.symptom.service;

import java.util.List;
import java.util.Set;

import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;

public interface SymptomService {

    /**
     * <p>
     * This method returns list of all symptoms.
     * </p>
     *
     * @return List of Symptom Entity
     */
    public List<Symptom> getSymptoms();

    /**
     * <p>
     * This method returns list of all symptoms for given symptom ids.
     * </p>
     *
     * @return List of Symptom Entity
     */
    public List<Symptom> getSymptoms(Set<Long> ids);

}
