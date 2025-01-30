package com.mdtlabs.coreplatform.spiceservice.symptom.service.impl;

import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mdtlabs.coreplatform.spiceservice.common.Constants;
import com.mdtlabs.coreplatform.spiceservice.common.model.Symptom;
import com.mdtlabs.coreplatform.spiceservice.staticdata.repository.SymptomRepository;
import com.mdtlabs.coreplatform.spiceservice.symptom.service.SymptomService;

@Service
public class SymptomServiceImpl implements SymptomService {
    private final SymptomRepository symptomRepository;

    private static Constants constantInstance = null;

    @Autowired
    public SymptomServiceImpl(SymptomRepository symptomRepository) {
        this.symptomRepository = symptomRepository;
    }

    /**
     * {@inheritDoc}
     */
    public List<Symptom> getSymptoms() {
        if (getConstantsInstance().getSymptoms().isEmpty()) {
            getConstantsInstance().setSymptoms(symptomRepository.findByIsDeletedFalseAndIsActiveTrue());
        }
        return getConstantsInstance().getSymptoms();
    }

    /**
     * {@inheritDoc}
     */
    public List<Symptom> getSymptoms(Set<Long> ids) {
        if (getConstantsInstance().getSymptoms().isEmpty()) {
            getConstantsInstance().setSymptoms(symptomRepository.findByIsDeletedFalseAndIsActiveTrue());
        }
        return getConstantsInstance().getSymptoms().stream().filter(obj -> ids.contains(obj.getId())).toList();
    }

    /**
     * Retrieves the singleton instance of the Constants class.
     * This method implements the Singleton design pattern to ensure that only one instance
     * of Constants exists throughout the application.
     *
     * @return the single instance of the Constants class.
     *         If the instance does not exist, it will be created on the first call to this method.
     */
    public static Constants getConstantsInstance() {
        // To ensure only one instance is created
        if (constantInstance == null) {
            constantInstance = new Constants();
        }
        return constantInstance;
    }

}
