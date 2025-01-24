package com.mdtlabs.coreplatform.spiceservice.staticdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.mdtlabs.coreplatform.spiceservice.common.model.FormMetaUi;

/**
 * This interface is responsible for performing database operations between
 * server and DosageFrequency entity.
 *
 * @author TTamilarasi Shanmugasundaram created on Aug 8, 2022
 *
 */
@Repository
public interface FormMetaUiRepository extends JpaRepository<FormMetaUi, Long> {

    List<FormMetaUi> findByIsDeletedFalseAndIsActiveTrue();

}
