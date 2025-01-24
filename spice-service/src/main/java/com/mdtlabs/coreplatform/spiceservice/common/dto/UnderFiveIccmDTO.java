package com.mdtlabs.coreplatform.spiceservice.common.dto;

import java.util.List;

import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.AnaemiaDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.BreastfeedingProblemDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.CoughDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.DiarrhoeaDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.EarProblemDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.FeverDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.GeneralDangerSigns;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.HIVInfectionDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.HivRdtTestDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.JaundiceDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.NonBreastfeedingProblemDTO;
import com.mdtlabs.coreplatform.spiceservice.common.dto.examinationdto.VerySevereDiseaseDTO;

import lombok.Data;

/**
 * This is a Request DTO class for under five years ICCM.
 *
 * @author Karthick M created on Mar 15, 2024
 */
@Data
public class UnderFiveIccmDTO {

    private String id;

    private ClinicalSummaryAndSignsDTO clinicalSummaryAndSigns;

    private Examination examination;

    private String clinicalNotes;

    private String presentingComplaints;

    private List<String> systemicExamination;

    private String systemicExaminationNotes;

    private String patientId;

    private String assessmentName;

    private EncounterDetailsDTO encounter;

    @Data
    public static class Examination {

        private VerySevereDiseaseDTO verySevereDisease;

        private JaundiceDTO jaundice;

        private DiarrhoeaDTO diarrhoea;

        private HIVInfectionDTO hivInfection;

        private BreastfeedingProblemDTO breastfeedingProblem;

        private NonBreastfeedingProblemDTO nonBreastfeedingProblem;

        private GeneralDangerSigns generalDangerSigns;

        private CoughDTO cough;

        private FeverDTO fever;

        private EarProblemDTO earProblem;

        private AnaemiaDTO anaemia;

        private HivRdtTestDTO hivRDT;

    }
}
