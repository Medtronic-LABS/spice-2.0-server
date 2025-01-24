package com.mdtlabs.coreplatform.spiceservice.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mdtlabs.coreplatform.spiceservice.common.model.FormMetaUi;
import com.mdtlabs.coreplatform.spiceservice.common.model.Message;
import lombok.Data;

/**
 * <p>
 * To define the common list values used all over the application.
 * </p>
 *
 * @author Tamilarasi Shanmugasundaram created on Aug 08, 2024
 */
@Data
public class CommonLists {
    private List<FormMetaUi> formMetaUis = new ArrayList<>();
}
