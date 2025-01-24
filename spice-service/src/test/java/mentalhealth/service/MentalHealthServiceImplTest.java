package mentalhealth.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.mdtlabs.coreplatform.commonservice.common.CommonUtil;
import com.mdtlabs.coreplatform.spiceservice.apiinterface.FhirServiceApiInterface;
import com.mdtlabs.coreplatform.spiceservice.common.TestConstants;
import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.impl.MentalHealthServiceImpl;

/**
 * <p>
 * MentalHealthServiceImplTest class used to test all possible positive
 * and negative cases for all methods and conditions used in
 * MentalHealthServiceImpl class.
 * </p>
 *
 * @author Nandhakumar Karthikeyan created on Feb 9, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MentalHealthServiceImplTest {

    @InjectMocks
    private MentalHealthServiceImpl mentalHealthService;

    @Mock
    private FhirServiceApiInterface fhirServiceApiInterface;

    @Test
    void createMentalHealth() {
        AssessmentDTO request = new AssessmentDTO();
        request.setPhq4(TestDataProvider.getMentalHealthRequest());
        ResponseEntity<String> responseEntity = ResponseEntity.ok(TestConstants.SUCCESS_MESSAGE);
        TestDataProvider.init();

        when(fhirServiceApiInterface.createMentalHealth(CommonUtil.getAuthToken(), CommonUtil.getClient(),request)).thenReturn(responseEntity);

        mentalHealthService.createMentalHealth(request);
        verify(fhirServiceApiInterface,atLeastOnce()).createMentalHealth(CommonUtil.getAuthToken(), CommonUtil.getClient(),request);
        TestDataProvider.cleanUp();
    }
}
