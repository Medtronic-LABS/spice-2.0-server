package mentalhealth.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatusCode;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;

import com.mdtlabs.coreplatform.spiceservice.common.TestDataProvider;
import com.mdtlabs.coreplatform.spiceservice.common.dto.AssessmentDTO;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.controller.MentalHealthController;
import com.mdtlabs.coreplatform.spiceservice.mentalhealth.service.MentalHealthService;
import com.mdtlabs.coreplatform.spiceservice.message.SuccessResponse;

/**
 * <p>
 *  MentalHealthControllerTest class used to test all possible positive
 *  and negative cases for all methods and conditions used in MentalHealthController class.
 * </p>
 *
 * @author Jaganathan created on Jan 30, 2023
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MentalHealthControllerTest {

    @InjectMocks
    private MentalHealthController mentalHealthController;

    @Mock
    private MentalHealthService mentalHealthService;

    @Test
    @DisplayName("MentalHealthCreate Test")
    void createMentalHealth() {
        AssessmentDTO request = new AssessmentDTO();
        request.setPhq4(TestDataProvider.getMentalHealthRequest());

        doNothing().when(mentalHealthService).createMentalHealth(request);

        SuccessResponse<String> response = mentalHealthController.createMentalHealth(request);
        verify(mentalHealthService,atLeastOnce()).createMentalHealth(request);
        Assertions.assertEquals(response.getStatusCode(), HttpStatusCode.valueOf(201));
    }

}
