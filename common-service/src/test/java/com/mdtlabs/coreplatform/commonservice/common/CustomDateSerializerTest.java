package com.mdtlabs.coreplatform.commonservice.common;

import java.io.IOException;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CustomDateSerializerTest {

    @InjectMocks
    private CustomDateSerializer customDateSerializer;

    @Test
    void setUserZoneId() {
        //then
        CustomDateSerializer.setUserZoneId("+05:00");
        assertNotNull(CustomDateSerializer.userZoneId);
    }

    @Test
    void serialize() throws IOException {
        //given
        JsonGenerator jsonGenerator = mock(JsonGenerator.class);
        SerializerProvider serializerProvider = mock(SerializerProvider.class);

        //then
        customDateSerializer.serialize(new Date(), jsonGenerator, serializerProvider);
        assertNotNull(jsonGenerator);
    }

    @Test
    void setCustomDateSerializer() {
        //then
        CustomDateSerializer response = new CustomDateSerializer();
        assertNotNull(response);
    }
}