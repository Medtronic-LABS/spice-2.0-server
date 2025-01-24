package com.mdtlabs.coreplatform.commonservice.common;

import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import com.fasterxml.jackson.core.JsonParser;
import org.mockito.quality.Strictness;
import org.springframework.security.core.GrantedAuthority;

import java.io.IOException;
import java.util.List;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class CustomAuthorityDeserializerTest {

    @InjectMocks
    CustomAuthorityDeserializer customAuthorityDeserializer;

    @Test
    void deserialize() throws IOException {
        //given
        DeserializationContext deserializationContext = Mockito.mock(DeserializationContext.class);
        ObjectMapper mapper = new ObjectMapper();
        JsonParser jsonParser = mapper.getFactory().createParser(TestConstants.JSON);
        //then
        Object result = customAuthorityDeserializer.deserialize(jsonParser, deserializationContext);
        List<GrantedAuthority> grantedAuthorities = (List<GrantedAuthority>)result;
        Assertions.assertNotNull(grantedAuthorities);
        Assertions.assertEquals(Constants.ONE, grantedAuthorities.size());
        jsonParser.close();
    }
}