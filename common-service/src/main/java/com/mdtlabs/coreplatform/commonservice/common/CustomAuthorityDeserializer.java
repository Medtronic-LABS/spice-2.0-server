package com.mdtlabs.coreplatform.commonservice.common;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * <p>
 * Deserialize the granted authority values in user service.
 * </p>
 *
 * @author Karthick Murugesan Created on Jan 11, 2024
 */
public class CustomAuthorityDeserializer extends JsonDeserializer<Object> {

	/**
	 * <p>
	 * Deserializes the JSON data from the user service into a list of GrantedAuthority objects.
	 * </p>
	 *
	 * @param jsonParser The JsonParser that provides access to the JSON data.
	 * @param deserializationContext The DeserializationContext that provides contextual information during deserialization.
	 * @return A list of GrantedAuthority objects that represent the authorities granted to a user.
	 * @throws IOException If an input or output exception occurred.
	 */
	@Override
	public Object deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
		ObjectMapper mapper = (ObjectMapper) jsonParser.getCodec();
		JsonNode jsonNode = mapper.readTree(jsonParser);
		List<GrantedAuthority> grantedAuthorities = new LinkedList<>();

		Iterator<JsonNode> elements = jsonNode.elements();
		while (elements.hasNext()) {
			JsonNode next = elements.next();
			JsonNode authority = next.get(Constants.AUTHORITY);
			grantedAuthorities.add(new SimpleGrantedAuthority(authority.asText()));
		}
		return grantedAuthorities;
	}

}
