package com.mdtlabs.coreplatform.commonservice.common;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.jackson.JsonComponent;

import java.io.IOException;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 * <p>
 * This is an implementation for converting date fields and its format respective to the user's timezone.
 * </p>
 *
 * @author Karthick Murugesan Created on Jan 11, 2024
 */
@JsonComponent
public class CustomDateSerializer extends StdSerializer<Date> {

    @Setter
    public static String userZoneId;

    public CustomDateSerializer() {
        this(null);
    }

    public CustomDateSerializer(Class t) {
        super(t);
    }

    /**
     * <p>
     * Serializes a Date object into a string representation, taking into account the user's timezone.
     * </p>
     *
     * @param date The Date object to be serialized.
     * @param gen The JsonGenerator that is used to output the serialized data.
     * @param arg2 The SerializerProvider that can be used to find serializers for the types that this serializer instance does not handle.
     * @throws IOException If an input or output exception occurred.
     */
    @Override
    public void serialize(Date date, JsonGenerator gen, SerializerProvider arg2)
            throws IOException {
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_TIMEZONE);
        Instant timeStamp = date.toInstant();
        String zoneId = String.valueOf(ZoneId.of(FieldConstants.UTC));
        if (StringUtils.isNotEmpty(CustomDateSerializer.userZoneId) && !CustomDateSerializer.userZoneId.equals("+00:00")) {
            zoneId = CustomDateSerializer.userZoneId;
        }
        ZonedDateTime zonedDateTime = timeStamp.atZone(ZoneId.of(zoneId));
        OffsetDateTime offsetDateTime = zonedDateTime.toOffsetDateTime();
        gen.writeString(offsetDateTime.format(dateTimeFormatter));
    }

}