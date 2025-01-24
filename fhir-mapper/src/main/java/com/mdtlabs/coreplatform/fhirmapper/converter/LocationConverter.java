package com.mdtlabs.coreplatform.fhirmapper.converter;

import org.hl7.fhir.r4.model.Location;
import org.springframework.stereotype.Component;

import java.util.Objects;

/**
 * <p>
 * Converts to FHIR Location based on provided location details
 * </p>
 *
 * @author Gokul
 * @version 1.0
 * @since 2024-08-12
 */
@Component
public class LocationConverter {

    /**
     * Converts to FHIR Location entity based on given location details
     *
     * @param landmark   The location landmark
     * @param longitude  The location longitude
     * @param latitude   The location latitude
     *
     * @return Converted FHIR Location entity.
     */
    public Location createLocation(String landmark, String longitude,
                                    String latitude) {
        Location location = new Location();
        location.setStatus(Location.LocationStatus.ACTIVE);
        location.setName(landmark);
        Location.LocationPositionComponent position = new Location.LocationPositionComponent();

        if (Objects.nonNull(longitude)) {
            position.setLongitude(Double.valueOf(longitude));
        }
        if (Objects.nonNull(latitude)) {
            position.setLatitude(Double.valueOf(latitude));
        }
        location.setPosition(position);

        return location;
    }
}
