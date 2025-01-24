package com.mdtlabs.coreplatform.dwarfurl.common.util;

import com.mdtlabs.coreplatform.dwarfurl.common.Constants;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * <p>
 * The JwtTokenUtil class is used to generate JWT tokens.
 * </p>
 */
@Component
public class JwtTokenUtil {

    /**
     * Generates a JWT token.
     *
     * @return the generated JWT token as a String
     */
    public static String generateToken() {
        return Jwts.builder()
                .setIssuedAt(new Date())
                .signWith(SignatureAlgorithm.HS256, Constants.SECRET_KEY)
                .compact();
    }

    public static Claims verifyToken(String token) {
        return Jwts.parser()
                .setSigningKey(Constants.SECRET_KEY)
                .parseClaimsJws(token)
                .getBody();
    }
}
