package com.mdtlabs.coreplatform.authservice.helper;

import com.mdtlabs.coreplatform.authservice.ErrorConstants;
import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.Validation;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.crypto.RSADecrypter;
import com.nimbusds.jwt.EncryptedJWT;
import jakarta.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import org.springframework.util.FileCopyUtils;

import java.security.KeyFactory;
import java.security.interfaces.RSAPrivateKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.text.ParseException;

@Component
public class HelperService {

    private RSAPrivateKey privateRsaKey = null;

    @Value("${app.private-key}")
    private String privateKey;

    public EncryptedJWT getJWT(String jwtToken) {
        if (null == privateRsaKey) {
            tokenDecrypt();
        }
        EncryptedJWT jwt;
        try {
            jwt = EncryptedJWT.parse(jwtToken);
        } catch (ParseException e) {
            Logger.logError(e);
            throw new Validation(20002);
        }
        RSADecrypter decrypter = new RSADecrypter(privateRsaKey);
        try {
            jwt.decrypt(decrypter);
        } catch (JOSEException e) {
            Logger.logError(e);
            throw new Validation(20002);
        }
        return jwt;
    }

    @PostConstruct
    private void tokenDecrypt() {
        try {
            Resource resource = new ClassPathResource(privateKey);
            byte[] bdata = FileCopyUtils.copyToByteArray(resource.getInputStream());
            PKCS8EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(bdata);
            KeyFactory kf = KeyFactory.getInstance(Constants.RSA);
            this.privateRsaKey = (RSAPrivateKey) kf.generatePrivate(privateKeySpec);
        } catch (Exception exception) {
            Logger.logError(ErrorConstants.EXCEPTION_DURING_TOKEN_UTIL, exception);
        }
    }
}
