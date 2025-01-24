package com.mdtlabs.coreplatform.dwarfurl.service.impl;

import com.mdtlabs.coreplatform.dwarfurl.common.Constants;
import com.mdtlabs.coreplatform.dwarfurl.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.dwarfurl.common.dto.ResponseDTO;

import com.mdtlabs.coreplatform.dwarfurl.common.exception.UrlShortenerException;
import com.mdtlabs.coreplatform.dwarfurl.common.model.UrlShortener;
import com.mdtlabs.coreplatform.dwarfurl.common.util.UrlShortenerUtils;
import com.mdtlabs.coreplatform.dwarfurl.repository.UrlShortenerRepository;
import com.mdtlabs.coreplatform.dwarfurl.service.UrlShortenerService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.view.RedirectView;

import java.util.Date;
import java.util.Objects;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>
 * The UrlShortenerServiceImpl class is a service implementation for URL shortening.
 * </p>
 *
 * @author JohnKennedy Created on 04 sep 2024
 */
@Service
public class UrlShortenerServiceImpl implements UrlShortenerService {

    // Compiled pattern for the URL regex
    private static final Pattern URL_PATTERN = Pattern.compile(Constants.URL_REGEX, Pattern.CASE_INSENSITIVE);
    private final String domain;
    private final UrlShortenerRepository urlShortenerRepository;
    private final Random random = new Random();

    /**
     * Constructor for UrlShortenerServiceImpl.
     * Initializes the character array used for generating short keys.
     *
     * @param urlShortenerRepository the repository for URL shortener entities
     */
    @Autowired
    public UrlShortenerServiceImpl(@Value("${app.domain}") String domain, UrlShortenerRepository urlShortenerRepository) {
        this.domain = domain.endsWith("/") ? domain : domain.concat("/");
        this.urlShortenerRepository = urlShortenerRepository;
    }

    /**
     * {@inheritDoc}
     * Shortens a given URL.
     *
     * @param requestDTO the request data transfer object containing the URL to be shortened
     * @return ResponseDTO containing the shortened URL
     */
    public ResponseDTO shortenURL(RequestDTO requestDTO) {
        validateUrl(requestDTO.getUrl());
        requestDTO.setToken(generateKey());
        UrlShortener urlShortener = constructUrlShortenerData(requestDTO);
        urlShortenerRepository.save(urlShortener);
        return new ResponseDTO(domain.concat(requestDTO.getToken()));
    }

    /**
     * {@inheritDoc}
     * Redirects to the original URL based on the short key.
     *
     * @param shortKey the short key representing the original URL
     * @return RedirectView the redirect view to the original URL
     */
    public RedirectView redirectUrl(String shortKey) {
        UrlShortener urlShortener = urlShortenerRepository.findByToken(shortKey);
        if (Objects.isNull(urlShortener)) {
            throw new UrlShortenerException(Constants.URL_NOT_FOUND, HttpStatus.NOT_FOUND);
        }
        if (!urlShortener.getExpiresAt().after(new Date())) {
            urlShortener.setActive(Boolean.FALSE);
            urlShortenerRepository.save(urlShortener);
            throw new UrlShortenerException(Constants.URL_EXPIRED, HttpStatus.FORBIDDEN);
        }
        urlShortener.setVisits(urlShortener.getVisits() + Constants.ONE);
        urlShortener.setUpdatedAt(new Date());
        urlShortenerRepository.save(urlShortener);
        RedirectView redirectView = new RedirectView();
        redirectView.setUrl(urlShortener.getUrl());
        return redirectView;
    }

    /**
     * Generates a random short key and verifies its uniqueness.
     *
     * @return String the generated short key
     */
    private String generateKey() {
        StringBuilder key = new StringBuilder();
        UrlShortener urlShortener;
        char[] asciiValue = UrlShortenerUtils.generateAsciiValues();
        boolean flag = Boolean.TRUE;
        while (flag) {
            key.setLength(Constants.ZERO);
            for (int i = Constants.ZERO; i < Constants.NINE; i++) {
                key.append(asciiValue[random.nextInt(Constants.SIXTY_TWO)]);
            }
            urlShortener = urlShortenerRepository.findByTokenAndIsActive(key.toString(), true);
            if (Objects.isNull(urlShortener)) {
                flag = Boolean.FALSE;
            }
        }
        return key.toString();
    }

    /**
     * Validates the given URL against a predefined pattern.
     *
     * @param url the URL to be validated
     * @throws IllegalArgumentException if the URL is invalid
     */
    private void validateUrl(String url) {
        Matcher matcher = URL_PATTERN.matcher(url);
        if (!matcher.matches()) {
            throw new UrlShortenerException(Constants.INVALID_URL, HttpStatus.NOT_ACCEPTABLE);
        }
    }

    /**
     * Construct URL shortener data
     *
     * @param request - request
     * @return UrlShortener
     */
    private UrlShortener constructUrlShortenerData(RequestDTO request) {
        UrlShortener urlShortener = new UrlShortener();
        urlShortener.setUrl(request.getUrl());
        urlShortener.setToken(request.getToken());
        urlShortener.setApp(request.getApp());
        urlShortener.setEnv(request.getEnv());
        urlShortener.setVisits(Constants.ZERO);
        urlShortener.setActive(Boolean.TRUE);
        urlShortener.setCreatedAt(new Date());
        urlShortener.setUpdatedAt(new Date());
        urlShortener.setExpiresAt(UrlShortenerUtils.generateExpireTime(request.getExpiresAt()));
        return urlShortener;
    }
}
