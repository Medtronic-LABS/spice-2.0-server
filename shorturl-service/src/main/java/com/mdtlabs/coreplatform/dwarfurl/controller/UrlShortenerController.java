package com.mdtlabs.coreplatform.dwarfurl.controller;

import com.mdtlabs.coreplatform.dwarfurl.common.Constants;
import com.mdtlabs.coreplatform.dwarfurl.common.Logger.Logger;
import com.mdtlabs.coreplatform.dwarfurl.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.dwarfurl.common.dto.ResponseDTO;
import com.mdtlabs.coreplatform.dwarfurl.common.exception.UrlShortenerException;
import com.mdtlabs.coreplatform.dwarfurl.service.UrlShortenerService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.view.RedirectView;

/**
 * <p>
 * The UrlShortenerController class is a REST controller that provides endpoints to shorten URLs and
 * redirect shortened URLs to their original destinations.
 * </p>
 *
 * @author JohnKennedy created on 04 sep 2024
 */
@RestController
@RequestMapping("/")
public class UrlShortenerController {

    private final UrlShortenerService urlShortenerService;

    @Autowired
    public UrlShortenerController(UrlShortenerService urlShortenerService) {
        this.urlShortenerService = urlShortenerService;
    }

    /**
     * <p>
     * Shorten the given URL.
     * This method is used to shorten the long URL to dwarf URL.
     * </p>
     *
     * @param requestDTO The requestDTO contains the long URL and expire limit.
     * @return A ResponseEntity which contains response object and HTTP status.
     */
    @PostMapping
    public ResponseEntity<Object> shortenURL(@RequestBody RequestDTO requestDTO) {
        try {
            ResponseDTO response = urlShortenerService.shortenURL(requestDTO);
            return new ResponseEntity<>(response, HttpStatus.OK);
        } catch (UrlShortenerException e) {
            Logger.logError(Constants.URL_CREATE_ERROR_MESSAGE);
            Logger.logError(e.getErrorMessage(), e);
            throw new ResponseStatusException(e.getStatus(), e.getErrorMessage());
        } catch (Exception e) {
            Logger.logError(Constants.URL_CREATE_ERROR_MESSAGE);
            Logger.logError(e.getMessage(), e);
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
        }
    }

    /**
     * <p>
     * Redirect the given URL
     * This method is used to redirect the given short URL to its original URL.
     * </p>
     *
     * @param shortURL The shortURL contains the short URL.
     * @return A RedirectView which redirect the URL to another URL.
     */
    @GetMapping("/{token}")
    public RedirectView redirectUrl(@PathVariable("token") String shortURL) {
        try {
            return urlShortenerService.redirectUrl(shortURL);
        } catch (UrlShortenerException e) {
            Logger.logError(Constants.URL_VALIDATE_ERROR_MESSAGE);
            Logger.logError(e.getErrorMessage(), e);
            throw new ResponseStatusException(e.getStatus(), e.getErrorMessage());
        } catch (Exception e) {
            Logger.logError(Constants.URL_VALIDATE_ERROR_MESSAGE);
            Logger.logError(e.getMessage(), e);
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
        }
    }
}
