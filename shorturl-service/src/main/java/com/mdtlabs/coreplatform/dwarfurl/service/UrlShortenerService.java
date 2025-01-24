package com.mdtlabs.coreplatform.dwarfurl.service;

import com.mdtlabs.coreplatform.dwarfurl.common.dto.RequestDTO;
import com.mdtlabs.coreplatform.dwarfurl.common.dto.ResponseDTO;
import org.springframework.web.servlet.view.RedirectView;

/**
 * <p>
 * The UrlShortenerService interface provides the contract for URL shortening and redirection services.
 * </p>
 *
 * @author JohnKennedy Created on 04 sep 2024
 */
public interface UrlShortenerService {

    /**
     * <p>
     * This method is used to shorten a long URL to a dwarf URL.
     * </p>
     *
     * @param requestDTO The requestDTO contains the long URL and expire limit.
     * @return A ResponseDTO which contains short URL.
     */
    ResponseDTO shortenURL(RequestDTO requestDTO);

    /**
     * <p>
     * This method is used to redirect the given short URL to its original URL.
     * </p>
     *
     * @param shortURL The shortURL contains the short URL.
     * @return A RedirectView which redirect the URL to another URL.
     */
    RedirectView redirectUrl(String shortURL);
}