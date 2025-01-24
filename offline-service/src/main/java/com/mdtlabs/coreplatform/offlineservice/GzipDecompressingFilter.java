package com.mdtlabs.coreplatform.offlineservice;


import com.mdtlabs.coreplatform.commonservice.common.exception.ServicesException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.offlineservice.common.Constants;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletRequestWrapper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

/**
 * This class is used to decompress the GZIP request.
 *
 * @author Praveen Created on 04 Jun 2024
 */
public class GzipDecompressingFilter implements Filter {

    /**
     * This method is used to decompress the GZIP request.
     *
     * @param request  - ServletRequest
     * @param response - ServletResponse
     * @param chain    - FilterChain
     * @throws IOException      - IOException
     * @throws ServletException - ServletException
     */
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        if (Constants.GZIP.equalsIgnoreCase(httpRequest.getHeader(Constants.CONTENT_ENCODING_HEADER))) {
            try {
                HttpServletRequest wrappedRequest = new GzipHttpServletRequestWrapper(httpRequest);
                chain.doFilter(wrappedRequest, response);
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                throw new ServicesException(1005);
            }
        } else {
            chain.doFilter(request, response);
        }
    }

    /**
     * This class is used to wrap the GZIP request.
     */
    private static class GzipHttpServletRequestWrapper extends HttpServletRequestWrapper {
        private final byte[] body;

        /**
         * Constructor to wrap the GZIP request.
         *
         * @param request - HttpServletRequest
         * @throws IOException - IOException
         */
        public GzipHttpServletRequestWrapper(HttpServletRequest request) throws IOException {
            super(request);
            body = decompressGzip(request.getInputStream());
        }

        /**
         * This method is used to get the ServletInputStream.
         *
         * @return ServletInputStream
         */
        @Override
        public ServletInputStream getInputStream() {
            return new ServletInputStreamWrapper(body);
        }

        /**
         * This method is used to decompress the GZIP request.
         *
         * @param inputStream - InputStream
         * @return byte[] - decompressed byte array
         * @throws IOException - IOException
         */
        private byte[] decompressGzip(InputStream inputStream) throws IOException {
            try (GZIPInputStream gzipInputStream = new GZIPInputStream(inputStream);
                 ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
                byte[] buffer = new byte[Constants.BUFFER_SIZE];
                int len;
                while ((len = gzipInputStream.read(buffer)) > Constants.ZERO) {
                    byteArrayOutputStream.write(buffer, Constants.ZERO, len);
                }
                return byteArrayOutputStream.toByteArray();
            }
        }
    }

    /**
     * This class is used to wrap the ServletInputStream.
     */
    private static class ServletInputStreamWrapper extends ServletInputStream {
        private final InputStream sourceStream;

        /**
         * Constructor to wrap the ServletInputStream.
         *
         * @param body - byte array
         */
        public ServletInputStreamWrapper(byte[] body) {
            this.sourceStream = new ByteArrayInputStream(body);
        }

        /**
         * This method is used to check whether the stream is finished or not.
         *
         * @return boolean - true if stream is finished, false otherwise
         */
        @Override
        public boolean isFinished() {
            try {
                return sourceStream.available() == Constants.ZERO;
            } catch (IOException e) {
                return true;
            }
        }

        /**
         * This method is used to check whether the stream is ready or not.
         *
         * @return boolean - true if stream is ready, false otherwise
         */
        @Override
        public boolean isReady() {
            return true;
        }

        /**
         * This method is used to set the ReadListener.
         *
         * @param listener - ReadListener
         */
        @Override
        public void setReadListener(ReadListener listener) {
            throw new UnsupportedOperationException();
        }

        /**
         * This method is used to read the stream.
         *
         * @return int - read byte
         * @throws IOException - IOException
         */
        @Override
        public int read() throws IOException {
            return sourceStream.read();
        }

        /**
         * This method is used to read the stream.
         *
         * @param b - byte array
         * @return int - read byte
         * @throws IOException - IOException
         */
        @Override
        public int read(byte[] b, int off, int len) throws IOException {
            return sourceStream.read(b, off, len);
        }
    }
}