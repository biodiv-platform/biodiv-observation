package com.strandls.observation.filter;

import com.strandls.observation.util.HMACRequestSigning;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

/**
 * Servlet filter to verify HMAC signatures on incoming requests
 *
 * This filter protects observation list endpoints from unauthorized access
 * by verifying HMAC signatures on all requests.
 *
 * Flow:
 * 1. Extract X-Request-Timestamp and X-Request-Signature headers
 * 2. Build parameter map from query string
 * 3. Verify signature matches expected value
 * 4. Allow request if valid, block if invalid
 */
public class HMACSignatureFilter implements Filter {

    private static final Logger logger = LoggerFactory.getLogger(HMACSignatureFilter.class);

    /**
     * Patterns for endpoints that require HMAC verification
     * Add more patterns here to protect additional endpoints
     */
    private static final String[] PROTECTED_ENDPOINTS = {
        "/v1/observation/list/"
    };

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        logger.info("=================================================");
        logger.info("HMAC Signature Filter initialized");
        logger.info("Protected endpoints: {}", String.join(", ", PROTECTED_ENDPOINTS));
        logger.info("Signature validity: 5 minutes");
        logger.info("Secret rotation: 15 minutes");
        logger.info("=================================================");
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;

        String requestURI = httpRequest.getRequestURI();
        String method = httpRequest.getMethod();

        // Check if this endpoint requires HMAC verification
        boolean requiresVerification = false;
        for (String pattern : PROTECTED_ENDPOINTS) {
            if (requestURI.contains(pattern)) {
                requiresVerification = true;
                break;
            }
        }

        if (!requiresVerification) {
            // Not a protected endpoint, proceed without verification
            chain.doFilter(request, response);
            return;
        }

        // Handle OPTIONS preflight requests (CORS)
        if ("OPTIONS".equalsIgnoreCase(method)) {
            httpResponse.setStatus(HttpServletResponse.SC_OK);
            return;
        }

        // Get signature headers
        String timestamp = httpRequest.getHeader("X-Request-Timestamp");
        String signature = httpRequest.getHeader("X-Request-Signature");

        String clientIP = getClientIP(httpRequest);
        String userAgent = httpRequest.getHeader("User-Agent");

        if (timestamp == null || signature == null) {
            logger.warn("[NO SIGNATURE] IP: {} | UA: {} | URI: {}",
                clientIP, userAgent, requestURI);
            sendErrorResponse(httpResponse, HttpServletResponse.SC_UNAUTHORIZED,
                "Missing authentication headers", "X-Request-Timestamp and X-Request-Signature headers are required");
            return;
        }

        // Build parameter map from query string
        Map<String, String> params = buildParamMap(httpRequest);

        // ===== TESTING: Print HMAC secret key for verification =====
        String currentSecret = HMACRequestSigning.getBaseSecret();
        logger.info("[HMAC SECRET TEST] IP: {} | Secret Key: {} | Length: {} | URI: {}",
            clientIP, currentSecret, currentSecret.length(), requestURI);
        // ============================================================

        // Log request details for debugging (remove in production)
        if (logger.isDebugEnabled()) {
            logger.debug("Request verification - IP: {}, Timestamp: {}, Params: {}",
                clientIP, timestamp, params);
        }

        // Verify signature
        try {
            long timestampLong = Long.parseLong(timestamp);
            HMACRequestSigning.VerificationResult result =
                HMACRequestSigning.verifySignature(params, timestampLong, signature);

            if (!result.isValid()) {
                logger.warn("[INVALID SIGNATURE] IP: {} | UA: {} | Reason: {} | URI: {}",
                    clientIP, userAgent, result.getReason(), requestURI);
                sendErrorResponse(httpResponse, HttpServletResponse.SC_FORBIDDEN,
                    "Invalid request signature", result.getReason());
                return;
            }

            // Signature valid, proceed with request
            logger.info("[OK] IP: {} | offset: {} | max: {} | URI: {}",
                clientIP,
                params.getOrDefault("offset", "0"),
                params.getOrDefault("max", "10"),
                requestURI);

            chain.doFilter(request, response);

        } catch (NumberFormatException e) {
            logger.warn("[INVALID TIMESTAMP] IP: {} | UA: {} | Timestamp: {} | URI: {}",
                clientIP, userAgent, timestamp, requestURI);
            sendErrorResponse(httpResponse, HttpServletResponse.SC_BAD_REQUEST,
                "Invalid timestamp format", "Timestamp must be a valid number");
        }
    }

    @Override
    public void destroy() {
        logger.info("HMAC Signature Filter destroyed");
    }

    /**
     * Build parameter map from request query parameters
     * Includes all query parameters for signature verification
     */
    private Map<String, String> buildParamMap(HttpServletRequest request) {
        Map<String, String> params = new HashMap<>();

        // Add all query parameters
        Enumeration<String> parameterNames = request.getParameterNames();
        while (parameterNames.hasMoreElements()) {
            String paramName = parameterNames.nextElement();
            String paramValue = request.getParameter(paramName);
            if (paramValue != null) {
                params.put(paramName, paramValue);
            }
        }

        return params;
    }

    /**
     * Get client IP address, checking X-Forwarded-For header first
     */
    private String getClientIP(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("X-Real-IP");
        }
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        // If multiple IPs in X-Forwarded-For, take the first one
        if (ip != null && ip.contains(",")) {
            ip = ip.split(",")[0].trim();
        }
        return ip != null ? ip : "unknown";
    }

    /**
     * Send error response as JSON
     */
    private void sendErrorResponse(HttpServletResponse response, int statusCode,
                                   String error, String reason) throws IOException {
        response.setStatus(statusCode);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");

        String jsonResponse = String.format(
            "{\"error\":\"%s\",\"reason\":\"%s\"}",
            escapeJson(error),
            escapeJson(reason)
        );

        response.getWriter().write(jsonResponse);
    }

    /**
     * Escape JSON special characters
     */
    private String escapeJson(String str) {
        if (str == null) {
            return "";
        }
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }
}
