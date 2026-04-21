package com.strandls.observation.util;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * HMAC-based request signing for securing API endpoints
 * Must match the implementation in biodiv-ui request-signing.ts
 *
 * How it works:
 * 1. Frontend generates time-based rotating secret (changes every 15 min)
 * 2. Frontend signs request with HMAC(timestamp + params, secret)
 * 3. Backend verifies signature using same secret generation logic
 * 4. Signatures valid for 5 minutes (prevents replay attacks)
 */
public class HMACRequestSigning {

    private static final Logger logger = LoggerFactory.getLogger(HMACRequestSigning.class);

    private static final long SIGNATURE_VALIDITY_MS = 5 * 60 * 1000; // 5 minutes
    private static final long SECRET_ROTATION_MS = 15 * 60 * 1000; // 15 minutes

    /**
     * Base secret key loaded from config.properties
     * MUST match SITE_CONFIG.SECURITY.HMAC_SECRET_KEY in biodiv-ui
     */
    private static final String BASE_SECRET;

    static {
        String secret = PropertyFileUtil.fetchProperty("config.properties", "hmac_secret_key");
        if (secret == null || secret.trim().isEmpty()) {
            logger.error("CRITICAL: hmac_secret_key not found in config.properties! Using fallback.");
            secret = "biodiv-fallback-secret-configure-in-properties";
        }
        BASE_SECRET = secret;
        logger.info("HMAC signing initialized with secret from config.properties");
    }

    /**
     * Generate current signing secret (time-based rotation)
     * Secrets rotate every 15 minutes based on current time window
     */
    public static String generateSigningSecret() {
        long timestamp = System.currentTimeMillis();
        long rotationWindow = timestamp / SECRET_ROTATION_MS;

        return hmacSha256(String.valueOf(rotationWindow), BASE_SECRET);
    }

    /**
     * Generate previous secret (for grace period during rotation)
     * Allows requests signed with previous secret to be valid for smooth transitions
     */
    public static String generatePreviousSecret() {
        long timestamp = System.currentTimeMillis();
        long rotationWindow = (timestamp / SECRET_ROTATION_MS) - 1;

        return hmacSha256(String.valueOf(rotationWindow), BASE_SECRET);
    }

    /**
     * Create canonical string from parameters
     * MUST match the format in biodiv-ui request-signing.ts createCanonicalString()
     * Format: "timestamp|key1=value1&key2=value2&..."
     */
    private static String createCanonicalString(long timestamp, Map<String, String> params) {
        // Sort keys to ensure consistent ordering
        List<String> sortedKeys = new ArrayList<>(params.keySet());
        Collections.sort(sortedKeys);

        String paramString = sortedKeys.stream()
            .map(key -> key + "=" + jsonStringify(params.get(key)))
            .collect(Collectors.joining("&"));

        return timestamp + "|" + paramString;
    }

    /**
     * JSON stringify for consistency with JavaScript JSON.stringify()
     * Wraps strings in quotes and escapes special characters
     */
    private static String jsonStringify(String value) {
        if (value == null) {
            return "null";
        }
        // Escape quotes and wrap in quotes to match JavaScript behavior
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }

    /**
     * Verify HMAC signature from incoming request
     *
     * @param params Request parameters (query params)
     * @param timestamp Timestamp from X-Request-Timestamp header
     * @param signature Signature from X-Request-Signature header
     * @return VerificationResult with validation status and reason
     */
    public static VerificationResult verifySignature(
        Map<String, String> params,
        long timestamp,
        String signature
    ) {
        long now = System.currentTimeMillis();

        // Check timestamp is within valid window (prevents replay attacks)
        long timeDiff = Math.abs(now - timestamp);
        if (timeDiff > SIGNATURE_VALIDITY_MS) {
            return new VerificationResult(
                false,
                "Timestamp expired. Diff: " + (timeDiff / 1000) + "s, Max: " + (SIGNATURE_VALIDITY_MS / 1000) + "s"
            );
        }

        // Verify with current secret
        String currentSecret = generateSigningSecret();
        String canonicalString = createCanonicalString(timestamp, params);
        String expectedSignature = hmacSha256(canonicalString, currentSecret);

        if (signature.equals(expectedSignature)) {
            return new VerificationResult(true, null);
        }

        // Try previous secret (grace period during rotation)
        String previousSecret = generatePreviousSecret();
        String expectedPreviousSignature = hmacSha256(canonicalString, previousSecret);

        if (signature.equals(expectedPreviousSignature)) {
            return new VerificationResult(true, null);
        }

        return new VerificationResult(false, "Invalid signature");
    }

    /**
     * HMAC-SHA256 implementation
     * Generates hex-encoded HMAC signature
     */
    private static String hmacSha256(String data, String key) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            SecretKeySpec secretKeySpec = new SecretKeySpec(
                key.getBytes(StandardCharsets.UTF_8),
                "HmacSHA256"
            );
            mac.init(secretKeySpec);

            byte[] hmacBytes = mac.doFinal(data.getBytes(StandardCharsets.UTF_8));

            // Convert to hex string (lowercase to match JavaScript)
            StringBuilder hexString = new StringBuilder();
            for (byte b : hmacBytes) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }

            return hexString.toString();
        } catch (NoSuchAlgorithmException | InvalidKeyException e) {
            throw new RuntimeException("Failed to compute HMAC", e);
        }
    }

    /**
     * Result of signature verification
     */
    public static class VerificationResult {
        private final boolean valid;
        private final String reason;

        public VerificationResult(boolean valid, String reason) {
            this.valid = valid;
            this.reason = reason;
        }

        public boolean isValid() {
            return valid;
        }

        public String getReason() {
            return reason;
        }
    }
}
