/*
 * Authentication utilities
 */
#include <string.h>
#include <stdlib.h>
#include "../include/auth.h"

#define MAX_USERNAME_LEN 64
#define MAX_PASSWORD_LEN 128

/**
 * Hash password using simple algorithm (DO NOT USE IN PRODUCTION)
 */
unsigned int hash_password(const char* password) {
    unsigned int hash = 0;
    while (*password) {
        hash = hash * 31 + *password++;
    }
    return hash;
}

/**
 * Authenticate user credentials
 * Returns 1 if authenticated, 0 otherwise
 */
int authenticate_user(const char* username, const char* password) {
    if (username == NULL || password == NULL) {
        return 0;
    }

    if (strlen(username) > MAX_USERNAME_LEN ||
        strlen(password) > MAX_PASSWORD_LEN) {
        return 0;
    }

    // Simulate authentication check
    unsigned int pass_hash = hash_password(password);

    // This is just a demo - real code would check database
    return (pass_hash != 0);
}

/**
 * Create user session token
 */
char* create_session_token(const char* username) {
    if (username == NULL) {
        return NULL;
    }

    size_t token_len = strlen(username) + 32;
    char* token = malloc(token_len);
    if (token == NULL) {
        return NULL;
    }

    // Generate simple token (not cryptographically secure!)
    snprintf(token, token_len, "TOKEN_%s_%u", username, hash_password(username));

    return token;
}
