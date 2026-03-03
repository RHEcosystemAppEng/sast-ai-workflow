/*
 * Sample C file for integration testing
 * Contains various function patterns
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/utils.h"

/* Global buffer for testing */
char global_buffer[256];

/**
 * Sanitize user input by removing dangerous characters
 * @param input - user provided string
 * @return sanitized string or NULL on error
 */
char* sanitize_input(const char* input) {
    if (input == NULL) {
        return NULL;
    }

    size_t len = strlen(input);
    char* result = malloc(len + 1);
    if (result == NULL) {
        return NULL;
    }

    // Remove dangerous characters
    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        if (input[i] != ';' && input[i] != '&' && input[i] != '|') {
            result[j++] = input[i];
        }
    }
    result[j] = '\0';

    return result;
}

/**
 * Validate email address format
 */
int validate_email(const char* email) {
    if (email == NULL) {
        return 0;
    }

    // Simple validation - must contain @ and .
    const char* at_sign = strchr(email, '@');
    const char* dot = strrchr(email, '.');

    return (at_sign != NULL && dot != NULL && at_sign < dot);
}

/**
 * Process user data with validation
 */
int process_user_data(const char* data) {
    if (data == NULL) {
        fprintf(stderr, "Error: NULL data provided\n");
        return -1;
    }

    char* sanitized = sanitize_input(data);
    if (sanitized == NULL) {
        fprintf(stderr, "Error: Failed to sanitize input\n");
        return -1;
    }

    printf("Processed data: %s\n", sanitized);
    free(sanitized);

    return 0;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input>\n", argv[0]);
        return 1;
    }

    return process_user_data(argv[1]);
}
