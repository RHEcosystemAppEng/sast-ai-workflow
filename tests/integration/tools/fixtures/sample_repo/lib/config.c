/*
 * Configuration management
 * Contains edge cases for testing
 */
#include <stdio.h>

#define CONFIG_MAX_SIZE 1024

// Function with assignment (should not match in search)
void (*callback_ptr)(void) = NULL;

// Nested function example
int load_config(const char* filename) {
    FILE* fp = fopen(filename, "r");
    if (fp == NULL) {
        return -1;
    }

    char buffer[CONFIG_MAX_SIZE];
    while (fgets(buffer, sizeof(buffer), fp) != NULL) {
        // Process config line
    }

    fclose(fp);
    return 0;
}

// Function with malloc/calloc for testing search
void* allocate_buffer(size_t size) {
    if (size == 0) {
        return NULL;
    }

    void* buffer = malloc(size);
    if (buffer == NULL) {
        return NULL;
    }

    memset(buffer, 0, size);
    return buffer;
}

// Function with realloc
void* resize_buffer(void* old_buffer, size_t old_size, size_t new_size) {
    if (new_size == 0) {
        free(old_buffer);
        return NULL;
    }

    void* new_buffer = realloc(old_buffer, new_size);
    return new_buffer;
}
