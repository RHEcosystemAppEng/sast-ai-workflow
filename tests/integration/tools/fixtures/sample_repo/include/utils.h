#ifndef UTILS_H
#define UTILS_H

/* Sanitize user input */
char* sanitize_input(const char* input);

/* Validate email format */
int validate_email(const char* email);

/* Process user data */
int process_user_data(const char* data);

#endif /* UTILS_H */
