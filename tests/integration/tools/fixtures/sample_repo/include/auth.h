#ifndef AUTH_H
#define AUTH_H

/* Hash password */
unsigned int hash_password(const char* password);

/* Authenticate user */
int authenticate_user(const char* username, const char* password);

/* Create session token */
char* create_session_token(const char* username);

#endif /* AUTH_H */
