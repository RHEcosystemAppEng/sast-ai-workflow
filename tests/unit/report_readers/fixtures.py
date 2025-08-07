"""
Shared test fixtures for report reader tests.
"""

# Sample SARIF data for integration testing
SAMPLE_SARIF = {
    "version": "2.1.0",
    "runs": [
        {
            "tool": {
                "driver": {
                    "name": "TestTool",
                    "version": "1.0.0",
                    "rules": [
                        {
                            "id": "RESOURCE_LEAK: leaked_storage",
                            "name": "Resource Leak Rule",
                            "shortDescription": {"text": "Detects resource leaks"},
                            "properties": {"cwe": "CWE-772"},
                        },
                        {
                            "id": "SNYK_CODE_WARNING: note[c/SQLInjection/test]",
                            "name": "SQL Injection Rule",
                            "shortDescription": {"text": "Detects SQL injection vulnerabilities"},
                            "properties": {"cwe": "CWE-89"},
                        },
                        {
                            "id": "SNYK_CODE_WARNING: note[c/UninitVar/test]",
                            "name": "Uninitialized Variable Rule",
                            "shortDescription": {"text": "Detects use of uninitialized variables"},
                            "properties": {"cwe": "CWE-457"},
                        },
                    ],
                }
            },
            "results": [
                {
                    "ruleId": "RESOURCE_LEAK: leaked_storage",
                    "properties": {"cwe": "CWE-772"},
                    "level": "error",
                    "locations": [
                        {
                            "id": 26,
                            "physicalLocation": {
                                "artifactLocation": {"uri": "src/file.c"},
                                "region": {
                                    "startLine": 946,
                                    "endLine": 946,
                                    "startColumn": 21,
                                    "endColumn": 21,
                                    "snippet": {
                                        "text": (
                                            "Problem detected in this context:\n"
                                            "  944|                       g_set_error(err, VAR, LRE_CBINTERRUPTED,\n"
                                            '  945|                               "Interrupted by LR_CB_ERROR from end callback");\n'
                                            "  946|->                     return FALSE;\n"
                                            "  947|                   }\n"
                                            "  948|               }"
                                        )
                                    },
                                },
                            },
                        }
                    ],
                    "message": {
                        "text": (
                            'Variable "full_url" going out of scope leaks the storage it points to.'
                        )
                    },
                    "codeFlows": [
                        {
                            "threadFlows": [
                                {
                                    "locations": [
                                        {
                                            "location": {
                                                "id": 0,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 850,
                                                        "endLine": 850,
                                                        "startColumn": 5,
                                                        "endColumn": 5,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "dd", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 1,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 851,
                                                        "endLine": 851,
                                                        "startColumn": 5,
                                                        "endColumn": 5,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "selected_target", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 2,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 852,
                                                        "endLine": 852,
                                                        "startColumn": 5,
                                                        "endColumn": 5,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "selected_full_url", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 3,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 853,
                                                        "endLine": 853,
                                                        "startColumn": 5,
                                                        "endColumn": 5,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "!err", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 4,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 858,
                                                        "endLine": 858,
                                                        "startColumn": 5,
                                                        "endColumn": 5,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "elem", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 5,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 864,
                                                        "endLine": 864,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "target->state != LR_DS_WAITING", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 6,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 865,
                                                        "endLine": 865,
                                                        "startColumn": 13,
                                                        "endColumn": 13,
                                                    },
                                                },
                                                "message": {"text": "Continuing loop."},
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 7,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 858,
                                                        "endLine": 858,
                                                        "startColumn": 44,
                                                        "endColumn": 44,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "elem", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 8,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 858,
                                                        "endLine": 858,
                                                        "startColumn": 5,
                                                        "endColumn": 5,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "elem", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 9,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 864,
                                                        "endLine": 864,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "target->state != LR_DS_WAITING", taking false branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 10,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 869,
                                                        "endLine": 869,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "strstr(target->target->path, "://")", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 11,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 873,
                                                        "endLine": 873,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "!target->target->baseurl", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 12,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 873,
                                                        "endLine": 873,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "!target->lrmirrors", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 13,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 873,
                                                        "endLine": 873,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "!complete_url_in_path", taking false branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 14,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 889,
                                                        "endLine": 889,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Condition "complete_url_in_path", taking true branch.'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 15,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 891,
                                                        "endLine": 891,
                                                        "startColumn": 13,
                                                        "endColumn": 13,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Storage is returned from allocation function "g_strdup_inline".'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 1,
                                            "kinds": ["alloc_fn"],
                                        },
                                        {
                                            "location": {
                                                "id": 16,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 891,
                                                        "endLine": 891,
                                                        "startColumn": 13,
                                                        "endColumn": 13,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Assigning: "full_url" = storage returned from '
                                                        '"g_strdup_inline(target->target->path)".'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 1,
                                            "kinds": ["var_assign"],
                                        },
                                        {
                                            "location": {
                                                "id": 17,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 892,
                                                        "endLine": 892,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": "Falling through to end of if statement."
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 18,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 919,
                                                        "endLine": 919,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "full_url", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 19,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 919,
                                                        "endLine": 919,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "target->handle", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 20,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 919,
                                                        "endLine": 919,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "target->handle->offline", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 21,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 919,
                                                        "endLine": 919,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Resource "full_url" is not freed or pointed-to in "lr_is_local_path".'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 1,
                                            "kinds": ["noescape"],
                                        },
                                        {
                                            "location": {
                                                "id": 22,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 919,
                                                        "endLine": 919,
                                                        "startColumn": 9,
                                                        "endColumn": 9,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "!lr_is_local_path(full_url)", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 23,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 924,
                                                        "endLine": 924,
                                                        "startColumn": 13,
                                                        "endColumn": 13,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        'Assuming resource "full_url" is not freed or pointed-to as ellipsis '
                                                        'argument to "g_debug".'
                                                    )
                                                },
                                            },
                                            "nestingLevel": 1,
                                            "kinds": ["noescape"],
                                        },
                                        {
                                            "location": {
                                                "id": 24,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 935,
                                                        "endLine": 935,
                                                        "startColumn": 13,
                                                        "endColumn": 13,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "end_cb", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 25,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 940,
                                                        "endLine": 940,
                                                        "startColumn": 17,
                                                        "endColumn": 17,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Condition "ret == LR_CB_ERROR", taking true branch.'
                                                },
                                            },
                                            "nestingLevel": 2,
                                            "kinds": ["path"],
                                        },
                                        {
                                            "location": {
                                                "id": 26,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/file.c"},
                                                    "region": {
                                                        "startLine": 946,
                                                        "endLine": 946,
                                                        "startColumn": 21,
                                                        "endColumn": 21,
                                                    },
                                                },
                                                "message": {
                                                    "text": 'Variable "full_url" going out of scope leaks the storage it points to.'
                                                },
                                            },
                                            "nestingLevel": 0,
                                            "kinds": ["leaked_storage"],
                                        },
                                    ]
                                }
                            ]
                        }
                    ],
                    "relatedLocations": [
                        {
                            "id": 27,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {
                                "text": (
                                    "  944|                       g_set_error(err, VAR, LRE_CBINTERRUPTED,"
                                )
                            },
                        },
                        {
                            "id": 28,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {
                                "text": (
                                    '  945|                               "Interrupted by LR_CB_ERROR from end callback");'
                                )
                            },
                        },
                        {
                            "id": 29,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {"text": "  946|->                     return FALSE;"},
                        },
                        {
                            "id": 30,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {"text": "  947|                   }"},
                        },
                        {
                            "id": 31,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {"text": "  948|               }"},
                        },
                    ],
                    "fingerprints": {
                        "csdiff/v0": "d0d3a379949e85ae62f7eb1a9c9beb79b29817fe",
                        "csdiff/v1": "0ff22612d347e57c28f87ae6ea8119b326f4e267",
                    },
                },
                {
                    "ruleId": "SNYK_CODE_WARNING: note[c/SQLInjection/test]",
                    "properties": {"cwe": "CWE-89"},
                    "level": "error",
                    "locations": [
                        {
                            "id": 0,
                            "physicalLocation": {
                                "artifactLocation": {"uri": "src/login.c"},
                                "region": {
                                    "startLine": 87,
                                    "endLine": 87,
                                    "startColumn": 12,
                                    "endColumn": 30,
                                    "snippet": {
                                        "text": (
                                            "SQL injection vulnerability:\n"
                                            "   85|   sprintf(query, \"SELECT * FROM users WHERE username='%s'\", username);\n"
                                            "   86|   // Direct user input concatenation\n"
                                            "   87|-> result = execute_query(query);\n"
                                            "   88|   return result;\n"
                                            "   89| }"
                                        )
                                    },
                                },
                            },
                        }
                    ],
                    "message": {
                        "text": (
                            "SQL injection vulnerability in login form where user input is not properly "
                            "sanitized before being used in database query."
                        )
                    },
                    "codeFlows": [
                        {
                            "threadFlows": [
                                {
                                    "locations": [
                                        {
                                            "location": {
                                                "id": 0,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/login.c"},
                                                    "region": {
                                                        "startLine": 87,
                                                        "endLine": 87,
                                                        "startColumn": 12,
                                                        "endColumn": 30,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        "SQL injection vulnerability in login form where user input is not "
                                                        "properly sanitized before being used in database query."
                                                    )
                                                },
                                            },
                                            "nestingLevel": 0,
                                            "kinds": ["error[c/SQLInjection/test]"],
                                        }
                                    ]
                                }
                            ]
                        }
                    ],
                    "relatedLocations": [
                        {
                            "id": 1,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {
                                "text": (
                                    "   85|   sprintf(query, \"SELECT * FROM users WHERE username='%s'\", username);"
                                )
                            },
                        },
                        {
                            "id": 2,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {"text": "   87|-> result = execute_query(query);"},
                        },
                    ],
                    "fingerprints": {
                        "csdiff/v0": "def456789abc012345678901234567890abcdef1",
                        "csdiff/v1": "789abc012def345678901234567890abcdef1234",
                    },
                },
                {
                    "ruleId": "SNYK_CODE_WARNING: note[c/UninitVar/test]",
                    "properties": {"cwe": "CWE-457"},
                    "level": "warning",
                    "locations": [
                        {
                            "id": 0,
                            "physicalLocation": {
                                "artifactLocation": {"uri": "src/helper.c"},
                                "region": {
                                    "startLine": 18,
                                    "endLine": 18,
                                    "startColumn": 12,
                                    "endColumn": 18,
                                    "snippet": {
                                        "text": (
                                            "Use of uninitialized variable:\n"
                                            "   16|   int result;\n"
                                            "   17|   // Missing initialization\n"
                                            "   18|-> return result;\n"
                                            "   19|   \n"
                                            "   20| }"
                                        )
                                    },
                                },
                            },
                        }
                    ],
                    "message": {
                        "text": (
                            "Use of uninitialized variable 'result'. Variable used before initialization in "
                            "function processData()."
                        )
                    },
                    "codeFlows": [
                        {
                            "threadFlows": [
                                {
                                    "locations": [
                                        {
                                            "location": {
                                                "id": 0,
                                                "physicalLocation": {
                                                    "artifactLocation": {"uri": "src/helper.c"},
                                                    "region": {
                                                        "startLine": 18,
                                                        "endLine": 18,
                                                        "startColumn": 12,
                                                        "endColumn": 18,
                                                    },
                                                },
                                                "message": {
                                                    "text": (
                                                        "Use of uninitialized variable 'result'. Variable used before "
                                                        "initialization in function processData()."
                                                    )
                                                },
                                            },
                                            "nestingLevel": 0,
                                            "kinds": ["warning[c/UninitVar/test]"],
                                        }
                                    ]
                                }
                            ]
                        }
                    ],
                    "relatedLocations": [
                        {
                            "id": 1,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {"text": "   16|   int result;"},
                        },
                        {
                            "id": 2,
                            "physicalLocation": {"artifactLocation": {"uri": ""}},
                            "message": {"text": "   18|-> return result;"},
                        },
                    ],
                    "fingerprints": {
                        "csdiff/v0": "456789abcdef012345678901234567890abcdef1",
                        "csdiff/v1": "abcdef012345789012345678901234567890abcd1",
                    },
                },
            ],
        }
    ],
}

# Sample HTML data for integration testing
SAMPLE_HTML = """
<!DOCTYPE html>
<html>
<head>
    <title>test-project-scan-results</title>
</head>
<body>
<h1>test-project-scan-results</h1>
<h2>List of Findings</h2>
<pre>
<a id='def1'></a><b>Error: <span class='checker'>RESOURCE_LEAK</span> (<a href="https://cwe.mitre.org/data/definitions/772.html" title="CWE-772: Missing Release of Resource after Effective Lifetime">CWE-772</a>):</b> <a href='#def1'>[#def1]</a> <span class='impFlag'>[important]</span>
<span class='infoEvent'>src/file.c:891:13: <b>alloc_fn</b>: Storage is returned from allocation function &quot;g_strdup_inline&quot;.</span>
<span class='infoEvent'>src/file.c:891:13: <b>var_assign</b>: Assigning: &quot;full_url&quot; = storage returned from &quot;g_strdup_inline(target-&gt;target-&gt;path)&quot;.</span>
<span class='infoEvent'>src/file.c:919:9: <b>noescape</b>: Resource &quot;full_url&quot; is not freed or pointed-to in &quot;lr_is_local_path&quot;.</span>
<span class='infoEvent'>src/file.c:924:13: <b>noescape</b>: Assuming resource &quot;full_url&quot; is not freed or pointed-to as ellipsis argument to &quot;g_debug&quot;.</span>
src/file.c:946:21: <b>leaked_storage</b>: Variable &quot;full_url&quot; going out of scope leaks the storage it points to.
<span class='infoEventComment'>#<span class='traceEvent'>  944|                       g_set_error(err, VAR, LRE_CBINTERRUPTED,</span></span>
<span class='infoEventComment'>#<span class='traceEvent'>  945|                               &quot;Interrupted by LR_CB_ERROR from end callback&quot;);</span></span>
<span class='infoEventComment'>#<span class='ctxLine'>  946|-&gt;                     return FALSE;</span></span>
<span class='infoEventComment'>#<span class='traceEvent'>  947|                   }</span></span>
<span class='infoEventComment'>#<span class='traceEvent'>  948|               }</span></span>

<a id='def2'></a><b>Error: <span class='checker'>SQL_INJECTION</span> (<a href="https://cwe.mitre.org/data/definitions/89.html" title="CWE-89: Improper Neutralization of Special Elements used in an SQL Command">CWE-89</a>):</b> <a href='#def2'>[#def2]</a>
<span class='infoEvent'>src/login.c:25:5: <b>user_input</b>: User input received without validation.</span>
src/login.c:87:12: <b>sql_injection</b>: SQL injection vulnerability in login form where user input is not properly sanitized before being used in database query.
<span class='infoEventComment'>#<span class='traceEvent'>   85|   sprintf(query, "SELECT * FROM users WHERE username='%s'", username);</span></span>
<span class='infoEventComment'>#<span class='traceEvent'>   86|   // Direct user input concatenation</span></span>
<span class='infoEventComment'>#<span class='ctxLine'>   87|-> result = execute_query(query);</span></span>

<a id='def3'></a><b>Error: <span class='checker'>UNINIT_VAR</span> (<a href="https://cwe.mitre.org/data/definitions/457.html" title="CWE-457: Use of Uninitialized Variable">CWE-457</a>):</b> <a href='#def3'>[#def3]</a>
<span class='infoEvent'>src/helper.c:16:8: <b>uninit_var</b>: Variable "result" declared but not initialized.</span>
src/helper.c:18:12: <b>use_uninit</b>: Using uninitialized variable "result" in return statement.
<span class='infoEventComment'>#<span class='traceEvent'>   16|   int result;</span></span>
<span class='infoEventComment'>#<span class='traceEvent'>   17|   // Missing initialization</span></span>
<span class='infoEventComment'>#<span class='ctxLine'>   18|-> return result;</span></span>

</pre>
<h2>Scan Properties</h2>
<table id='scanProps'>
</table>
</body>
</html>
"""

# Sample Google Sheets data for testing
SAMPLE_SHEET_DATA = [
    {
        "False Positive?": "No",
        "Finding": """Error: RESOURCE_LEAK (CWE-772):
src/file.c:891:13: alloc_fn: Storage is returned from allocation function "g_strdup_inline".
src/file.c:891:13: var_assign: Assigning: "full_url" = storage returned from "g_strdup_inline(target->target->path)".
src/file.c:919:9: noescape: Resource "full_url" is not freed or pointed-to in "lr_is_local_path".
src/file.c:924:13: noescape: Assuming resource "full_url" is not freed or pointed-to as ellipsis argument to "g_debug".
src/file.c:946:21: leaked_storage: Variable "full_url" going out of scope leaks the storage it points to.
#  944|                       g_set_error(err, VAR, LRE_CBINTERRUPTED,
#  945|                               "Interrupted by LR_CB_ERROR from end callback");
#  946|->                     return FALSE;
#  947|                   }
#  948|               }""",
    },
    {
        "False Positive?": "No",
        "Finding": """Error: SQL_INJECTION (CWE-89):
SQL injection vulnerability in login form where user input is not properly sanitized before being used in database query.
File: src/login.c:87
Code: result = execute_query(query);""",
    },
    {
        "False Positive?": "No",
        "Finding": """Error: UNINIT_VAR (CWE-457):
Use of uninitialized variable 'result'. Variable used before initialization in function processData().
File: src/helper.c:18
Code: return result;""",
    },
]
