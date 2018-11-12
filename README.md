# Code-Stat web services

A set of web services that analyze source code.

The 'detect' web service identifies the language based on the source. Only
the one source file is required. (For example, a C source file does not require any header files for language identification.)

The 'confidence' web service reports on the confidence values computed during the 'detect' web service.

The 'tokens' web service breaks a source file into tokens, given a language name.
