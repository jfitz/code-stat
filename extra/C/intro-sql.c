/* ---------------------------------------- */
/*                                          */
/*      EMBEDDED SQL IN C TEST PROGRAM      */
/*                                          */
/*               Frans Coenen               */
/*             (20 March, 1998)             */
/*                                          */
/* ---------------------------------------- */

/* To compile (assuming an appropriate .dcl file exists):
"esqlc tempest.sc" and then "cc -Aa -o tempest tempest.c
/cs/apps3/ingres/ingres/lib/libingres.a -lm -lc" */

/* Include statements. */

#include
EXEC SQL INCLUDE SQLCA;

/* Function prototypes */

void cleanUp(void);

/* ------ MAIN ------ */

/* Top level function */

void main(void)
{
EXEC SQL BEGIN DECLARE SECTION;
        char host_name[21];
        int host_emp_number;
        EXEC SQL INCLUDE "employees.dcl";
EXEC SQL END DECLARE SECTION;

EXEC SQL DECLARE empcsr CURSOR FOR
        SELECT name, emp_number
        FROM employees
        WHERE emp_number = 10001;

/* An error when opening the frans database will cause
the error to be printed and the program to be aborted */

EXEC SQL WHENEVER SQLERROR STOP;

EXEC SQL CONNECT frans;

/* Errors from here on will cause the program to clean up */

EXEC SQL WHENEVER SQLERROR CALL cleanUp;

EXEC SQL OPEN empcsr;

printf("Some values from the /"employees/" table /n");

/* When ever no more rows are fetched, close the cursor */

EXEC SQL WHENEVER NOT FOUND GOTO close_empcsr;

/* The last executable SQL statement was OPEN so we know that
the value of "sqlcode" cannot be SQLERROR or NOT FOUND */

while(sqlca.sqlcode == 0) {     /* Loop is broken by NOT FOUND */
        EXEC SQL FETCH empcsr
                INTO :host_name, :host_emp_number;

/* This "printf" does not execute after the previous FETCH returns the
NOT FOUND condition */

        printf("%s %d/n",host_name,host_emp_number);
        }

/* From this point onwards the program ignore all errors.
Also turn off the NOT FOUND condition for consistency */

EXEC SQL WHENEVER SQLERROR CONTINUE;
EXEC SQL WHENEVER NOT FOUND CONTINUE;

close_empcsr:
        EXEC SQL CLOSE empcsr;

EXEC SQL DISCONNECT;
}

/* ------ CLEAN UP ------ */

/* Error handling procedure (print error and disconnect) */

void cleanUp(void)
{
EXEC SQL BEGIN DECLARE SECTION;
        char errmsg[10];
EXEC SQL END DECLARE SECTION;

EXEC SQL INQUIRE_SQL (:errmsg = ERRORTEXT); /* Get error message. */
/* Alternatively to get only the error number
EXEC SQL COPY SQLERROR INTO :errmsg WITH 256; */

printf("Aborting because of error: /n%s/n",errmsg);

EXEC SQL DISCONNECT;
exit(-1);
}
