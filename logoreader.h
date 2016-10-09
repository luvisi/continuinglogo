
/*
    ContinuingLogo Logo Interpreter 
    
    Copyright (C) 2014 Andru Luvisi
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */



/* This is the reader for Logo expressions. */

#ifndef LOGOREADER_H
#define LOGOREADER_H
#include <stdio.h>
#include "pcgc.h"
#include "interpreter.h"
#include "byte_buffer.h"

enum breaking_state { BROKE_ON_SPACE, BROKE_ON_NONSPACE };

struct logoreader {
    IC *ic;
    int token_la_valid;  /* Do we have a token lookahead? */
    byte_buffer *bb;     /* byte_buffer containing the token being built.
                            Contains the lookahead token when there is one. */
    int token_escaped;   /* Were there any \ or | escapes while reading the
                            token?  If so, then we don't want to interpret
                            brackets or braces specially. */
    int char_la_valid;   /* Is there a lookahead character? */
    int char_la;         /* Lookahead character */

    /* The function currently being used to read a character. */
    int (*char_reader)(struct logoreader *r);

    /* Used when reading from a file */
    FILE *ifp;

    /* Used when reading from a string */
    char *source_string;
    int source_position;

    /* Used for reading the raw line, which is used to create LINE
       objects and for printing out the source code with RAWTEXT. */
    byte_buffer *raw_line;
    int logging_raw_line;

    /* Did the last token end by finding a whitespace character
       or a non-whitespace character?  Used for determining whether
       a minus is binary or unary. */
    enum breaking_state last_break;

    /* Are we blocking on wxWidgets reads from the user? */
    int blocking;
};

typedef struct logoreader logoreader;


/* Create a reader. */
logoreader *mk_logoreader(IC *ic);

/* Linemode, charmode_blocking, and charmode_nonblocking when using a
   terminal. */
void linemode_terminal(logoreader *r);
void charmode_blocking_terminal(logoreader *r);
void charmode_nonblocking_terminal(logoreader *r);

/* Begin reading from the user in terminal mode. */
void logoread_from_user_terminal(logoreader *r);

/* Linemode, charmode_blocking, and charmode_nonblocking when using
   wxWidgets. */
void linemode_wx(logoreader *r);
void charmode_blocking_wx(logoreader *r);
void charmode_nonblocking_wx(logoreader *r);

/* Begin reading from the user in wxWidgets. */
void logoread_from_user_wx(logoreader *r);

/* Print a prompt if reading from the user. */
void maybe_prompt_terminal(logoreader *lr, const char *prompt);
void maybe_prompt_wx(logoreader *lr, const char *prompt);


/* Set the source for this reader to a file or a string. */
void logoread_from_file(logoreader *lr, FILE *fp);
void logoread_from_string(logoreader *lr, char *source);

/* Read an object. */
sexpr *logoreadobj(logoreader *lr);

/* Read a newline terminated list. */
sexpr *readlist(logoreader *lr);

/* Read a newline terminated word. */
sexpr *readword(logoreader *lr);

/* Read a newline terminated word with no escape processing. */
sexpr *readrawline(logoreader *lr);

/* Read a single character.  Use cbreak mode if reading from the
   terminal. */
sexpr *readchar(logoreader *lr);

/* Read a particular number of characters.
   Use cbreak mode if reading from the terminal. */
sexpr *readchars(logoreader *lr, int count);

/* Read a LINE to be used in the definition of a procedure.
   The created LINE object includes the procedure, the parsed,
   and the raw versions of the line read. */
sexpr *readline(logoreader *lr, sexpr *procedure, sexpr *prompt);

/* Is there a key press waiting to be read? */
sexpr *keyp(logoreader *lr);

#endif
