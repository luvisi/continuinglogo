
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


/* This is a reader for a primitive Lisp syntax.
   It is used to read initialize.txt, which implements many of
   the higher level capabilities of the interpreter, including
   procedure definition.
 */

#ifndef READER_H
#define READER_H
#include <stdio.h>
#include "pcgc.h"
#include "interpreter.h"

/* The maximum length of a single token.
   This is a silly limitation, but it only needs to be able
   to parse one single file, which I happen to know has
   no tokens even close to 100 characters long.
 */
#define TOKEN_MAXLEN 100


struct reader {
    int token_la_valid;     /* Do we have a lookahead token in buf?
                               This occurs after a token is put back. */
    char buf[TOKEN_MAXLEN]; /* The current token. */
    int bufused;            /* How much of buf is used. */
    int char_la, char_la_valid; /* char_la_valid is true if a character
                                   was put back.  If so, then char_la
                                   contains the character. */
    int (*char_reader)(struct reader *r); /* The C function currently being
                                             used to read characters. */

    /* Used when reading from a file */
    FILE *ifp;

    /* Used when reading from a string */
    char *source_string;
    int source_position;
};

typedef struct reader reader;



/* Create a reader */
reader *mk_reader(IC *ic);

/* Read from the user in terminal mode. */
void read_from_user_terminal(IC *ic);

/* Set the reader to read from a file pointer. */
void read_from_file(IC *ic, FILE *fp);

/* Set the reader to read from a string. */
void read_from_string(IC *ic, char *source);

/* Read one object from the input. */
sexpr *readobj(IC *ic);

#endif
