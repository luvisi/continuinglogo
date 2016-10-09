
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

#ifndef IO_H
#define IO_H

#include <stdarg.h>
#include "list_memory.h"
#include "interpreter.h"

/* The normal printing function. */
/* Like printf, but prints to the current output FILEP.
   Only prints to the dribble file if we are printing to the
   terminal. */
void lprintf(IC *ic, const char *format, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

/* Output printers, assigned to ic->output_printer depending on
   whether we are using wxWidgets or a terminal interface. */
void wx_vprintf(IC *ic, const char *format, va_list ap);
void terminal_vprintf(IC *ic, const char *format, va_list ap);

/* The terminal printing function. */
/* Always prints to terminal and to the dribble file if there is one. */
void tprintf(IC *ic, const char *format, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

/* The error printing function. */
/* Prints to ic->error_byte_buffer to add to an error that will
   be thrown. */
void eprintf(IC *ic, const char *format, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

/* Print the sexpr using lprintf */
void lprint_sexpr(IC *ic, sexpr *e);

/* Print the sexpr using tprintf */
void tprint_sexpr(IC *ic, sexpr *e);

/* Print the sexpr using eprintf */
void eprint_sexpr(IC *ic, sexpr *e);

/* Opens the path in the appropriate mode. */
void openread(IC *ic, sexpr *path);
void openwrite(IC *ic, sexpr *path);
void openappend(IC *ic, sexpr *path);
void openupdate(IC *ic, sexpr *path);

/* Close the path. */
void lclose(IC *ic, sexpr *path);

/* Check for end of file. */
sexpr *leofp(IC *ic);

/* Set or clear the dribble file. */
void dribble(IC *ic, sexpr *path);
void nodribble(IC *ic);

/* Set or clear the input or output stream. */
void setread(IC *ic, sexpr *name);
void setwrite(IC *ic, sexpr *name);

/* Get a list of all open files. */
sexpr *allopen(IC *ic);

/* Close all open files, and redirect input and output to the terminal. */
void closeall(IC *ic);

/* unlink() a file. */
void erasefile(IC *ic, sexpr *name);

/* Return the current reader/writer. */
sexpr *lreader(IC *ic);
sexpr *writer(IC *ic);

/* Get the current position in the reader/writer. */
sexpr *readpos(IC *ic);
sexpr *writepos(IC *ic);

/* Set the current position in the reader/writer. */
void setreadpos(IC *ic, sexpr *pos);
void setwritepos(IC *ic, sexpr *pos);

/* Is the name a path to a valid file? */
sexpr *filep(IC *ic, sexpr *name);

/* Run a command and open its output as a readable file.
   Used by SHELL. */
void openshell(IC *ic, sexpr *name, sexpr *args);

/* Run a command and return its exit status.  Used by EDIT. */
sexpr *systemf(IC *ic, sexpr *args);

#endif
