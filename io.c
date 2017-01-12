
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

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "list_memory.h"
#include "byte_buffer.h"
#include "io.h"
#include "reader.h"
#include "logoreader.h"
#include "wxui.h"


/* io.c

   Implements input and output with support for adjustment of the input
   and output files and supports the use of a dribble file.

   The dribble file is supposed to contain a transcript of everything
   printed to the terminal, including all output, error messages, and
   everything typed by the user.
 */


/* Vector byte_buffer printf.
   Performs a printf into a byte_buffer.  Used by eprintf (error
   printf) for stashing error messages in ic->error_byte_buffer and
   used when printing into a word */
void vbbprintf(IC *ic, byte_buffer *bb, const char *format, va_list ap) {
    protect_ptr(ic->g, (void **)&bb);

    va_list ap_copy;
    int i, len;
    char *buf;

    /* va_lists cannot be reused, so they must be copied if they
       are going to be used again.
       We use one call to vsnprintf with a NULL target and a maximum
       length of 0 to figure out how large of a buffer we need. */
    va_copy(ap_copy, ap);
    len = vsnprintf(NULL, 0, format, ap_copy);
    va_end(ap_copy);

    /* Allocate the buffer and do the vsnprintf for real.
       vsnprintf does not call va_end, so to keep the interface of
       vabbprintf consistent with that of vsnprintf, we do not call it
       here either.  It is the caller's responsibility to call va_end
       on the passed va_list.

       Add +1 for the null byte.  According to the vsnprintf(3) man
       page:
           Upon successful return, these functions return the number
           of characters printed (excluding the null byte used to end
           output to strings).
     */
    buf = (char *)malloc((len+1)*sizeof(char));
    if(buf == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(EXIT_FAILURE);
    }

    vsnprintf(buf, len+1, format, ap);

    /* Add the contents of the buffer to the byte_buffer. */
    for(i = 0; i < len; i++) {
        add_to_byte_buffer(ic, bb, buf[i]);
    }

    free(buf);
    unprotect_ptr(ic->g);
}

/* A pointer to wx_vprintf() will be stored in ic->output_printer
   when we are running in GUI mode.  This is how things ultimately
   get printed to the user. */
void wx_vprintf(IC *ic, const char *format, va_list ap) {
    va_list ap_copy;
    int len;
    char *buf;

    /* va_lists cannot be reused, so they must be copied if they
       are going to be used again.
       We use one call to vsnprintf with a NULL target and a maximum
       length of 0 to figure out how large of a buffer we need. */
    va_copy(ap_copy, ap);
    len = vsnprintf(NULL, 0, format, ap_copy);
    va_end(ap_copy);

    /* Allocate the buffer and do the vsnprintf for real.
       vsnprintf does not call va_end, so to keep the interface of
       wx_vprintf consistent with that of vsnprintf, we do not call it
       here either.  It is the caller's responsibility to call va_end
       on the passed va_list.

       Add 1 because the return count does not include the null bye
       at the end of the string.
     */
    buf = (char *)malloc((len+1)*sizeof(char));
    if(buf == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(EXIT_FAILURE);
    }

    vsnprintf(buf, len+1, format, ap);

    wxString s(buf);
    free(buf);

    wxCommandEvent *printevent = new wxCommandEvent(PRINT_TEXT);
    printevent->SetString(s);
    wxTheApp->QueueEvent(printevent);

    /* Without this, the GUI thread can get so overrun with successive
       PRINT_TEXT events that it becomes unresponsive to UI events. */
    wxThread::This()->Sleep(1);
}

/* A pointer to terminal_vprintf() will be stored in ic->output_printer
   when we are running in terminal mode.  This is how things ultimately
   get printed to the user. */
void terminal_vprintf(IC *ic, const char *format, va_list ap) {
    /* Flush stdout before printing to stderr so that things
       are more likely to come out in the right order. */
    fflush(stdout);
    vfprintf(stderr, format, ap);
    fflush(stderr);
}

/* Vector terminal printf.  
   Used to print things directly to the terminal, even if output
   is redirected.  Anything printed to the terminal is copied
   to the dribble file, if there is one.
 */
void vtprintf(IC *ic, const char *format, va_list ap) {
    va_list ap_copy;

    if(!is_nil(ic, ic->dribble_name)) {
        /* A dribble file is open.  Send a copy to the dribble file. */
        va_copy(ap_copy, ap);
        vfprintf(ic->dribble->u.filep.file, format, ap_copy);
        va_end(ap_copy);
    }

    ic->output_printer(ic, format, ap);
}

/* Vector destination printf.
   Performs a printf to a given sexpr destination.
   If the destination is a FILEP, then we print to the associated file.
   If it is a BYTE_BUFFERP, then we print to the byte_buffer.
 */
void vdestprintf(IC *ic, sexpr *dest, const char *format, va_list ap) {
    if(dest->t == FILEP) {
        vfprintf(dest->u.filep.file, format, ap);
        fflush(dest->u.filep.file);
    } else if(dest->t == BYTE_BUFFERP)
        vbbprintf(ic, dest->u.byte_bufferp.bb, format, ap);
    else {
        fprintf(stderr, "Invalid argument to vdestprintf");
        longjmp(ic->quit, 1);
    }
}
   
/* Terminal printf.
   This is a wrapper around vtprintf that has printf style arguments. */
void tprintf(IC *ic, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    vtprintf(ic, format, ap);
    va_end(ap);
}

/* Logo printf.
   This is the print function for normal output.
   It prints to the user if output is not redirected, and otherwise
   prints to the current output destination
 */
void lprintf(IC *ic, const char *format, ...) {
    va_list ap;
    va_start(ap, format);

    if(is_nil(ic, ic->output_name))
        vtprintf(ic, format, ap);
    else
        vdestprintf(ic, ic->output, format, ap);

    va_end(ap);
}


/* This is the function for printing error messages.
   Prints to ic->error_byte_buffer to add to an error that will
   be thrown by throw_error() or fatal_error().

   If we are not inside of a CATCH "ERROR [...] or if fatal_error()
   is called, then the error will be printed and control will return
   to the top level.

   If we are inside of a CATCH "ERROR [...] and the throw_error() is
   called, then the error will be stored for retrieval by ERROR.
 */
void eprintf(IC *ic, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    vdestprintf(ic, ic->error_byte_buffer, format, ap);
    va_end(ap);
}

typedef void (*printerf)(IC *ic, const char *format, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

/* This is the recursive procedure to print sexprs.
   It prints sexpr e using the supplied printer, which will be
   tprintf, lprintf, or eprintf.
 */
static void io_print_sexpr(IC *ic, printerf printer, sexpr *e) {
    protect_ptr(ic->g, (void **)&e);

    int fullprint = 0;

    /* FULLPRINTP controls whether we will escape delimiters in the
       output.

       In theory, when FULLPRINTP is set, we will write
       out valid logo expressions that can be reread back in to duplicate
       the original data structure.

       In practice, many structures cannot be read back in.
     */
    if(name_eq(ic->n_fullprintp->u.name.symbol->value, ic->n_true))
        fullprint = 1;

    if(e == NULL) {
        printer(ic, "#<NULL>");
        return;
    }

    switch(e->t) {
        case SUBR:
            /* SUBR's are primitive subroutines.  They cannot be read back
               in. */
            printer(ic, "#<SUBR ");
            io_print_sexpr(ic, printer, e->u.subr.name);
            printer(ic, ">");
            break;

        case FSUBR:
            /* FSUBR's are primitive subroutines called on an unevaluated
               list of arguments.  They cannot be read back in. */
            printer(ic, "#<FSUBR ");
            io_print_sexpr(ic, printer, e->u.fsubr.name);
            printer(ic, ">");
            break;

        case FUNARG:
            /* FUNARG's are closures containing a procedure and a dynamic
               environment in which to execute the procedure.
               They cannot be read back in. */
            printer(ic, "#<FUNARG ");
            io_print_sexpr(ic, printer, e->u.funarg.lfun);
            printer(ic, ">");
            break;

        case MACRO:
            /* MACRO's cannot be read back in. */
            printer(ic, "#<MACRO ");
            io_print_sexpr(ic, printer, e->u.macro.macro_type);
            printer(ic, " ");
            io_print_sexpr(ic, printer, e->u.macro.expander);
            printer(ic, ">");
            break;

        case CONTINUATION:
            /* CONTINUATION's cannot be read back in. */
            printer(ic, "#<CONTINUATION>");
            break;

        case UNBOUND:
            /* The UNBOUND object cannot be read back in. */
            printer(ic, "#<UNBOUND>");
            break;

        case NAME:
            {
                /* When printing a name, we don't honor fullprintp if
                   the name would be read back in correctly without it. */
                unsigned int i;
                int really_fullprint = fullprint;
                if(name_eq(e, ic->n_plus) ||
                   name_eq(e, ic->n_minus) ||
                   name_eq(e, ic->n_star) ||
                   name_eq(e, ic->n_slash) ||
                   name_eq(e, ic->n_equal) ||
                   name_eq(e, ic->n_lt) ||
                   name_eq(e, ic->n_gt) ||
                   name_eq(e, ic->n_le) ||
                   name_eq(e, ic->n_ge) ||
                   name_eq(e, ic->n_ne))
                    really_fullprint = 0;

                /* Print out the name, adding backslashes before any
                   characters that would otherwise terminate reading. */
                for(i = 0; i < e->u.name.length; i++) {
                  char ch = e->u.name.head[e->u.name.start+i];
                  if(really_fullprint &&
                     !isalnum(ch) &&
                     !strchr("\":._?", ch))
                      printer(ic, "\\");
                  printer(ic, "%c", ch);
              }
            }
            break;

        case CONS:
            /* Print out a list, with recursive calls to print the
               elements. 
               We know the list isn't empty because the empty list
               has its own data type, different from a CONS. */
            printer(ic, "[");
            io_print_sexpr(ic, printer, car(e));
            STORE(ic->g, NULL, e, cdr(e));
            while(!is_nil(ic, e)) {
              if(e->t != CONS) {
                  printer(ic, " . ");
                  io_print_sexpr(ic, printer, e);
                  break;
              }
              printer(ic, " ");
              io_print_sexpr(ic, printer, car(e));
              STORE(ic->g, NULL, e, cdr(e));
            }
            printer(ic, "]");
            break;

        case ARRAY:
            /* Print an array literal. */
            printer(ic, "{");
            {
              unsigned int i;
              for(i = 0; i < e->u.array.length; i++) {
                  if(i > 0) printer(ic, " ");
                  io_print_sexpr(ic, printer, e->u.array.members[i]);
              }
            }
            printer(ic, "}");
            if(e->u.array.origin != 1)
                printer(ic, "@%d", e->u.array.origin);
            break;

        case NUMBER:
            printer(ic, "%g", e->u.number.value);
            break;

        case LINE:
            /* Lines cannot be read back on, though there is no
               reason why this must be the case. */
            printer(ic, "#<LINE raw=\"");
            io_print_sexpr(ic, printer, e->u.line.raw_line);
            printer(ic, "\" parsed=");
            io_print_sexpr(ic, printer, e->u.line.parsed_line);
            printer(ic, "\" procedure=");
            io_print_sexpr(ic, printer, e->u.line.procedure);
            printer(ic, ">");
            break;
            

        case PROC:
            /* PROC's cannot be read back in. */
            printer(ic, "#<PROC ");
            io_print_sexpr(ic, printer, e->u.proc.proc);
            printer(ic, " minargs=%d", e->u.proc.minargs);
            printer(ic, " defargs=%d", e->u.proc.defargs);
            printer(ic, " maxargs=%d", e->u.proc.maxargs);
            printer(ic, ">");
            break;

        case EMPTY_LIST:
            printer(ic, "[]");
            break;

        default:
            printer(ic, "#<Invalid type %p>\n", e);
            break;
    }

    unprotect_ptr(ic->g);
}


/* Print the sexpr using lprintf */
void lprint_sexpr(IC *ic, sexpr *e) {
    io_print_sexpr(ic, lprintf, e);
}

/* Print the sexpr using tprintf */
void tprint_sexpr(IC *ic, sexpr *e) {
    io_print_sexpr(ic, tprintf, e);
}

/* Print the sexpr using eprintf */
void eprint_sexpr(IC *ic, sexpr *e) {
    io_print_sexpr(ic, eprintf, e);
}


/* Searches the open_files list to see if any file with name "name"
   is open.  Returns the FILEP or BYTE_BUFFERP object if so, returns
   nil if not.
 */
static sexpr *findfile(IC *ic, sexpr *name) {
    protect_ptr(ic->g, (void **) &name);

    sexpr *l = ic->open_files;
    protect_ptr(ic->g, (void **) &l);

    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    while(l->t == CONS) {
        /* Check for object equality so we can find matching lists
           used for printing to a name. */
        if(name == car(car(l))) {
            STORE(ic->g, NULL, ret, cdr(car(l)));
            goto end;
        }

        /* Check for name equality for normal file operations. */
        if(name_eq(name, car(car(l)))) {
            STORE(ic->g, NULL, ret, cdr(car(l)));
            goto end;
        }
        STORE(ic->g, NULL, l, cdr(l));
    }


    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Opens the file pointed to by "path" in the mode pointed to by "mode".
   It is an error if the file is already open.
   If successfully opened, the path and file are added to the open_files
   list.
 */
static void lopen(IC *ic, sexpr *path, const char *mode) {
    protect_ptr(ic->g, (void **)&path);
    sexpr *file = findfile(ic, path); /* Check for file already open. */
    protect_ptr(ic->g, (void **)&file);
    FILE *fp;
    
    if(!is_nil(ic, file)) {
        /* Error if already open. */
        eprintf(ic, "File ");
        eprint_sexpr(ic, path);
        eprintf(ic, " is already open");
        throw_error(ic, ic->continuation->line);
    }

    char *path_cs = get_cstring(ic, path);
    protect_ptr(ic->g, (void **)&path_cs);

    if(path_cs == NULL)
        bad_argument(ic, path);

    fp = fopen(path_cs, mode);
    if(fp == NULL) {
        eprintf(ic, "Unable to open file %s in mode %s.\n",
                    path_cs, mode);
        throw_error(ic, ic->continuation->line);
    }

    STORE(ic->g, NULL, file, mk_filep(ic, fp));

    /* Add the path and file to the open file list. */
    sexpr *pair = cons(ic, path, file);
    protect_ptr(ic->g, (void **)&pair);
    STORE(ic->g, NULL,
          ic->open_files,
          cons(ic, pair, ic->open_files));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

}

void openread(IC *ic, sexpr *path)   { lopen(ic, path, "r"); }

void openwrite(IC *ic, sexpr *path)  {
    protect_ptr(ic->g, (void **)&path);

    if(path->t == CONS) {
        /* If a list is opened for writing, then the first element of
           the list is a name whose value will be set to another name
           containing all of the material printed to this destination. */
        if(car(path)->t != NAME)
            bad_argument(ic, path);
        if(!is_nil(ic, findfile(ic, path))) {
            eprintf(ic, "Output buffer ");
            eprint_sexpr(ic, path);
            eprintf(ic, " is already open");
            throw_error(ic, ic->continuation->line);
        }
        /* The output destination associated with the list is a byte_buffer.
           Any data printed while the output is set to the list will be
           added to the byte_buffer, and the byte_buffer will be converted
           to a name when the output is closed. */
        sexpr *output_bbp = mk_byte_bufferp(ic, mk_byte_buffer(ic));
        protect_ptr(ic->g, (void **)&output_bbp);

        sexpr *pair = cons(ic, path, output_bbp);
        protect_ptr(ic->g, (void **)&pair);

        STORE(ic->g, NULL,
              ic->open_files,
              cons(ic, pair, ic->open_files));

        unprotect_ptr(ic->g);
        unprotect_ptr(ic->g);
    } else if(path->t == NAME) {
        lopen(ic, path, "w");
    } else {
        bad_argument(ic, path);
    }

    unprotect_ptr(ic->g);
}

void openappend(IC *ic, sexpr *path) { lopen(ic, path, "a"); }
void openupdate(IC *ic, sexpr *path) { lopen(ic, path, "a+"); }

void lclose(IC *ic, sexpr *path) {
    /* If the destination we are closing is the current input or output
       then we need to set the current input or output back to the terminal.
     */
    if(name_eq(path, ic->input_name))
        setread(ic, ic->g_nil);
    if(path == ic->output_name || name_eq(path, ic->output_name))
        setwrite(ic, ic->g_nil);

    /* We use a pointer to a pointer to make it easier to delete
       the item from the linked list.
       I think this is safe in this case if I am careful about
       word_from_byte_buffer. */
    sexpr *linking_object = NULL;
    sexpr **l = &ic->open_files;

    while(!is_nil(ic, *l)) {
        if(car(car(*l))->t == NAME &&
           name_eq(car(car(*l)), path)) {
            /* We found the destination in the list.
               Close the file and remove this destination from
               the open files list. */
            if(fclose(cdr(car(*l))->u.filep.file)) {
                eprintf(ic, "Error closing file ");
                eprint_sexpr(ic, path);
                eprintf(ic, ": %s", strerror(errno));
                throw_error(ic, ic->continuation->line);
            }
            STORE(ic->g, linking_object, *l, cdr(*l));
            return;
        }
        if(car(car(*l))->t == CONS &&
           car(car(*l)) == path) {
            /* We found the destination, and it is a CONS.
               Create a word containing the contents of the associated
               byte_buffer, and store it as the value of the word
               in the car. */
            /* car(*l) is the pair containing a destination description
               (name or CONS in the car, in this case a CONS, and
               a FILEP or BYTE_BUFFERP in the cdr, in this case a
               BYTE_BUFFERP).
               car(car(*l)) is the destination description, in this
               case a cons.
               car(car(car(*l))) is the name to which we are printing.
               Its value will become a new name containing the output
               stored in the byte_buffer. */

            /* Protect these from collection during word_from_byte_buffer */
            sexpr *name = car(car(car(*l)));
            protect_ptr(ic->g, (void **)&name);

            sexpr *bbp = cdr(car(*l));
            protect_ptr(ic->g, (void **)&bbp);

            /* Remove the pair from the open files list.
               This needs to happen before word_from_byte_buffer()
               so that garbage collection doesn't move linking_object. */
            STORE(ic->g, linking_object, *l, cdr(*l));

            sexpr *value =
                word_from_byte_buffer(ic, bbp->u.byte_bufferp.bb);
            protect_ptr(ic->g, (void **)&value);

            STORE(ic->g, name->u.name.symbol,
                  name->u.name.symbol->value,
                  value);

            unprotect_ptr(ic->g);
            unprotect_ptr(ic->g);
            unprotect_ptr(ic->g);

            return;
        }
        linking_object = *l;
        l = &(cdr(*l));
    }

    eprintf(ic, "Attempt to close file that is not open: ");
    eprint_sexpr(ic, path);
    throw_error(ic, ic->continuation->line);
}


/* Test for end of file on input.  This works by reading ahead a character,
   so it can hang when called with the input set to the terminal.
 */
sexpr *leofp(IC *ic) {
    FILE *fp = ic->input->u.filep.file;
    int ch = getc(fp);
    if(ch == EOF)
        return ic->n_true;
    ungetc(ch, fp);
    return ic->n_false;
}

/* Open a dribble file.
   All output printed to the terminal will also be sent to the dribble
   file.
 */
void dribble(IC *ic, sexpr *name) {
    protect_ptr(ic->g, (void **)&name);

    sexpr *filep = NULL;
    protect_ptr(ic->g, (void **)&filep);

    if(is_nil(ic, name)) {
        sexpr *old_name = ic->dribble_name;
        protect_ptr(ic->g, (void **)&old_name);

        STORE(ic->g, NULL, ic->dribble, ic->g_nil);
        STORE(ic->g, NULL, ic->dribble_name, ic->g_nil);
        if(!is_nil(ic, old_name))
            lclose(ic, old_name);

        unprotect_ptr(ic->g);
    } else {
        STORE(ic->g, NULL, filep, findfile(ic, name));
        if(is_nil(ic, filep)) {
            lopen(ic, name, "a");
            STORE(ic->g, NULL, filep, findfile(ic, name));
        }

        STORE(ic->g, NULL, ic->dribble, filep);
        STORE(ic->g, NULL, ic->dribble_name, name);
    }


    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}


/* Stop dribbling. */
void nodribble(IC *ic) {
    dribble(ic, ic->g_nil);
}

/* Begin reading from file "name".  The file must already be open. */
void setread(IC *ic, sexpr *name) {
    protect_ptr(ic->g, (void **)&name);

    sexpr *filep = NULL;
    protect_ptr(ic->g, (void **)&filep);

    if(is_nil(ic, name)) {
        STORE(ic->g, NULL, ic->input, mk_filep(ic, NULL));
        STORE(ic->g, NULL, ic->input_name, ic->g_nil);
        read_from_file(ic, NULL);
        logoread_from_file(ic->lr, NULL);
    } else {
        STORE(ic->g, NULL, filep, findfile(ic, name));
        if(is_nil(ic, filep)) {
            eprintf(ic, "File ");
            eprint_sexpr(ic, name);
            eprintf(ic, " is not open");
            throw_error(ic, ic->continuation->line);
        }

        STORE(ic->g, NULL, ic->input, filep);
        STORE(ic->g, NULL, ic->input_name, name);
        read_from_file(ic, filep->u.filep.file);
        logoread_from_file(ic->lr, filep->u.filep.file);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

/* Begin writing to file "name".  The file must already be open. */
void setwrite(IC *ic, sexpr *name) {
    protect_ptr(ic->g, (void **)&name);
    
    sexpr *filep = NULL;
    protect_ptr(ic->g, (void **)&filep);

    if(is_nil(ic, name)) {
        /* Switch to writing to the user.
           ic->output is only used for WRITEPOS to stdout since
           lprintf() calls vtprintf() if ic->output_name is nil. */
        STORE(ic->g, NULL, ic->output, mk_filep(ic, stdout));
        STORE(ic->g, NULL, ic->output_name, ic->g_nil);
    } else {
        STORE(ic->g, NULL, filep, findfile(ic, name));
        if(is_nil(ic, filep)) {
            eprintf(ic, "File ");
            eprint_sexpr(ic, name);
            eprintf(ic, " is not open");
            throw_error(ic, ic->continuation->line);
        }

        STORE(ic->g, NULL, ic->output, filep);
        STORE(ic->g, NULL, ic->output_name, name);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

/* Generate a list of all open files to be returned by allopen() below. */
static sexpr *allopen_helper(IC *ic, sexpr *flist) {
    protect_ptr(ic->g, (void **)&flist);

    /* Create list head */
    sexpr *ret = cons(ic, ic->g_nil, ic->g_nil);
    protect_ptr(ic->g, (void **)&ret);
    sexpr *tail = ret;
    protect_ptr(ic->g, (void **)&tail);

    /* Push file names onto list */
	while(!is_nil(ic, flist)) {
        push_back(ic, &tail, car(car(flist)));
        STORE(ic->g, NULL, flist, cdr(flist));
	}

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    /* Return list, minus list head */
    return cdr(ret);
}

/* Return a list of all open files. */
sexpr *allopen(IC *ic) {
    return allopen_helper(ic, ic->open_files);
}

/* Close all open files. */
void closeall(IC *ic) {
    sexpr *l = allopen(ic);
    protect_ptr(ic->g, (void **) &l);

    while(!is_nil(ic, l)) {
        lclose(ic, car(l));
        STORE(ic->g, NULL, l, cdr(l));
    }
    unprotect_ptr(ic->g);
}

/* Delete a file. */
void erasefile(IC *ic, sexpr *path) {
    char *path_cs = get_cstring(ic, path);
    
    if(unlink(path_cs)) {
        eprintf(ic, "Error erasing file ");
        eprint_sexpr(ic, path);
        eprintf(ic, ": %s", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }
}

/* Fetch the current input. */
sexpr *lreader(IC *ic) {
    return ic->input_name;
}

/* Fetch the current output. */
sexpr *writer(IC *ic) {
    return ic->output_name;
}

/* Fetch the current input position. */
sexpr *readpos(IC *ic) {
    return mk_number(ic, ftell(ic->input->u.filep.file));
}

/* Fetch the current output position. */
sexpr *writepos(IC *ic) {
    return mk_number(ic, ftell(ic->output->u.filep.file));
}

/* Set the current input position. */
void setreadpos(IC *ic, sexpr *pos) {
    long loc = truncl(to_number(ic, pos)->u.number.value);
    if(fseek(ic->input->u.filep.file,
             loc,
             SEEK_SET)) {
        eprintf(ic, "Error seeking in file ");
        eprint_sexpr(ic, ic->input_name);
        eprintf(ic, ": %s", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }
}

/* Set the current output position. */
void setwritepos(IC *ic, sexpr *pos) {
    if(ic->output_name->t != NAME) {
        eprintf(ic, "Cannot SETWRITEPOS when output is a list: ");
        eprint_sexpr(ic, ic->output_name);
        throw_error(ic, ic->continuation->line);
    }

    long loc = truncl(to_number(ic, pos)->u.number.value);

    if(fseek(ic->output->u.filep.file,
             loc,
             SEEK_SET)) {
        eprintf(ic, "Error seeking in file ");
        eprint_sexpr(ic, ic->output_name);
        eprintf(ic, ": %s", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }
}

/* Is the file readable? */
sexpr *filep(IC *ic, sexpr *name) {
    char *path_cs = get_cstring(ic, name);
    if(access(path_cs, R_OK))
        return ic->n_false;
    else
        return ic->n_true;
}

/* A string array is an array of char *'s.  It is used when executing
   system programs by openshell and system.
   The end is marked by a NULL pointer.
 */
void mark_string_array(GC *g,
                       void *c, object_marker om, weak_pointer_registerer wpr) {
  char **a  = (char **) c;
  if(a != NULL) {
      while(*a != NULL) {
          om(g, (void **) a);
          a++;
      }
  }
}

/* Create a string array whose elements point to char *'s created from
   the names in the argument.
 */
char **mk_string_array(IC *ic, sexpr *names) {
    protect_ptr(ic->g, (void **) &names);

    sexpr *np = names;
    protect_ptr(ic->g, (void **) &np);

    int i, count;
    char **ret;
    
    /* Count how many items we have. */
    count = 0;
    while(!is_nil(ic, np) && np->t == CONS) {
        count++;
        STORE(ic->g, NULL, np, cdr(np));
    }
    if(!is_nil(ic, np) || count == 0)
        bad_argument(ic, names);

    /* Need to allocate an extra pointer for the NULL marker at the end. */
    /* Needs to be protected during the allocations that occur in get_cstring
       and that can occur in to_name. */
    ret = (char **)ic_xmalloc(ic, sizeof(char *)*(count+1), mark_string_array);
    protect_ptr(ic->g, (void **) &ret);

    /* We need to NULL out ret so that if the allocations below cause
       attempts to traverse it during garbage collection nothing bad
       will happen.
       mark_string_array() stops when it gets to a NULL, so after this
       loop it will not mark any children until we add them below.
       This loop also adds the terminating NULL to ret[count]. */
    for(i = 0; i < count+1; i++)
        ret[i] = NULL;

    STORE(ic->g, NULL, np, names);
    i = 0;
    while(!is_nil(ic, np)) {
        char *str = get_cstring(ic, to_name(ic, car(np)));
        protect_ptr(ic->g, (void **) &str);

        STORE(ic->g, ret, ret[i], str);
        i++;

        unprotect_ptr(ic->g);
        STORE(ic->g, NULL, np, cdr(np));
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}


/* Runs the command specified by args, and opens it as a readable
   input named "name". */
void openshell(IC *ic, sexpr *name, sexpr *args) {
    protect_ptr(ic->g, (void **)&name);
    protect_ptr(ic->g, (void **)&args);

    sexpr *file = findfile(ic, name);
    protect_ptr(ic->g, (void **)&file);

    char **argv = NULL;
    protect_ptr(ic->g, (void **)&argv);

    FILE *fp;
    int pipefd[2];
    
    /* Convert to a name and force an error early if we can't. */
    STORE(ic->g, NULL, name, to_name(ic, name));

    if(!is_nil(ic, file)) {
        eprintf(ic, "File ");
        eprint_sexpr(ic, name);
        eprintf(ic, " is already open");
        throw_error(ic, ic->continuation->line);
    }

    if(args->t == NAME) {
        /* If args is just a single name, then run it with the shell. */
        sexpr *sh = intern(ic, "sh");
        protect_ptr(ic->g, (void **) &sh);

        sexpr *mc = intern(ic, "-c");
        protect_ptr(ic->g, (void **) &mc);

        STORE(ic->g, NULL, argv,
              mk_string_array(ic, listl(ic, sh, mc, args, NULL)));
        // argv = mk_string_array(ic,
        //            cons(ic, sh,
        //                     cons(ic, mc,
        //                              cons(ic, args, ic->g_nil))));
        unprotect_ptr(ic->g);
        unprotect_ptr(ic->g);
    } else if(args->t == CONS) {
        /* If args is a list, then use it as the arguments to execvp. */
        STORE(ic->g, NULL, argv, mk_string_array(ic, args));
    } else {
        bad_argument(ic, args);
    }

    /* Create a pipe. */
    if(pipe(pipefd) < 0) {
        eprintf(ic, "Error running ");
        eprint_sexpr(ic, args);
        eprintf(ic, ": %s\n", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }

    /* Create a child process. */
    if(fork() == 0) {
        /* Child */

        /* Close the read end of the pipe in the child. */
        if(close(pipefd[0]) < 0) {
            eprintf(ic, "Error closing fd in child: %s\n", strerror(errno));
            throw_error(ic, ic->continuation->line);
        }

        /* Make the child's output go to the pipe. */
        if(dup2(pipefd[1], 1) < 0) {
            eprintf(ic, "Error dup2'ing fd in child: %s\n", strerror(errno));
            throw_error(ic, ic->continuation->line);
        }

        /* Exec the program. */
        if(execvp(argv[0], argv) < 0) {
            eprintf(ic, "Error execvp'ing in child: %s\n", strerror(errno));
            throw_error(ic, ic->continuation->line);
        }
    }
    
    /* Close the writer in the parent. */
    if(close(pipefd[1]) < 0) {
        eprintf(ic, "Error closing fd in parent: %s\n", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }

    /* Create a FILE * that wraps the reading end of the pipe. */
    fp = fdopen(pipefd[0], "r");
    if(fp == NULL) {
        eprintf(ic, "Error fdopen'ing fd in parent: %s\n", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }

    /* Wrap it in a FILEP sexpr. */
    STORE(ic->g, NULL, file, mk_filep(ic, fp));

    /* Add it to the open files list. */
    sexpr *pair = cons(ic, name, file);
    protect_ptr(ic->g, (void **)&pair);

    STORE(ic->g, NULL,
          ic->open_files,
          cons(ic, pair, ic->open_files));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}


/* Runs the args as a system command and waits for the command to terminate.
   returns the exit status. */
sexpr *systemf(IC *ic, sexpr *args) {
    protect_ptr(ic->g, (void **) &args);

    char **argv = NULL;
    protect_ptr(ic->g, (void **) &argv);

    int pid, status;
    
    if(args->t == NAME) {
        /* Use the shell if args is a name. */
        sexpr *sh = intern(ic, "sh");
        protect_ptr(ic->g, (void **) &sh);
        sexpr *mc = intern(ic, "-c");
        protect_ptr(ic->g, (void **) &mc);

        STORE(ic->g, NULL, argv, mk_string_array(ic, listl(ic, sh, mc, args, NULL)));

        unprotect_ptr(ic->g);
        unprotect_ptr(ic->g);
    } else if(args->t == CONS) {
        /* Use args as the arguments to execvp directly if it is a list. */
        STORE(ic->g, NULL, argv, mk_string_array(ic, args));
    } else {
        bad_argument(ic, args);
    }

    /* Create the child. */
    pid = fork();
    if(pid == 0) {
        /* Child */

        /* Execute the program. */
        if(execvp(argv[0], argv) < 0) {
            eprintf(ic, "Error execvp'ing in child: %s\n", strerror(errno));
            throw_error(ic, ic->continuation->line);
        }
    }

    /* Wait for the child to terminate. */
    if(waitpid(pid, &status, 0) < 0) {
        eprintf(ic, "Error waitpid'ing in parent: %s\n", strerror(errno));
        throw_error(ic, ic->continuation->line);
    }
    
    /* Return the exit status. */
    sexpr *ret = mk_number(ic, WEXITSTATUS(status));
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

