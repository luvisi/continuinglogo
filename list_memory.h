
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

#ifndef LIST_MEMORY_H
#define LIST_MEMORY_H
#include "pcgc.h"
#include "interpreter.h"

#include <stdio.h>

/* This is a wrapper around allocate that aborts execution on an
   out of memory error. */
void *ic_xmalloc(IC *ic, size_t size, pointer_iterator marker);

/* These are the declarations for "struct sexpr" and "struct continuation"
   and the declarations for the associated mark_* and mk_* functions. */
#include "structures.h"

typedef struct sexpr sexpr;
typedef struct continuation continuation;

/* This is the structure for an environmental frame.
   Frames are stored in a stack, with each frame temporarily storing
   shadowed values of global variables.
 */

struct frame {
    struct frame *parent; /* Parent frame from a higher level procedure call */
    struct frame *to_active; /* This is NULL in the active frame.
                                In all other frames, it points along a path
                                towards the active frame.
                                Used by reroot() to figure out the path between
                                the frame currently active and the one we wish
                                to make active. */
    sexpr *bindings; /* A list of bindings in this frame.  Can sometimes
                        share bindings with another frame that has the
                        same parent frame as a result of tail calls. */
    sexpr *procedure; /* The procedure that is running when this frame is
                         active. */
    sexpr *output_error_info; /* Information to use in generating errors
                                 of the type "You don't say what to do with .."
                                 or "XXX didn't output to YYY" */
    struct continuation *continuation; /* The continuation for returning from
                                          this procedure.  Used by OUTPUT
                                          and STOP. */

    int allowed_results; /* The allowed results for the procedure call
                            expression that created this frame.  All
                            that really matters is the VALUE_MASK values. */

    /* Information about the parent frame for use in upstack_output.
       Needs to be tracked here because the parent frame can be
       eliminated by tail call optimization. */
    struct continuation *parent_continuation;
    int parent_allowed_results;
};
typedef struct frame frame;

/* Function for creating a new frame. */
frame *mk_frame(IC *ic, frame *parent,
                        sexpr *bindings,
                        sexpr *procedure,
                        sexpr *output_error_info,
                        struct continuation *continuation,
                        int allowed_results,
                        struct continuation *parent_continuation,
                        int parent_allowed_results);

/* Function for creating a new frame that has the same parent as
   argument "f", and that shares any bindings with "f" that aren't
   shadowed by *formalsp and values.  Advances *formalsp past all
   formal arguments for which values are provided in values.
 */
frame *extend(IC *ic, frame *f, sexpr **formalsp, sexpr *values,
              sexpr *procedure, sexpr *output_error_info,
              struct continuation *continuation,
              int allowed_results,
              struct continuation *parent_continuation,
              int parent_allowed_results);

/* Destructively modifies frame f to include name name.
   Used by LOCAL. */
void add_local(IC *ic, frame *f, sexpr *name);


/* Creates a set of bindings suitable for use in mk_frame.
   New bindings are created using *paramsp as a list of formal
   arguments and args as a list of actual arguments, and these are
   prepended to ending.

   *paramsp is advanced over any formal parameters that are bound.
 */
sexpr *mk_bindings(IC *ic, sexpr **paramsp, sexpr *args, sexpr *ending);

/* Make frame f be the current frame. */
void reroot(IC *ic, frame *f);

/* Syntactic sugar for cons, car, and cdr. */
#define cons(ic, car, cdr) mk_cons(ic, car, cdr, ic->g_nil, ic->g_nil)
#define unsafe_cons(ic, car, cdr) unsafe_mk_cons(ic, car, cdr, ic->g_nil, ic->g_nil)
#define car(X) ((X)->u.cons.car)
#define cdr(X) ((X)->u.cons.cdr)

/* Function for interning a C string as a word. */
sexpr *intern(IC *ic, const char *name);

/* Function for interning a pointer + length as a word.

   If head is NULL and a new word needs to be created, then a new string
   is allocated and len characters are copied from name.

   If head is not NULL and a new word needs to be created, then name is
   assumed to point into head somewhere.  The new word will point to 
   head for garbage collection purposes, and use name and len as is.
 */
sexpr *intern_len(IC *ic, const char *head, const char *name, unsigned int len);

/* Creates garbage collected copy of namestr if namehead is NULL and
   creates an unbound and uninterned symbol using mk_symbol.  Should not
   generally be used outside of list_memory.c.  Only here for one call
   in interpreter.c and one call in global_environment.c.
 */
sexpr *new_name(IC *ic, const char *namehead, const char *namestr, unsigned int len);


/* This is the garbage collection callback that removes cons cells from
   the symbol list if their car has been reset to g_nil.
 */
void cleanup_name_table(GC *g);

/* Used for testing equality of names.
   Being case insensitive, but case preserving, means
   that we need to track multiple names for each symbol.
   For printing, we use the string stored in the NAME.
   For all other purposes (value, procedure, property list, equality)
   we use the SYMBOL.

   Two names are considered the same if they have the same SYMBOL,
   even if they are different NAME objects.
 */
static inline int name_eq(sexpr *a, sexpr *b) {
    struct symbol *syma, *symb;

    if(a == NULL || b == NULL)
        return 0;

    if(a->t == NAME)
        syma = a->u.name.symbol;
    else
        return 0;

    if(b->t == NAME)
        symb = b->u.name.symbol;
    else
        return 0;

    return syma == symb;
}


/* Test whether a sexpr is nil. */
#define is_nil(ic, e) ((e) == ((ic)->g_nil))

/* Converts e to a NAME object. */
sexpr *to_name(IC *ic, sexpr *e);

/* Converts e to a NUMBER object. */
sexpr *to_number(IC *ic, sexpr *e);


/* Do nothing marking function for use when creating garbage collected
   cstrings.  Does nothing because strings do not contain pointers to
   other garbage collected objects. 
 */

void mark_cstring(GC *g, void *c, object_marker om, weak_pointer_registerer);

/* Creates a NULL terminated C string from the contents of the NAME name */
char *get_cstring(IC *ic, sexpr *name);


/* Manipulate property lists */
void pprop(IC *ic, sexpr *name, sexpr *prop, sexpr *value);
sexpr *gprop(IC *ic, sexpr *name, sexpr *prop);
void remprop(IC *ic, sexpr *name, sexpr *prop);
sexpr *plist(IC *ic, sexpr *name);


/* The symbol structure contains all of the information associated with
   a word except for the actual characters in the word.

   Multiple NAME sexprs can point to the same symbol structure.
   For example, "foo, "Foo, and "FOO are all different NAME's, and will
   each print with its own capitalization, but they all refer to the
   same variable, procedure, and property list.
 */
struct symbol {
    struct sexpr *value;              /* The current value of this symbol */
    struct sexpr *function;           /* The procedure for this symbol */
    struct sexpr *function_source;    /* The source code for this procedure */
    struct sexpr *properties;         /* property list */
    struct sexpr *canonical_name;     /* A lower case NAME */
    struct sexpr *names;              /* A list of all NAMES */
    int flags;                        /* A bitfield - See below */
};

/* Burried procedures, variables, and property lists will not be
   saved by SAVE.  This allows us to only save user defined procedures
   etc., and to not save values from libraries.
 */
#define PROC_BURIED         01
#define VAL_BURIED          02
#define PLIST_BURIED        04

/* When a procedure is traced, the interpreter will print a message
   when it is called and when it returns.
   When a variable is traced, the interpreter will print a message
   when it is modified with MAKE.
   When a property list is traced, the interpreter will print a message
   when it is modified with PPROP.
 */
#define PROC_TRACED         010
#define VAL_TRACED          020
#define PLIST_TRACED        040

/* When a procedure is stepped, the interpreter will print every line
   before executing it and wait for the user to press a key.
   It is meaningless for variables or property lists to be stepped,
   but the flags exist because several functions accept flags in threes
   and it is easier to create these flags and do nothing with them than
   to write the special case code necessary to handle stepping separately
   from tracing and burying.
 */
#define PROC_STEPPED        0100
#define VAL_STEPPED         0200
#define PLIST_STEPPED       0400



struct symbol *mk_symbol(IC *ic,
                        struct sexpr *value,
                        struct sexpr *function,
                        struct sexpr *function_source,
                        struct sexpr *properties,
                        struct sexpr *canonical_name,
                        struct sexpr *names,
                        int flags);

/* Creates an ARRAY.  Used by the ARRAY command and list_to_array() */
sexpr *array(IC *ic, int length, int origin);

/* Converts a list into an ARRAY.  Used by the reader to process
   { ... } notation. */
sexpr *list_to_array(IC *ic, sexpr *l, int origin);


/* Returns 1 if e is a number or a name that can be turned into a number.
   Returns 0 otherwise. */
int numberp(IC *ic, sexpr *e);

#endif
