
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

#ifndef INTERPRETER_H
#define INTERPRETER_H
#include <stdio.h>
#include <setjmp.h>
#include "gc.h"

/* Bits in allowed_results */
#define VALUE_OK    1 /* The current expression may return a value */
#define NO_VALUE_OK 2 /* The current expression may return no value */
#define VALUE_MASK  3 /* Bit mask for accessing just VALUE_OK and NO_VALUE_OK.
                         Currently unused. */

#define OUTPUT_OK   4  /* The current procedure may OUTPUT a value */
#define STOP_OK     8  /* The current procedure may STOP or run off the end */
#define OUTPUT_MASK 12 /* Bit mask used to manipulate VALUE_OK and NO_VALUE_OK
                          while preserving OUTPUT_OK and STOP_OK */

#define NAME_TABLE_HASH_BUCKETS 4096

struct interpreter {
    GC *g; /* Garbage Collector context */

    jmp_buf abort; /* setjmp() target for fatal errors. */

    struct reader *r;      /* Lisp reader object */
    struct logoreader *lr; /* Logo reader object */
    int catch_errors;      /* Unused */
    int debug_eval;
    int debug_apply;

    struct frame *frame;               /* Current frame */
    struct frame *root_frame;          /* Root frame, global environment
                                          when no procedures are running */
    struct continuation *continuation; /* The continuation to apply to the
                                          next computed value */

    unsigned int allowed_results;    /* Bitfield.  See above. */
    struct sexpr *output_error_info; /* Information constantly kept up to date
                                        so we can produce accurate error
                                        error messages when allowed_results
                                        is violated.  This is tricky because
                                        the place where the error occurs may
                                        have been optimized away by a tail
                                        call. */
    struct sexpr *eof;               /* EOF object.  Not interned. */
    struct sexpr *current_primitive; /* The primitive currently executing.
                                        Used to generate "doesn't like"
                                        errors. */
    struct sexpr *current_line;      /* The current line executing. */
    struct sexpr *g_unbound;         /* The value returned to indicate that
                                        no value is being returned. */
    struct sexpr *g_no_value;        /* A special value used only within
                                        the interpreter to signal to
                                        APPLY_C or OPTIONALS_C that they
                                        are not receiving a value. */
    struct sexpr *g_nil;             /* The empty list */

    struct sexpr *open_files;        /* Association list mapping names
                                        to file pointers */
    struct sexpr *output;            /* The current output FILEP */
    struct sexpr *output_name;       /* The NAME associated with the current
                                        output FILEP */
    struct sexpr *input;             /* The current input FILEP */
    struct sexpr *input_name;        /* The NAME associated with the current
                                        input FILEP */
    struct sexpr *dribble;           /* The current dribble FILEP */
    struct sexpr *dribble_name;      /* The NAME associated with the current
                                        dribble FILEP */

    struct sexpr *error_byte_buffer; /* The byte_bufer used to accumulate
                                        error messages in using eprintf
                                        and eprint_sexpr */

    /* The global table of interned names.  Each entry is a list. */
    struct sexpr *name_table[NAME_TABLE_HASH_BUCKETS];

    struct sexpr *n_empty;           /* The empty word */


    /* Other names that need to be accessed from C */
    struct sexpr *n_lambda;
    struct sexpr *n_callcc;
    struct sexpr *n_internal_callcc;
    struct sexpr *n_quote;
    struct sexpr *n_thingquote;
    struct sexpr *n_if;
    struct sexpr *n_ifelse;
    struct sexpr *n_begin;
    struct sexpr *n_macro;
    struct sexpr *n_dot_macro;
    struct sexpr *n_eval;
    struct sexpr *n_quasiquote;
    struct sexpr *n_unquote;
    struct sexpr *n_unquote_splicing;
    struct sexpr *n_list;
    struct sexpr *n_rest;
    struct sexpr *n_append;
    struct sexpr *n_car;
    struct sexpr *n_invoke;
    struct sexpr *n_internal_invoke;
    struct sexpr *n_internal_apply;
    struct sexpr *n_output;
    struct sexpr *n_op;
    struct sexpr *n_stop;
    struct sexpr *n_plus;
    struct sexpr *n_minus;
    struct sexpr *n_star;
    struct sexpr *n_slash;
    struct sexpr *n_equal;
    struct sexpr *n_lparen;
    struct sexpr *n_rparen;
    struct sexpr *n_lbracket;
    struct sexpr *n_rbracket;
    struct sexpr *n_lbrace;
    struct sexpr *n_rbrace;
    struct sexpr *n_newline;
    struct sexpr *n_atsign;
    struct sexpr *n_run;
    struct sexpr *n_beginresult;
    struct sexpr *n_beginnoresult;
    struct sexpr *n_to;
    struct sexpr *n_lt;
    struct sexpr *n_le;
    struct sexpr *n_gt;
    struct sexpr *n_ge;
    struct sexpr *n_ne;
    struct sexpr *n_true;
    struct sexpr *n_false;
    struct sexpr *n_maybeoutput;
    struct sexpr *n_erract;
    struct sexpr *n_error;
    struct sexpr *n_exception_catchers;
    struct sexpr *n_error_catcher;
    struct sexpr *n_lisp_macro;
    struct sexpr *n_logo_macro;
    struct sexpr *n_fullprintp;
    struct sexpr *n_upstack_output;
    struct sexpr *n_pause;
    struct sexpr *n_pause_caller;
    struct sexpr *n_caseignoredp;
    struct sexpr *n_treeify;
    struct sexpr *n_treeify_cache_generation;
    struct sexpr *n_logoversion;
    struct sexpr *n_create_logo_procedure;
    struct sexpr *n_create_logo_macro;
    struct sexpr *n_template_number;
    struct sexpr *n_q1;
    struct sexpr *n_q2;
    struct sexpr *n_q3;
    struct sexpr *n_q4;
    struct sexpr *n_q5;
    struct sexpr *n_q6;
    struct sexpr *n_q7;
    struct sexpr *n_q8;
    struct sexpr *n_q9;
    struct sexpr *n_q10;
};
typedef struct interpreter IC; /* Interpreter Context */

/* Macro to run a macro on all of the names in the interpreter
   context.  Used in interpreter.c to initialize them to interned
   values. */
#define FOR_INTERPRETER_NAMES(MACRO) \
    MACRO(lambda) \
    MACRO(callcc) \
    MACRO(internal_callcc) \
    MACRO(quote) \
    MACRO(thingquote) \
    MACRO(if) \
    MACRO(ifelse) \
    MACRO(begin) \
    MACRO(macro) \
    MACRO(dot_macro) \
    MACRO(eval) \
    MACRO(quasiquote) \
    MACRO(unquote) \
    MACRO(unquote_splicing) \
    MACRO(list) \
    MACRO(rest) \
    MACRO(append) \
    MACRO(car) \
    MACRO(invoke) \
    MACRO(internal_invoke) \
    MACRO(internal_apply) \
    MACRO(output) \
    MACRO(op) \
    MACRO(stop) \
    MACRO(plus) \
    MACRO(minus) \
    MACRO(star) \
    MACRO(slash) \
    MACRO(equal) \
    MACRO(lparen) \
    MACRO(rparen) \
    MACRO(lbracket) \
    MACRO(rbracket) \
    MACRO(lbrace) \
    MACRO(rbrace) \
    MACRO(newline) \
    MACRO(atsign) \
    MACRO(run) \
    MACRO(beginresult) \
    MACRO(beginnoresult) \
    MACRO(to) \
    MACRO(lt) \
    MACRO(le) \
    MACRO(gt) \
    MACRO(ge) \
    MACRO(ne) \
    MACRO(true) \
    MACRO(false) \
    MACRO(maybeoutput) \
    MACRO(erract) \
    MACRO(error) \
    MACRO(exception_catchers) \
    MACRO(error_catcher) \
    MACRO(lisp_macro) \
    MACRO(logo_macro) \
    MACRO(fullprintp) \
    MACRO(upstack_output) \
    MACRO(pause) \
    MACRO(pause_caller) \
    MACRO(caseignoredp) \
    MACRO(treeify) \
    MACRO(treeify_cache_generation) \
    MACRO(logoversion) \
    MACRO(create_logo_procedure) \
    MACRO(create_logo_macro) \
    MACRO(template_number) \
    MACRO(q1) \
    MACRO(q2) \
    MACRO(q3) \
    MACRO(q4) \
    MACRO(q5) \
    MACRO(q6) \
    MACRO(q7) \
    MACRO(q8) \
    MACRO(q9) \
    MACRO(q10)

/* Macro to run a macro on all of the interpreter fields.
   used in interpreter.c to mark the root set during garbage
   collection. */
#define FOR_INTERPRETER_FIELDS(MACRO) \
    MACRO(r) \
    MACRO(lr) \
    MACRO(frame) \
    MACRO(continuation) \
    MACRO(output_error_info) \
    MACRO(eof) \
    MACRO(current_primitive) \
    MACRO(current_line) \
    MACRO(g_unbound) \
    MACRO(g_no_value) \
    MACRO(g_nil) \
    MACRO(open_files) \
    MACRO(output) \
    MACRO(output_name) \
    MACRO(input) \
    MACRO(input_name) \
    MACRO(dribble) \
    MACRO(dribble_name) \
    MACRO(error_byte_buffer) \
    MACRO(n_empty) \
    MACRO(n_lambda) \
    MACRO(n_callcc) \
    MACRO(n_internal_callcc) \
    MACRO(n_quote) \
    MACRO(n_thingquote) \
    MACRO(n_if) \
    MACRO(n_ifelse) \
    MACRO(n_begin) \
    MACRO(n_macro) \
    MACRO(n_dot_macro) \
    MACRO(n_eval) \
    MACRO(n_quasiquote) \
    MACRO(n_unquote) \
    MACRO(n_unquote_splicing) \
    MACRO(n_list) \
    MACRO(n_rest) \
    MACRO(n_append) \
    MACRO(n_car) \
    MACRO(n_invoke) \
    MACRO(n_internal_invoke) \
    MACRO(n_internal_apply) \
    MACRO(n_output) \
    MACRO(n_op) \
    MACRO(n_stop) \
    MACRO(n_plus) \
    MACRO(n_minus) \
    MACRO(n_star) \
    MACRO(n_slash) \
    MACRO(n_equal) \
    MACRO(n_lparen) \
    MACRO(n_rparen) \
    MACRO(n_lbracket) \
    MACRO(n_rbracket) \
    MACRO(n_lbrace) \
    MACRO(n_rbrace) \
    MACRO(n_newline) \
    MACRO(n_atsign) \
    MACRO(n_run) \
    MACRO(n_beginresult) \
    MACRO(n_beginnoresult) \
    MACRO(n_to) \
    MACRO(n_lt) \
    MACRO(n_le) \
    MACRO(n_gt) \
    MACRO(n_ge) \
    MACRO(n_ne) \
    MACRO(n_true) \
    MACRO(n_false) \
    MACRO(n_maybeoutput) \
    MACRO(n_erract) \
    MACRO(n_error) \
    MACRO(n_exception_catchers) \
    MACRO(n_error_catcher) \
    MACRO(n_lisp_macro) \
    MACRO(n_logo_macro) \
    MACRO(n_fullprintp) \
    MACRO(n_upstack_output) \
    MACRO(n_pause) \
    MACRO(n_pause_caller) \
    MACRO(n_caseignoredp) \
    MACRO(n_treeify) \
    MACRO(n_treeify_cache_generation) \
    MACRO(n_logoversion) \
    MACRO(n_create_logo_procedure) \
    MACRO(n_create_logo_macro) \
    MACRO(n_template_number) \
    MACRO(n_q1) \
    MACRO(n_q2) \
    MACRO(n_q3) \
    MACRO(n_q4) \
    MACRO(n_q5) \
    MACRO(n_q6) \
    MACRO(n_q7) \
    MACRO(n_q8) \
    MACRO(n_q9) \
    MACRO(n_q10)


/* Makes an interpreter.  gc_delay is how many allocations to perform
   between garbage collections when in stop the world mode. */
IC *mk_interpreter(int gc_delay);

/* Frees up the interpreter and all garbage collected objects created
   during interpretation. */
void free_interpreter(IC *);

/* Evaluates an expression.  top_allowed_results is a bitfield with the
   same meaning as ic->allowed_results. */
struct sexpr *eval(IC *ic, struct sexpr *expr, int top_allowed_results);

/* Prints a stack trace.  Sometimes useful for debugging, but not very often.
   Lots of information gets thrown away by tail call optimization. */
void print_stacktrace(IC *ic);

/* Finds information about an operator.  Not ordinarily used outside of
   interpreter.c.  It is only here because treeify uses this to figure out
   the arity of operators. */
int get_apply_parts(IC *ic,
                    struct sexpr *e,
                    struct sexpr **procp,
                    struct sexpr **funargp,
                    struct sexpr **macrop,
                    struct sexpr **operp,
                    int print_errors);

/* Prints out an error message and longjmp's to an exit from eval() */
#ifdef __GNUC__
__attribute__((noreturn))
#endif
void throw_error(IC *ic, struct sexpr *line);

/* Prints out an error message and longjmp's to an exit from eval().
   The interpreter will exit, and this cannot be caught with ERRACT or
   CATCH "ERROR [...]
 */
#ifdef __GNUC__
__attribute__((noreturn))
#endif
void fatal_error(IC *ic, struct sexpr *line);

/* Used inside of primitives to generate a "doesn't like" error message */
#ifdef __GNUC__
__attribute__((noreturn))
#endif
void bad_argument(IC *ic, struct sexpr *arg);

/* Used as a flag to allow signal handlers to communicate to the interpreter
   that interpretation needs to be terminated. */
extern int interrupted;

/* Used as a flag to allow signal handlers to communicate to the interpreter
   that interpretation needs to be paused. */
extern int paused;

#endif

