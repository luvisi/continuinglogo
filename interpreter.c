
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

#include "list_memory.h"
#include "gc.h"
#include "interpreter.h"
#include "global_environment.h"
#include "reader.h"
#include "logoreader.h"
#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <setjmp.h>

/* Used as arguments to longjmp() to indicate whether eval() should
   process ERRACT or CATCH "ERROR [...] actions.  CATCHABLE_ERROR's
   can trigger ERRACT or be caught with CATCH "ERROR [...] while
   FATAL_ERROR's will always terminate the interpreter.
 */
enum error_type { NO_ERROR = 0, CATCHABLE_ERROR, FATAL_ERROR };

/* Used as a way for the SIGINT handler to communicate to the interpreter
   that the user pressed control-c. */
int interrupted = 0;

/* Used as a way for the SIGINT handler to communicate to the interpreter
   that the user pressed control-\. */
int paused = 0;

/* Marks the root set (the interpreter context).
   The higher order macro is so that the long lists of structure
   members can all be kept together in interpreter.h, which makes
   it easier to keep them in sync.
 */
void mark_interpreter(GC *g, void *c,
                      object_marker om, weak_pointer_registerer wpr) {
    IC *ic = (IC *) c;

#define MARK_INTERPRETER_FIELD(field) om(ic->g, ic->field);

    FOR_INTERPRETER_FIELDS(MARK_INTERPRETER_FIELD)
}

/* Create an interpreter context. */
IC *mk_interpreter(int gc_delay) {
    IC *ic = (IC *) malloc(sizeof(IC));
    if(ic == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(EXIT_FAILURE);
    }

    /* The new garbage collector will not attempt to collect garbage
       until activated below.  This means that we can use the garbage
       collector's allocation routines to fill in the fields of ic
       without worrying about a garbage collection running whle some
       of them are uninitialized.
     */
    GC *g = create_gc(mark_interpreter, ic, gc_delay);
    if(g == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(EXIT_FAILURE);
    }

    ic->g = g;

    ic->r = mk_reader(ic);
    ic->lr = mk_logoreader(ic);

    ic->catch_errors = 0;
    ic->debug_eval = 0;
    ic->debug_apply = 0;

    ic->g_unbound = mk_unbound(ic);   /* UNBOUND is its own type.
                                         That's why this is g_ and not n_ */
    ic->g_no_value = mk_no_value(ic); /* Same for NO_VALUE */
    ic->g_nil = mk_empty_list(ic);

    /* open_files is an association list mapping NAME's to
       FILEP's of open files.  Each element of the list is a
       CONS where the car is a NAME and the cdr is a FILEP.
     */
    ic->open_files = ic->g_nil;

    /* The output file is stdout by default, which is refered to in
       Logo programs as the empty list [] */
    ic->output = mk_filep(ic, stdout);
    ic->output_name = ic->g_nil;

    /* The input file is stdin by default, which is refered to in
       Logo programs as the empty list [] */
    ic->input = mk_filep(ic, stdin);
    ic->input_name = ic->g_nil;

    /* The dribble file gets a transcript of everything written to the
       terminal.  There is no dribble by default, and this state is
       referred to in Logo programs as the empty list [] */
    ic->dribble = ic->g_nil;
    ic->dribble_name = ic->g_nil;

    /* The byte_buffer used to accumulate error messages using eprintf
       and eprint_sexpr. */
    ic->error_byte_buffer =
        mk_byte_bufferp(ic, mk_byte_buffer(ic));

    /* This is the value that cars in the name list will be reset to
       if they point to a name that has no value, procedure, or
       property list and are not referenced from anywhere else in the
       program.
     */
    set_gc_default_value(ic->g, ic->g_nil);

    /* This is the callback that takes cons cells out of the name list
       that have had their cars set to ic->g_unbound.
     */
    set_gc_post_collection_callback(ic->g, cleanup_name_table);


    ic->allowed_results = 0;

    /* Dummy values so that garbage collection doesn't choke and
       errors that occur before info is set don't result in invalid
       memory accesses. */
    ic->output_error_info = mk_output_error_info(ic, ic->g_nil,
                                                     ic->g_nil,
                                                     mk_line(ic, ic->g_nil,
                                                                 ic->g_nil,
                                                                 ic->g_nil));
    ic->eof = new_name(ic, NULL, "#<EOF>", 6); /* Uninterned */
    ic->current_primitive = ic->g_nil;
    ic->current_line = mk_line(ic, ic->g_nil, ic->g_nil, ic->g_nil);
    ic->name_list = cons(ic, ic->g_nil, ic->g_nil);

    ic->frame = mk_frame(ic, NULL, ic->g_nil, ic->g_nil, 
                             ic->output_error_info, NULL, 0, NULL, 0);
    ic->root_frame = ic->frame;
    ic->continuation = NULL;

    /* Initialize most global symbols by interning their stringified
       name.  The purpose of the higher order macro here is to keep all
       three long lists of interpreter context names together in
       interpreter.h, where they are easier to keep in sync. */
#define GLOBAL_NAME(sym) ic->n_ ## sym = intern(ic, # sym );

    FOR_INTERPRETER_NAMES(GLOBAL_NAME)

    /* Some global names I actually want to refer to a name that is
       different than the stringified version of the field name.
       For example, I don't want ic->n_empty to refer to the word
       "empty, I want it to refer to the empty word.
     */
    ic->n_empty       = intern(ic, "");
    ic->n_plus        = intern(ic, "+");
    ic->n_minus       = intern(ic, "-");
    ic->n_star        = intern(ic, "*");
    ic->n_slash       = intern(ic, "/");
    ic->n_equal       = intern(ic, "=");
    ic->n_lparen      = intern(ic, "(");
    ic->n_rparen      = intern(ic, ")");
    ic->n_lbracket    = intern(ic, "[");
    ic->n_rbracket    = intern(ic, "]");
    ic->n_lbrace      = intern(ic, "{");
    ic->n_rbrace      = intern(ic, "}");
    ic->n_newline     = intern(ic, "\n");
    ic->n_atsign      = intern(ic, "@");
    ic->n_lt          = intern(ic, "<");
    ic->n_le          = intern(ic, "<=");
    ic->n_gt          = intern(ic, ">");
    ic->n_ge          = intern(ic, ">=");
    ic->n_ne          = intern(ic, "<>");
    ic->n_maybeoutput = intern(ic, ".maybeoutput");
    ic->n_dot_macro   = intern(ic, ".macro");

    /* This adds all of the primitive procedures. */
    initialize_global_environment(ic);

    /* Now we can enable the garbage collection because all pointers
       in the root set have valid values. */
    enable_gc(g);

    return ic;
}


/* Free an interpreter.
   There is no need to free the reader or logoreader because they
   were allocated by the garbage collector. */
void free_interpreter(IC *ic) {
    free_gc(ic->g);
    free(ic);
}

sexpr *reverse(IC *ic, sexpr *l) {
    sexpr *ret = ic->g_nil;

    while(!is_nil(ic, l)) {
        ret = cons(ic, car(l), ret);
        l = cdr(l);
    }
    return ret;
}

int length(IC *ic, sexpr *e) {
    int len = 0;

    while(!is_nil(ic, e) && e->t == CONS) {
        len++;
        e = cdr(e);
    }

    if(!is_nil(ic, e)) {
        fprintf(stderr, "Malformed argument to length!\n");
        exit(EXIT_FAILURE);
    }

    return len;
}

/* Given an operator, e, tries to figure out:
      The PROC, if any, which contains the minimum, default, and maximum args
      for the operator.

      The FUNARG, if any, which contains the frame in which to execute the
      operator.

      The MACRO, if any, which is a marker that this operator is a macro.

      The actual operator which will be applied to the arguments.
 */
int get_apply_parts(IC *ic,
                    sexpr *e,
                    sexpr **procp,
                    sexpr **funargp,
                    sexpr **macrop,
                    sexpr **operp,
                    int print_errors) {
    sexpr *original_e = e;
    *procp = ic->g_nil;
    *funargp = ic->g_nil;
    *macrop = ic->g_nil;
    *operp = ic->g_nil;
    for(;;) {
        if(e->t == NAME) {
            if(e->u.name.symbol->function == ic->g_unbound) {
                if(print_errors) {
                    eprintf(ic, "I don't know how to ");
                    eprint_sexpr(ic, e);
                }
                return 0;
            } else {
                e = e->u.name.symbol->function;
            }
        } else if(e->t == PROC) {
            *procp = e;
            e = e->u.proc.proc;
        } else if(e->t == FUNARG) {
            *funargp = e;
            e = e->u.funarg.lfun;
        } else if(e->t == MACRO) {
            *macrop = e;
            e = e->u.macro.expander;
        } else if(e->t == CONS &&
                  car(e) == ic->n_lambda &&
                  !is_nil(ic, cdr(e)) &&
                  (is_nil(ic, car(cdr(e))) ||
                   car(cdr(e))->t == CONS)) {
            *operp = e;
            return 1;
        } else if(e->t == SUBR || e->t == FSUBR || e->t == CONTINUATION) {
            *operp = e;
            return 1;
        } else {
            if(print_errors) {
                eprintf(ic, "I don't know how to ");
                eprint_sexpr(ic, original_e);
            }
            return 0;
        }
    }
}

/* Given an argument list, figures out the minimum, default, and
   maximum number of arguments for the procedure.
 */
void arg_counts(IC *ic, sexpr *e, int *minp, int *defp, int *maxp) {
    sexpr *args = car(cdr(e));
    sexpr *orig_args = args;

    *minp = *defp = *maxp = 0;

    while(!is_nil(ic, args) && car(args)->t == NAME) {
        /* Required arguments, symbols in the lambda list */
        *minp += 1;
        *defp += 1;
        *maxp += 1;
        args = cdr(args);
    }

    while(!is_nil(ic, args) &&
          car(args)->t == CONS &&
          car(car(args))->t == NAME &&
          !is_nil(ic, cdr(car(args)))) {
        /* Optional arguments, two element lists.
           (<symbol> <default value>) */
        *maxp += 1;
        args = cdr(args);
    }

    if(!is_nil(ic, args) &&
       car(args)->t == CONS &&
       car(car(args))->t == NAME &&
       is_nil(ic, cdr(car(args)))) {
        /* Rest argument: (<symbol>) */
        *maxp = INT_MAX;
        args = cdr(args);
    }

    if(!is_nil(ic, args) && car(args)->t == NUMBER) {
        *defp = (int) (car(args)->u.number.value);
        args = cdr(args);
    }

    if(!is_nil(ic, args)) {
        eprintf(ic, "Malformed argument list: ");
        eprint_sexpr(ic, orig_args);
        throw_error(ic, ic->current_line);
    }

    return;
}

/* Given a lambda expression, wraps it in a PROC object that contains
   the minimum, default, and maximum number of arguments. */
sexpr *lambda_to_proc(IC *ic, sexpr *e) {
    int minargs, defargs, maxargs;

    arg_counts(ic, e, &minargs, &defargs, &maxargs);

    return mk_proc(ic, e, minargs, defargs, maxargs);
}

/* Signals a fatal error when an expression returns a value when
   it is not allowed to do so, or when a procedure outputs a value when
   it is not allowed to do so.
 */
#ifdef __GNUC__
__attribute__((noreturn))
#endif
void dont_say(IC *ic, sexpr *output_error_info, sexpr *value) {
    eprintf(ic, "You don't say what to do with ");
    eprint_sexpr(ic, value);
    throw_error(ic, output_error_info->u.output_error_info.line);
}

/* Signals a fatal error when an expression fails to return a required
   value, or when a procedure fails to output a required value.
 */
#ifdef __GNUC__
__attribute__((noreturn))
#endif
void didnt_output(IC *ic, sexpr *output_error_info) {
    eprint_sexpr(ic, output_error_info->u.output_error_info.to_blame);
    eprintf(ic, " didn't output to ");
    eprint_sexpr(ic, output_error_info->u.output_error_info.wanting);
    throw_error(ic, output_error_info->u.output_error_info.line);
}

/* eval() takes an Interpreter Context ic and an expression expr to evaluate.
   The return value is the result of evaluating expr.

   This is essentially an interpreter for a lisp2 with dynamic scope,
   case insensitive but case preserving symbols, and a statement/expression
   distinction.  Lines of Logo code are converted into lisp by treeify() in
   treeify.c.  The input to eval() generally comes either from initialize.txt
   (raw lisp read by reader.c) or from logo lines read by readline in
   logoreader.c and turned into lisp by treeify() in treeify.c.

   In order to make continuations work properly in all situations, eval()
   must never be called recursively.

   eval() is made up of two sections, both headed with labeled and targeted
   by many goto statements.

   The eval: section is for evaluating an expression.
   The apply: section is for applying a continuation to a value.

   Inputs to eval: are:
           expr                      The expression being evaluated

           ic->allowed_results       Bitmask describing allowed ways
                                     for the current expression to
                                     terminate.  Used to detect incorrect
                                     use of an expression versus a statement.
                                     Bits are:
                                         OUTPUT_OK      The current procedure
                                                        is allowed to OUTPUT
                                                        a value.
                                         STOP_OK        The current procedure
                                                        is allowed to STOP
                                                        or run off the end
                                                        without outputting
                                                        a value.
                                         VALUE_OK       The current expression
                                                        is allowed to output
                                                        a value (normally
                                                        as an argument to
                                                        another procedure).
                                         NO_VALUE_OK    The current expression
                                                        is allowed to not
                                                        output a value.

                                     There is a special value, OUTPUT_MASK,
                                     which contains both OUTPUT_OK and STOP_OK.
                                     It is used to preserve the record of
                                     what the procedure is allowed to do while
                                     manipulating the allowed behavior for
                                     expressions.

           ic->output_error_info     Information about the current line,
                                     procedure call waiting for arguments
                                     to be evaluated, and procedure
                                     generating an output.  Used to create
                                     error messages.

           ic->current_line          The LINE object containing the procedure,
                                     the raw line, and the parsed line
                                     currently executing.

           tail                      A flag indicating whether the current
                                     expression is in a tail context.

           The current environment   The values of all symbols.

           ic->frame                 The current environment frame.

   Inputs to apply: are:
           value                     The value resulting from an evaluation.

           ic->continuation          The continuation to which the value
                                     is being returned.

    throw_error(), didnt_output(), and dont_say() are functions
    that print error messages and terminate the current interpretation.
    The current implementations never return.

    expr, value, and values are reassigned so often that it is easiest
    to keep them in the garbage collection root set.  protect_ptr()
    does this.  unprotect_ptr() unprotects the most recent pointer that
    was protected (stack style).

    There is only one return statement in all of eval().  It occurs when
    apply: handles a RETURN_C type continuation.  This is also where
    expr, value, and values get removed from the root set with three calls
    to unprotect_ptr().

    Jumps to eval: from within eval: can often reuse much of the existing
    environment.

    Jumps to apply: from either eval: or apply: just need to make sure that
    value and ic->continuation are set correctly.

    Jumps to eval: from apply: require restoring all of the inputs to eval:.
    Unfortunately, there are subtle differences in how this is done from
    different points, so I have not yet figured out how to standardize this
    as a macro or some such.

    "Not returning a value" is implemented by returning an sexpr of
    type UNBOUND.  There is only one global object of this type in the
    interpreter, and it is stored in ic->g_unbound.

    NO_VALUE, of which there is also only one global object called
    ic->g_no_value, is a hack for informing APPLY_C and OPTIONALS_C
    continuations that they are not receiving a value.

    The STORE() macro is the write barrier for the garbage collector to
    enable incremental garbage collection.  It must be used whenever
    storing a pointer to a garbage collected object into the root set
    or into another garbage collected object.
    The arguments are:
        The garbage collection context.
        The object being stored into (NULL for the interpreter context or
                                      for protected pointers like expr, value,
                                      or values)
        The lvalue of the pointer being modified.
        The pointer value being stored.
 */

sexpr *eval(IC *ic, sexpr *expr, int top_allowed_results) {

    protect_ptr(ic->g, (void **)&expr);

    int tail = 0;

    STORE(ic->g, NULL, 
          ic->continuation,
          mk_return_c(ic, NULL,
                          ic->frame,
                          expr,
                          ic->current_line,
                          ic->allowed_results,
                          ic->output_error_info,
                          0));

    sexpr *value = ic->g_nil;
    protect_ptr(ic->g, (void **)&value);

    /* Continuation for escaping out of the top level. */
    STORE(ic->g, ic->frame, ic->frame->continuation, ic->continuation);

    /* Used as a temporary variable when applying an apply_c continuation */
    sexpr *values = ic->g_nil;
    protect_ptr(ic->g, (void **)&values);

    /* Tracks valid ways to terminate (OUTPUT, STOP, end a begin
       returning something, end a begin without returning something). */
    ic->allowed_results = top_allowed_results;

    /* Tracks information used to generate an error message when an
       invalid termination occurs. */
    STORE(ic->g, NULL, ic->output_error_info,
                       mk_output_error_info(ic, ic->g_nil,
                                                ic->g_nil,
                                                mk_line(ic, ic->g_nil,
                                                            ic->g_nil,
                                                            ic->g_nil)));


    /* Evil hack to get the garbage collector back into a sane state
       after an error exit.  */
    int stored_protect_count = ic->g->protect_count;
    int stored_protect_ptr_count = ic->g->protect_ptr_count;

    /* Set at eval: and apply:.  Used to figure out whether to restore
       the environment from the continuation or not when running
       ERRACT. */
    enum { EVAL, APPLY } state = EVAL;
    enum error_type error_type;
    error_type = setjmp(ic->abort);
    if(error_type != NO_ERROR) {
        /* throw_error() jumps back here when an evaluation
           is being aborted.
           Reset the protection counts in the garbage collector, return
           no value, and jump into the handler for RETURN_C continuations.
         */
        ic->g->protect_count = stored_protect_count;
        ic->g->protect_ptr_count = stored_protect_ptr_count;
        STORE(ic->g, NULL, value, ic->g_unbound);

        if(error_type == CATCHABLE_ERROR) {
            /* ERRACT is a list of instructions to run in the event of an
               error.
             */
            if(ic->n_erract->u.name.symbol->value != ic->g_unbound) {
                STORE(ic->g, NULL,
                      expr,
                      cons(ic, ic->n_run,
                               cons(ic, ic->n_erract->u.name.symbol->value,
                                        ic->g_nil)));
                switch(state) {
                    case APPLY:
                        /* If we were in an APPLY state, then the arguments
                           to eval: need to be reset before jumping to eval:
                         */
                        ic->allowed_results = ic->continuation->allowed_results;
                        STORE(ic->g, NULL, ic->output_error_info,
                                           ic->continuation->output_error_info);
                        STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                        tail = ic->continuation->tail;
                        STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                        reroot(ic, ic->frame);
                        STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                    case EVAL:
                        goto eval;
                }
            }

            if(ic->n_error_catcher->u.name.symbol->value != 
               ic->g_unbound) {
                struct continuation *catcher =
                    ic->n_error_catcher->
                        u.name.symbol->
                        value->
                        u.proc.proc->
                        u.continuation.parent;
                /* There's an error catcher to return to.  Return to it
                   with an unbound value */
                STORE(ic->g, NULL, value, ic->g_unbound);
                STORE(ic->g, NULL, ic->continuation, catcher);
                goto apply;
            }
        }

        /* Jumping into the normal return handler feels slightly less
           dirty than duplicating the unprotect_* calls and having
           two return statements.  This way eval() still only has one
           official exit point. */
        goto exit;
    }


    eval:
        if(interrupted) {
            /* The global interrupted can be set by a signal handler
               to indicate that interpretation should terminate. */
            interrupted = 0;
            eprintf(ic, "User interrupt");
            fatal_error(ic, ic->current_line);
        }

        if(paused) {
            sexpr *pause_call;
            /* The user pressed control-\ */
            paused = 0;

            pause_call = protect(ic->g, 
                     cons(ic, ic->n_pause_caller, 
                              cons(ic, cons(ic, ic->n_quote,
                                                cons(ic, ic->current_line->u.line.procedure, ic->g_nil)),
                                       ic->g_nil)));
            STORE(ic->g, NULL,
                  expr,
                  cons(ic, ic->n_begin,
                           cons(ic, pause_call,
                                    cons(ic, expr, ic->g_nil))));
            unprotect(ic->g);
        }

        state = EVAL;

        if(ic->debug_eval) {
            eprintf(ic, "Eval (%d): ", ic->allowed_results);
            eprint_sexpr(ic, expr);
            eprintf(ic, "\n");
        }

        switch(expr->t) {
            case UNBOUND:
                /* An UNBOUND value indicates that nothing is being
                   returned.  This is only acceptable if NO_VALUE_OK
                   is set. */
                if(!(ic->allowed_results & NO_VALUE_OK))
                    didnt_output(ic, ic->output_error_info);
                STORE(ic->g, NULL, value, expr);
                goto apply;

            case NO_VALUE:
            case SUBR:
            case FSUBR:
            case FUNARG:
            case MACRO:
            case CONTINUATION:
            case OUTPUT_ERROR_INFO:
            case NUMBER:
            case PROC:
            case FILEP:
            case BYTE_BUFFERP:
            case EMPTY_LIST:
            case ARRAY:
                /* Other self-evaluating types are actual values, so they
                   can only be returned in a context where VALUE_OK is
                   set. */
                /* ic->continuation is not adjusted here but is sent to
                   apply: as is to indicate that the value is to be
                   returned to the parent expression. */
                if(!(ic->allowed_results & VALUE_OK))
                    dont_say(ic, ic->output_error_info, expr);
                STORE(ic->g, NULL, value, expr);
                goto apply;

            case LINE:
                /* LINE objects are a special case.  We allow them to
                   self evaluate no matter what ic->allowed_results says.
                   It is by returning LINE objects to a BEGIN_C continuation
                   that ic->current_line gets updated, so we want that to 
                   happen no matter the context in which (begin ...) was
                   called. */
                if(expr->u.line.procedure->t == NAME &&
                   (expr->u.line.procedure->u.name.symbol->flags &
                    PROC_STEPPED)) {
                    /* If the procedure for this line is being stepped,
                       we print the line and pause for the user to
                       press return.
                     */
                    tprint_sexpr(ic, expr->u.line.parsed_line);
                    tprintf(ic, " >>> ");
                    readrawline(ic->lr);
                }

                STORE(ic->g, NULL, value, expr);
                goto apply;

            case NAME:
                /* NAME objects are the primary representations of
                   Logo words.  NAME's contain a string and a reference
                   to a SYMBOL.  Multiple NAME's can refer to the same
                   SYMBOL, usually because they are spelled with different
                   capitalization, but they will all refer to the same
                   procedure, value, and property list, because those
                   are stored in the SYMBOL.  We use name_eq() for comparing
                   NAME's so that two NAME's that refer to the same
                   SYMBOL will register as equal. */
                if(ic->debug_eval) eprintf(ic, "    name\n");
                if(name_eq(expr, ic->n_stop)) {
                    if(!(ic->allowed_results & STOP_OK)) {
                        didnt_output(ic, ic->output_error_info);
                    }

                    /* In the event of a STOP expression, the continuation
                       from ic->frame is used to escape from the current
                       procedure rather than just outputing a value to
                       the parent expression. */

                    STORE(ic->g, NULL, value, ic->g_unbound);
                    STORE(ic->g, NULL, ic->continuation,
                                       ic->frame->continuation);
                    goto apply;
                }
                STORE(ic->g, NULL, value, expr->u.name.symbol->value);
                if(value->t == UNBOUND) {
                    eprint_sexpr(ic, expr);
                    eprintf(ic, " has no value");
                    throw_error(ic, ic->current_line);
                }
                if(!(ic->allowed_results & VALUE_OK))
                    dont_say(ic, ic->output_error_info, expr);
                goto apply;


            case CONS:
                /* We are attempting to eval: a list.  Check special forms,
                   and then assume it's a procedure call if none match. */
                if(ic->debug_eval) eprintf(ic, "    cons\n");
                if(car(expr) == ic->n_lambda) {
                    /* lambda_to_proc() creates a PROC object which records
                       the lambda expression to be used as an operator along
                       with the minimum, default, and maximum argument counts.
                       This information is necessary in order for treeify()
                       to properly parse Logo code. */
                    STORE(ic->g, NULL, value, lambda_to_proc(ic, expr));
                    goto apply;
                } else if(car(expr) == ic->n_eval) {
                    /* We handle eval by creating an extra EVAL_C
                       continuation that, when applied to a value,
                       just jumps back into eval: for a second go around.
                       As a result, the value returned gets evaluated.
                       eval requires a value as input, so we adjust
                       allowed_results to reflect that.  Whether the eval'd
                       expression is allowed to return a value or not is
                       dependent upon the context in which eval was called. */
                    if(ic->debug_eval) eprintf(ic, "    eval\n");
                    STORE(ic->g, NULL, ic->continuation,
                          mk_eval_c(ic,
                                    ic->continuation,
                                    ic->frame,
                                    expr,
                                    ic->current_line,
                                    ic->allowed_results,
                                    ic->output_error_info,
                                    tail));
                    STORE(ic->g, NULL, expr, car(cdr(expr)));
                    ic->allowed_results =
                      (ic->allowed_results & OUTPUT_MASK) | VALUE_OK;
                    STORE(ic->g, NULL, ic->output_error_info,
                          mk_output_error_info(ic,
                                               ic->n_eval,
                                               expr,
                                               ic->current_line));
                    goto eval;
                } else if(car(expr) == ic->n_internal_callcc) {
                    /* mk_continuation makes a CONTINUATION object, which
                       is a type of s-expression (struct sexpr).
                       This is a user visible object that is handed to
                       the program.  It contains a "struct continuation *"
                       of the sort applied by apply, but those continuations
                       are not directly user visible. */
                    /* This works by rewriting the expression.  There
                       may be a better way to do this, but the tricky thing
                       is that C recursion is not allowed here.  After all,
                       a continuation could be captured while inside the
                       call/cc'd function! */
                    if(ic->debug_eval) eprintf(ic, "    callcc\n");
                    STORE(ic->g, NULL,
                          expr,
                          cons(ic, car(cdr(expr)),
                                   cons(ic, mk_proc(ic,
                                                    mk_continuation(ic, ic->continuation),
                                                    0,
                                                    1,
                                                    1),
                                             ic->g_nil)));
                    goto eval;
                } else if(name_eq(car(expr), ic->n_if) ||
                          name_eq(car(expr), ic->n_ifelse)) {
                    /* Run the conditional expression as a non-tail call
                       with OUTPUT_OK on.  We stash the current value for
                       ic->allowed_results in the IF_C continuation.
                       Down in the handler for IF_C continuations, the 
                       stored value will be passed on to the then_expr or
                       the else_expr, depending on whether cond returns
                       "true or "false. */
                    sexpr *cond = ic->g_nil;
                    sexpr *then_expr = ic->g_unbound;
                    sexpr *else_expr = ic->g_unbound;

                    if(!is_nil(ic, cdr(expr)))
                        cond = car(cdr(expr));

                    if(!is_nil(ic,          cdr(expr)) &&
                       !is_nil(ic,      cdr(cdr(expr))))
                        then_expr = car(cdr(cdr(expr)));

                    if(!is_nil(ic,              cdr(expr)) &&
                       !is_nil(ic,          cdr(cdr(expr))) &&
                       !is_nil(ic,      cdr(cdr(cdr(expr)))))
                        else_expr = car(cdr(cdr(cdr(expr))));

                    if(ic->debug_eval) eprintf(ic, "    if\n");
                    STORE(ic->g, NULL, 
                          ic->continuation, 
                          mk_if_c(ic,
                                  ic->continuation,
                                  ic->frame,
                                  cond,
                                  ic->current_line,
                                  ic->allowed_results,
                                  ic->output_error_info,
                                  tail,
                                  then_expr,
                                  else_expr));
                    ic->allowed_results =
                        (ic->allowed_results & OUTPUT_MASK) | VALUE_OK;
                    STORE(ic->g, NULL, ic->output_error_info,
                          mk_output_error_info(ic, car(expr), cond, ic->current_line));
                    STORE(ic->g, NULL, expr, cond);
                    tail = 0;
                    goto eval;            
                } else if(name_eq(car(expr), ic->n_beginresult)) {
                    /* This is the internal form that handles RUNRESULT.
                       It acts like begin, except the last value is
                       able to either return a result or not.  If no
                       result is returned, then beginresult returns an
                       empty list.  If a result is returned, then beginresult
                       returns a list containing just the result.

                       beginresult always returns a value, so VALUE_OK needs
                       to be set.
                       If VALUE_OK is not set, then we need know that we
                       will generate a "You don't say what to do with ..."
                       error, but we don't yet know the contents of the error.
                       Therefore, we set up a special continuation that will
                       error out once it receives a value, and proceed as
                       usual for the moment. */
                    if(!(ic->allowed_results & VALUE_OK)) {
                        STORE(ic->g, NULL,
                              ic->continuation,
                              mk_dont_say_c(ic,
                                            ic->continuation,
                                            ic->frame,
                                            expr,
                                            ic->current_line,
                                            ic->allowed_results,
                                            ic->output_error_info,
                                            tail));
                        tail = 0;
                    }
                    STORE(ic->g, NULL, expr, cons(ic, ic->n_begin, cdr(expr)));
                    STORE(ic->g, NULL, ic->continuation,
                          mk_beginresult_c(ic,
                                           ic->continuation,
                                           ic->frame,
                                           expr,
                                           ic->current_line,
                                           ic->allowed_results,
                                           ic->output_error_info,
                                           tail));
                    ic->allowed_results = ic->allowed_results |
                                          VALUE_OK |
                                          NO_VALUE_OK;
                    goto eval;
                } else if(name_eq(car(expr), ic->n_begin) ||
                          name_eq(car(expr), ic->n_beginnoresult) ||
                          name_eq(car(expr), ic->n_upstack_output)) {
                    /* (begin ...) runs each of its arguments.
                       Any argument other than the last one is not
                       allowed to return a value.
                       The last argument is allowed to return a value if
                       the entire (begin ...) expression is allowed to
                       return a value.

                       (beginnoresult ...) runs each of its arguments.
                       None of them are allowed to return a value.
                       This is the internal mechanism used to wrap the body
                       of a Logo procedure to disallow any of its statements
                       (in particular, the last) from returning any values.

                       If the procedure is required to return a value
                       (STOP_OK is not set), then there is no way for the
                       last statement to successfully terminate.  Some
                       subexpression must OUTPUT a value for the procedure
                       to succeed.  In this case, we create a
                       DIDNT_OUTPUT_C continuation to print an error
                       if the sequence ever terminates normally.
                       If the procedure makes a tail call via
                       OUTPUT then this continuation will be thrown 
                       away.

                       We set NO_VALUE_OK even if STOP_OK is not set
                       so that the sequence will terminate and return
                       control to the DIDNT_OUTPUT_C rather than
                       creating some cryptic error message farther
                       down the call stack.

                       UPSTACK_OUTPUT runs its arguments like begin,
                       but OUTPUT and STOP return to the parent procedure's
                       caller.  allowed_results must have STOP_OK and
                       OUTPUT_OK adjusted to the appropriate values
                       for the new output continuation.  Then the frame
                       is mutated to adjust the output continuation.
                     */

                    if(ic->debug_eval) eprintf(ic, "    begin\n");
                    if(name_eq(car(expr), ic->n_beginnoresult)) {
                      ic->allowed_results =
                        (ic->allowed_results & OUTPUT_MASK) | NO_VALUE_OK;
                      if(!(ic->allowed_results & STOP_OK)) {
                            STORE(ic->g, NULL,
                                  ic->continuation,
                                  mk_didnt_output_c(ic,
                                                    ic->continuation,
                                                    ic->frame,
                                                    expr,
                                                    ic->current_line,
                                                    ic->allowed_results,
                                                    ic->frame->output_error_info,
                                                    tail));
                            tail = 0;
                        }
                    } else if(name_eq(car(expr), ic->n_upstack_output)) {

                        ic->frame->allowed_results =
                            ic->frame->parent_allowed_results;
                        STORE(ic->g, ic->frame,
                              ic->frame->continuation,
                              ic->frame->parent_continuation);

                        ic->allowed_results =
                         (ic->allowed_results & VALUE_MASK) |
                         ((ic->frame->allowed_results & VALUE_OK) ?
                          OUTPUT_OK : 0) |
                         ((ic->frame->allowed_results & NO_VALUE_OK) ?
                          STOP_OK : 0);
                    }

                    if(is_nil(ic, cdr(expr))) {
                        /* (begin) with no arguments.  Make sure that
                           we are allowed to return nothing, and then
                           return nothing. */
                        if(!(ic->allowed_results & NO_VALUE_OK))
                            didnt_output(ic, ic->output_error_info);
                        STORE(ic->g, NULL, value, ic->g_unbound);
                        goto apply;
                    } else if(is_nil(ic, cdr(cdr(expr)))) {
                        /* (begin <expr>) with just one expression.
                           Just switch to <expr> and jump back to eval: */
                        STORE(ic->g, NULL, expr, car(cdr(expr)));
                        goto eval;
                    } else if(car(cdr(expr))->t != LINE &&
                              name_eq(car(cdr(cdr(expr))), ic->n_stop)) {
                        /* (begin <expr> STOP) -- Assuming <expr> is not a LINE
                           Confirm that we are allowed to exit the current
                           procedure with a STOP, and run <expr> without
                           allowing it to return a value (only NO_VALUE_OK
                           is set).  Use the continuation from the environment
                           frame so that the result is passed out of the
                           procedure and not to the containing expression. */
                        if(!(ic->allowed_results & STOP_OK)) {
                            /* Need to show an error, but must run last
                               item first.  Create a didnt_output_c
                               to error out once we get a value back.
                               Take output_error_info from the frame and
                               not the continuation because, technically,
                               the error occurs in our caller when we don't
                               output a value. */
                            STORE(ic->g, NULL,
                                  ic->continuation,
                                  mk_didnt_output_c(ic,
                                                    ic->continuation,
                                                    ic->frame,
                                                    expr,
                                                    ic->current_line,
                                                    ic->allowed_results,
                                                    ic->frame->output_error_info,
                                                    tail));
                            tail = 0;
                        } else {
                            STORE(ic->g, NULL, ic->continuation,
                                               ic->frame->continuation);
                            tail = 1;
                        }

                        /* Tail call the last expression with NO_VALUE_OK. */
                        STORE(ic->g, NULL, expr, car(cdr(expr)));

                        ic->allowed_results =
                            (ic->allowed_results & OUTPUT_MASK) | NO_VALUE_OK;
                        STORE(ic->g, NULL, ic->output_error_info,
                              mk_output_error_info(ic, ic->g_nil, expr, 
                                                   ic->current_line));
                              
                        goto eval;
                    } else {
                        /* (begin <expr> ...)
                           This is a begin with multiple expressions.
                           Store all but the first in a BEGIN_C continuation
                           and eval: the first one with no value allowed. */
                        STORE(ic->g, NULL, ic->continuation,
                              mk_begin_c(ic,
                                         ic->continuation,
                                         ic->frame,
                                         car(cdr(expr)),
                                         ic->current_line,
                                         ic->allowed_results,
                                         ic->output_error_info,
                                         tail,
                                         cdr(cdr(expr))));
                        STORE(ic->g, NULL, expr, car(cdr(expr)));
                        ic->allowed_results =
                            (ic->allowed_results & OUTPUT_MASK) | NO_VALUE_OK;
                        STORE(ic->g, NULL, ic->output_error_info,
                              mk_output_error_info(ic, ic->g_nil, expr, ic->current_line));
                        tail = 0;
                        goto eval;
                    }
                } else if(car(expr) == ic->n_maybeoutput) {
                    /* (.maybeoutput <expr>)
                       If <expr> returns a value, then OUTPUT's that value.
                       If <expr> does not return a value, then STOP's.
                       Or, alternatively, if the current procedure is allowed
                       to STOP, then <expr> is allowed to return no value, and
                       if the current procedure is allowed to OUTPUT then
                       <expr> is allowed to return a value.
                       It is possible for both to be possible, for example
                       inside of a RUNRESULT.
                       The continuation is switched to the one from the
                       environment frame so that either way the result of
                       the evaluation is returned to the caller and not to
                       the enclosing expression.  */
                    STORE(ic->g, NULL, expr, car(cdr(expr)));
                    ic->allowed_results = 
                        (ic->allowed_results & OUTPUT_MASK) | 
                        (ic->allowed_results & OUTPUT_OK ? VALUE_OK : 0) |
                        (ic->allowed_results & STOP_OK ? NO_VALUE_OK : 0);
                    STORE(ic->g, NULL, ic->continuation,
                                       ic->frame->continuation);
                    tail = 1;
                    goto eval;
                } else if(car(expr) == ic->n_output ||
                          car(expr) == ic->n_op) {
                    /* (OUTPUT <expr>)
                       This is only allowed if OUTPUT_OK is set.
                       If OUTPUT_OK is not set, then we will have an
                       error of the type "You don't say what to do with ...",
                       but we don't know what it is yet, so we
                       create an error continuation to print out the
                       error once the value is calculated.
                       Otherwise, the continuation is reset to the one
                       from the current environment frame so that the
                       computed value will be returned to the calling
                       procedure and not to the enclosing expression.
                       VALUE_OK is set so that we must get a value back
                       from <expr>. */
                    if(!(ic->allowed_results & OUTPUT_OK)) {
                        /* We now know there's going to be an error 
                           of the type "You don't say what to do with ..."
                           but we don't know "...".  Set up a dont_say_c
                           continuation which will trigger the error when
                           the argument is done being evaluated. */
                        STORE(ic->g, NULL,
                              ic->continuation,
                              mk_dont_say_c(ic,
                                            ic->continuation,
                                            ic->frame,
                                            expr,
                                            ic->current_line,
                                            ic->allowed_results,
                                            ic->frame->output_error_info,
                                            tail));
                        tail = 0;
                    } else {
                        STORE(ic->g, NULL, ic->continuation,
                                           ic->frame->continuation);
                        tail = 1;
                    }

                    STORE(ic->g, NULL, expr, car(cdr(expr)));
                    ic->allowed_results = 
                        (ic->allowed_results & OUTPUT_MASK) | VALUE_OK;
                    STORE(ic->g, NULL, ic->output_error_info,
                          mk_output_error_info(ic,
                                               ic->n_output,
                                               expr,
                                               ic->current_line));
                    goto eval;
                } else {
                    /* (<expr> ...)
                       This is a function call.
                       Function calls are a multi-step process.
                       The operator must be evaluated and sent to a
                       OPER_C continuation.
                       The actual parameters must be evaluated.  Each one is
                       sent to an APPLY_C continuation.
                       Finally, the values of any outstanding optional
                       arguments must be evaluated and sent to an
                       OPTIONALS_C continuation.
                       Because of the structure of the interpreter, these
                       closely related operations and loops are scattered
                       all over the place. */
                    if(ic->debug_eval) eprintf(ic, "    function call\n");

                    if(car(expr) != ic->n_internal_invoke && car(expr)->t == NAME) {
                        /* If the operand is a NAME other than internal_invoke,
                           then we just ship it over to OPER_C as is, without
                           any evaluation. */
                        STORE(ic->g, NULL, value, car(expr));
                        STORE(ic->g, NULL, ic->continuation,
                              mk_oper_c(ic,
                                          ic->continuation,
                                          ic->frame,
                                          car(expr),
                                          ic->current_line,
                                          ic->allowed_results,
                                          ic->output_error_info,
                                          tail,
                                          cdr(expr)));
                        goto apply;
                    } else {
                        /* The operator must be evaluated.
                           If the operator is internal_invoke, then
                           we use the first argument as the expression
                           to evaluate to get the operator, otherwise
                           we evaluate the operator.
                           internal_invoke only knows about things
                           lisp can apply, like lambda forms.  It does
                           not know about things like [[x] x+5].  Those
                           are handled by APPLY in initialize.txt. */
                        sexpr *params;
                        if(car(expr) == ic->n_internal_invoke) {
                          params = cdr(cdr(expr));
                          STORE(ic->g, NULL, expr, car(cdr(expr)));
                        } else {
                          params = cdr(expr);
                          STORE(ic->g, NULL, expr, car(expr));
                        }

    
                        STORE(ic->g, NULL, ic->continuation,
                              mk_oper_c(ic,
                                        ic->continuation,
                                        ic->frame,
                                        expr,
                                        ic->current_line,
                                        ic->allowed_results,
                                        ic->output_error_info,
                                        tail,
                                        params));
                        ic->allowed_results =
                            (ic->allowed_results & OUTPUT_MASK) | VALUE_OK;
                        STORE(ic->g, NULL, ic->output_error_info,
                              mk_output_error_info(ic,
                                                   ic->n_internal_invoke,
                                                   expr,
                                                   ic->current_line));
                        goto eval;
                    }
                }
                    
        }

    apply:
        if(interrupted) {
            /* The global interrupted can be set by a signal handler
               to indicate that interpretation should terminate. */
            interrupted = 0;
            eprintf(ic, "User interrupt.\n");
            fatal_error(ic, ic->continuation->line);
        }

        if(paused) {
            sexpr *pause_call, *quote_call;
            paused = 0;

            pause_call = protect(ic->g, 
                     cons(ic, ic->n_pause_caller, 
                              cons(ic, cons(ic, ic->n_quote,
                                                cons(ic, ic->continuation->line->u.line.procedure, ic->g_nil)),
                                       ic->g_nil)));
            if(value == ic->g_unbound) {
                quote_call = cons(ic, ic->n_begin, ic->g_nil);
            } else {
                quote_call = cons(ic, ic->n_quote,
                                      cons(ic, value, ic->g_nil));
            }

            STORE(ic->g, NULL,
                  expr,
                  cons(ic, ic->n_begin,
                           cons(ic, pause_call,
                                    cons(ic, quote_call, ic->g_nil))));
            unprotect(ic->g);

            ic->allowed_results =
                (ic->continuation->allowed_results & OUTPUT_MASK) |
                VALUE_OK | NO_VALUE_OK;
            STORE(ic->g, NULL, ic->output_error_info,
                               ic->continuation->output_error_info);
            STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
            tail = ic->continuation->tail;
            STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
            reroot(ic, ic->frame);
            /* Keep the same continuation. */
            goto eval;
        }

        state = APPLY;

        if(ic->debug_apply) {
            eprintf(ic, "Apply: ");
            eprint_sexpr(ic, ic->continuation->expr);
            eprintf(ic, " => ");
            eprint_sexpr(ic, value);
            eprintf(ic, "\n");
        }

        switch(ic->continuation->t) {
            case RETURN_C:
                /* There is only one RETURN_C, the one initially created
                   on entrance to the interpreter.  If we get here, we
                   have returned a value to the RETURN_C continuation and
                   that value should be returned from eval(). */
                if(ic->debug_apply) eprintf(ic, "    return_c\n");

              exit: /* This label is for use by error exits that longjmp()
                       to the setjmp() at the beginning of eval(). */

                /* Reroot to the root frame before returning so the
                   next evaluation will begin there. */
                STORE(ic->g, NULL, ic->frame, ic->root_frame);
                reroot(ic, ic->frame);
                STORE(ic->g, NULL, ic->continuation, NULL);

                /* These unprotect_ptr()'s are to remove expr, value, and
                   values from the root set.  They were added at the beginning
                   of eval(). */
                unprotect_ptr(ic->g);
                unprotect_ptr(ic->g);
                unprotect_ptr(ic->g);
                return value;

            case IF_C:
                /* A conditional expression has completed, and its value
                   is being passed to an IF_C continuation.  The IF_C
                   contains both a then_expr and an else_expr.
                   Logo uses "true and "false as the only valid boolean
                   operators.  Anything else is an error. */
                if(ic->debug_apply) eprintf(ic, "    if_c\n");

                if(name_eq(value, ic->n_true)) {
                    STORE(ic->g, NULL, expr, ic->continuation->u.if_c.then_expr);
                } else if(name_eq(value, ic->n_false)) {
                    STORE(ic->g, NULL, expr, ic->continuation->u.if_c.else_expr);
                } else {
                    eprintf(ic, "IF doesn't like ");
                    eprint_sexpr(ic, value);
                    eprintf(ic, " as input.\n");
                    eprintf(ic, "Stacktrace:\n");
                    print_stacktrace(ic);
                    throw_error(ic, ic->continuation->line);
                }

                ic->allowed_results = ic->continuation->allowed_results;
                STORE(ic->g, NULL, ic->output_error_info,
                                   ic->continuation->output_error_info);
                STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                tail = ic->continuation->tail;
                STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                reroot(ic, ic->frame);
                STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);

                goto eval;

            case BEGIN_C:
                if(ic->debug_apply) eprintf(ic, "    begin_c\n");

                if(value->t == LINE) {
                    /* Got a LINE object.  Need to replace the BEGIN_C
                       with one that has the correct LINE before processing.
                       This funkyness avoids mutating the contents of
                       ic->continuation. */
                    STORE(ic->g, NULL,
                          ic->continuation,
                          mk_begin_c(ic,
                                     ic->continuation->parent,
                                     ic->continuation->frame,
                                     ic->continuation->expr,
                                     value,
                                     ic->continuation->allowed_results,
                                     mk_output_error_info(ic,
                                                          ic->g_nil,
                                                          ic->g_nil,
                                                          value),
                                     ic->continuation->tail,
                                     ic->continuation->u.begin_c.exprs));
                } else if(value != ic->g_unbound) {
                    eprintf(ic, "You don't say what to do with ");
                    eprint_sexpr(ic, value);
                    throw_error(ic, ic->continuation->line);
                }
                
                if(is_nil(ic, ic->continuation->u.begin_c.exprs)) {
                    /* No more expressions to evaluate.  Return to caller. */
                    STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                    goto apply;
                } else if(is_nil(ic, cdr(ic->continuation->u.begin_c.exprs))) {
                    /* One more expression to evaluate.  Do not set up
                       a BEGIN_C continuation, just have it return to our
                       caller. */
                    STORE(ic->g, NULL, expr, car(ic->continuation->u.begin_c.exprs));
                    ic->allowed_results = ic->continuation->allowed_results;
                    STORE(ic->g, NULL, ic->output_error_info,
                                       ic->continuation->output_error_info);
                    STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                    tail = ic->continuation->tail;
                    STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                    reroot(ic, ic->frame);
                    STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                    goto eval;
                } else if(car(ic->continuation->u.begin_c.exprs)->t != LINE &&
                          name_eq(car(cdr(ic->continuation->u.begin_c.exprs)),
                                  ic->n_stop)) {
                    /* One more expression followed by a STOP.
                       Evaluate that expression in the environment from
                       the current continuation, but have it return to
                       our caller continuation stored in the frame.
                       Only NO_VALUE_OK is allowed.  This expression
                       is not allowed to return a value. */
                    if(!(ic->continuation->allowed_results & STOP_OK)) {
                        /* We know we will have a "X didn't output to Y"
                           error, but we need to evaluate the last expression
                           first in case it has side effects like printing.
                           We use the output_error_info from the environment
                           frame because technically the error occurs in our
                           caller even though we detect it here, and that
                           will have the line info from the caller. */
                        STORE(ic->g, NULL,
                              ic->continuation,
                              mk_didnt_output_c(ic,
                                                ic->continuation,
                                                ic->continuation->frame,
                                                car(ic->continuation->u.begin_c.exprs),
                                                ic->continuation->line,
                                                ic->continuation->allowed_results,
                                                ic->continuation->frame->output_error_info,
                                                ic->continuation->tail));
                        tail = 0;
                    } else {
                        STORE(ic->g, NULL, ic->continuation,
                                           ic->frame->continuation);
                        tail = 1;
                    }

                    STORE(ic->g, NULL, expr, car(ic->continuation->u.begin_c.exprs));
                    ic->allowed_results =
                        (ic->continuation->allowed_results & OUTPUT_MASK) |
                        NO_VALUE_OK;
                    STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                    STORE(ic->g, NULL, ic->output_error_info,
                          mk_output_error_info(ic, ic->g_nil, expr,
                                               ic->current_line));
                    STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                    reroot(ic, ic->frame);
                    goto eval;
                } else {
                    /* More than one value left.  Run the first one and
                       set up another BEGIN_C with the rest in waiting.
                       The expression is not allowed to return a value. */
                    STORE(ic->g, NULL, expr, car(ic->continuation->u.begin_c.exprs));
                    ic->allowed_results =
                        (ic->continuation->allowed_results & OUTPUT_MASK) | NO_VALUE_OK;
                    STORE(ic->g, NULL, ic->output_error_info,
                                       ic->continuation->output_error_info);
                    STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                    tail = 0;
                    STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                    reroot(ic, ic->frame);
                    STORE(ic->g, NULL,
                          ic->continuation,
                          mk_begin_c(ic,
                                  ic->continuation->parent,
                                  ic->frame,
                                  expr,
                                  ic->continuation->line,
                                  ic->continuation->allowed_results,
                                  ic->continuation->output_error_info,
                                  ic->continuation->tail,
                                  cdr(ic->continuation->u.begin_c.exprs)));
                    goto eval;
                    
                }
            case OPER_C:
                /* value is an operator in a procedure call.
                   We need to figure out:
                       Is it a macro?
                       Is it a funarg?  If so, what is its frame?
                       What are the minimum and maximum numbers of
                       arguments?

                   If it's a Lisp macro, we can create an APPLY_C
                   continuation where the "already evaluated" values
                   are just the raw unevaluated arguments.  We wrap this
                   in an EVAL_C continuation so the output of the macro
                   is evaluated.
 
                   If it's a Logo macro, we need to evaluate the
                   arguments.  We also wrap things in an EVAL_C in
                   this case so the macro output will be evaluated.

                   If it's an fsubr, then we run the primitive on the
                   unevaluated arguments right here.  Otherwise, we need
                   to set up evaluation of the first argument and an
                   APPLY_C continuation to receive it when finished. */

                if(ic->debug_apply) eprintf(ic, "    oper_c\n");

                {
                    /* Get all the interesting pieces of information about
                       the operator. */
                    sexpr *proc, *funarg, *macro, *oper;
                    if(!get_apply_parts(ic, value, &proc, &funarg, &macro, &oper, 1)) {
                        throw_error(ic, ic->continuation->line);
                    }

                    if(!is_nil(ic, macro)) {
                        /* It's a macro.  
                           Lisp macros and Logo macros are just different
                           enough that it's easier to handle them
                           separately. */

                        if(name_eq(macro->u.macro.macro_type,
                                   ic->n_lisp_macro)) {
                            continuation *econt =
                                mk_eval_c(ic,
                                          ic->continuation->parent,
                                          ic->continuation->frame,
                                          ic->continuation->expr,
                                          ic->continuation->line,
                                          ic->continuation->allowed_results,
                                          ic->continuation->output_error_info,
                                          ic->continuation->tail);

                            /* Lisp macros don't have their arguments
                               evaluated.  Logo macros do.
                               If we have a lisp macro, we set up
                               the apply continuation with the
                               unevaluated arguments and jump into
                               the APPLY_C handler.
                               If we have a Logo macro, we skip this
                               and finish handling the arguments
                               below just like with a normal procedure.
                               */
                            /* protect() is needed because of the call
                               to reverse(). */
                            protect(ic->g, econt);
                            STORE(ic->g, NULL, ic->continuation,
                                  mk_apply_c(ic,
                                             econt,
                                             ic->continuation->frame,
                                             ic->continuation->expr,
                                             ic->continuation->line,
                                             (ic->continuation->allowed_results & OUTPUT_MASK) | VALUE_OK,
                                             ic->continuation->output_error_info,
                                             0,
                                             value,
                                             ic->g_nil,
                                             reverse(ic,
                                                     ic->continuation->u.oper_c.params)));
                            unprotect(ic->g);
                            /* no_value tells APPLY_C to pretend that it did not
                               receive any input. */
                            STORE(ic->g, NULL, value, ic->g_no_value);
                            goto apply;
                        } else if(name_eq(macro->u.macro.macro_type,
                                          ic->n_logo_macro)) {
                            /* Logo macros must still output a result
                               and are never tail calls. */
                            continuation *econt =
                                mk_eval_c(ic,
                                          ic->continuation->parent,
                                          ic->continuation->frame,
                                          ic->continuation->expr,
                                          ic->continuation->line,
                                          ic->continuation->allowed_results,
                                          ic->continuation->output_error_info,
                                          ic->continuation->tail);
                            /* Make a dummy OPER_C to feed the rest of the
                               logic below. */
                            STORE(ic->g, NULL, ic->continuation,
                                mk_oper_c(ic,
                                          econt,
                                          ic->continuation->frame,
                                          ic->continuation->expr,
                                          ic->continuation->line,
                                          (ic->continuation->allowed_results & OUTPUT_MASK) | VALUE_OK,
                                          ic->continuation->output_error_info,
                                          0,
                                          ic->continuation->u.oper_c.params));
                        } else {
                            eprintf(ic, "Invalid macro type: ");
                            eprint_sexpr(ic, macro->u.macro.macro_type);
                            throw_error(ic, ic->continuation->line);
                        }
                    } else if(oper->t == FSUBR) {
                        /* Run the C function implementing the FSUBR. */

                        ic->allowed_results =
                            ic->continuation->allowed_results;
                        STORE(ic->g, NULL,
                                     ic->output_error_info,
                                     ic->continuation->output_error_info);
                        STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                        tail = ic->continuation->tail;
                        STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                        reroot(ic, ic->frame);

                        /* ic->current_primitive is used for error messages */
                        STORE(ic->g, NULL, ic->current_primitive, value);
                        STORE(ic->g, NULL, 
                              value, 
                              oper->u.fsubr.func(ic, 
                                                 ic->continuation->u.oper_c.params));
                        /* Verify that the results are allowed in the currrent
                           context. */
                        if(value != ic->g_unbound &&
                           !(ic->continuation->allowed_results & VALUE_OK)) {
                            dont_say(ic, ic->continuation->output_error_info, value);
                        }
                        if(value == ic->g_unbound &&
                           !(ic->continuation->allowed_results & NO_VALUE_OK)) {
                            didnt_output(ic, ic->continuation->output_error_info);
                        }

                        /* Ship the result up the chain to the parent
                           continuation. */
                        STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                        goto apply;
                    }

                    /* If we get here, we're either running a normal
                       procedure call or we are dealing with a Logo
                       macro.  Both are handled the same way from here
                       until they return. */
                    if(is_nil(ic, ic->continuation->u.oper_c.params)) {
                        /* There are no arguments.  Go straight to APPLY_C. */
                        STORE(ic->g, NULL, ic->continuation,
                              mk_apply_c(ic,
                                         ic->continuation->parent,
                                         ic->continuation->frame,
                                         ic->continuation->expr,
                                         ic->continuation->line,
                                         ic->continuation->allowed_results,
                                         ic->continuation->output_error_info,
                                         ic->continuation->tail,
                                         value,
                                         ic->g_nil,
                                         ic->g_nil));
                        STORE(ic->g, NULL, value, ic->g_no_value);
                        goto apply;
                    } else {
                        /* We're doing a normal procedure call.  Eval the
                           first argument and have it return to an APPLY_C.
                           Arguments must return values. */
                        STORE(ic->g, NULL, expr, car(ic->continuation->u.oper_c.params));
                        ic->allowed_results =
                            (ic->continuation->allowed_results & OUTPUT_MASK) | VALUE_OK;
                        STORE(ic->g, NULL, ic->output_error_info,
                              mk_output_error_info(ic, value, expr,
                                                       ic->continuation->line));
                        STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                        tail = 0;
                        STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                        reroot(ic, ic->frame);
                        STORE(ic->g, NULL, ic->continuation,
                              mk_apply_c(ic,
                                         ic->continuation->parent,
                                         ic->continuation->frame,
                                         expr,
                                         ic->continuation->line,
                                         ic->continuation->allowed_results,
                                         ic->continuation->output_error_info,
                                         ic->continuation->tail,
                                         value,
                                         cdr(ic->continuation->u.oper_c.params),
                                         ic->g_nil));
                        goto eval;
                    }
                }
            case APPLY_C:
                /* An argument to a procedure has just finished being
                   evaluated and is stored in value.
                   If we got ic->g_unbound, that means that a subprocedure
                   failed to return a value and we have an error.
                   If we got ic->g_no_value, that is a signal from elsewhere
                   in eval() that we weren't actually sent a value.
                   Therefore, we cons value onto the values calculated so far
                   if it is not ic->g_no_value.

                   If we're done evaluating arguments, we test whether we 
                   have errors from too many or too few arguments.

                   If the operator is a SUBR, we can run it right here
                   and return the result.

                   If the operator is a user defined procedure, then we
                   need to hop on over to OPTIONALS_C to deal with evaluating
                   the default values of optional parameters one at a time. */
                   
                if(ic->debug_apply) eprintf(ic, "    apply_c\n");

                if(value == ic->g_unbound) {
                    eprint_sexpr(ic, ic->continuation->expr);
                    eprintf(ic, " didn't output to ");
                    eprint_sexpr(ic, ic->continuation->u.apply_c.oper);
                    throw_error(ic, ic->continuation->line);
                } else if(value == ic->g_no_value) {
                    STORE(ic->g, NULL, values, ic->continuation->u.apply_c.values);
                } else {
                    STORE(ic->g, NULL, 
                          values, 
                          cons(ic, value, ic->continuation->u.apply_c.values));
                }

                if(ic->debug_apply) {
                    eprintf(ic, "    values: ");
                    eprint_sexpr(ic, values);
                    eprintf(ic, "\n");
                }

                if(is_nil(ic, ic->continuation->u.apply_c.exprs)) {
                    /* All values have been evaluated. */
                    STORE(ic->g, NULL, values, reverse(ic, values));

                    if(ic->continuation->u.apply_c.oper->t == NAME &&
                       (ic->continuation->u.apply_c.oper->u.name.symbol->flags
                        & PROC_TRACED)) {
                        sexpr *oper = ic->continuation->u.apply_c.oper;
                        struct continuation *trace_c =
                              mk_trace_c(ic,
                                         ic->continuation->parent,
                                         ic->continuation->frame,
                                         ic->continuation->expr,
                                         ic->continuation->line,
                                         ic->continuation->allowed_results,
                                         ic->continuation->output_error_info,
                                         ic->continuation->tail,
                                         oper);
                        STORE(ic->g, NULL,
                              ic->continuation,
                              mk_apply_c(ic,
                                         trace_c,
                                         ic->continuation->frame,
                                         ic->continuation->expr,
                                         ic->continuation->line,
                                         ic->continuation->allowed_results,
                                         ic->continuation->output_error_info,
                                         ic->continuation->tail,
                                         ic->continuation->u.apply_c.oper,
                                         ic->continuation->u.apply_c.exprs,
                                         ic->continuation->u.apply_c.values));

                        tprint_sexpr(ic, cons(ic, oper, values));
                        tprintf(ic, "\n");
                    }

                    /* proc, funarg, macro, and oper are part of
                       continuation, and do not need to be protected */
                    sexpr *proc, *funarg, *macro, *oper;
                    if(!get_apply_parts(ic, ic->continuation->u.apply_c.oper,
                                        &proc, &funarg, &macro, &oper, 1)) {
                        throw_error(ic, ic->continuation->line);
                    }

                    if(is_nil(ic, proc)) {
                        eprintf(ic, "ERROR: No PROC found in: ");
                        eprint_sexpr(ic, ic->continuation->u.apply_c.oper);
                        throw_error(ic, ic->continuation->line);
                    }

                    int arg_count = length(ic, values);

                    if(arg_count < proc->u.proc.minargs) {
                        eprintf(ic, "Too few arguments to ");
                        eprint_sexpr(ic, ic->continuation->u.apply_c.oper);
                        throw_error(ic, ic->continuation->line);
                    }

                    if(arg_count > proc->u.proc.maxargs) {
                        eprintf(ic, "Too many arguments to ");
                        eprint_sexpr(ic, proc);
                        throw_error(ic, ic->continuation->line);
                    }

                    if(oper->t == SUBR) {
                        /* The operator is a SUBR.  We call it, test the
                           return value to see whether it was allowed to
                           return a value or not, and if everything is good,
                           we return the value to the parent continuation. */
                        if(ic->debug_apply) {
                            eprintf(ic, "    calling SUBR ");
                            eprint_sexpr(ic, oper);
                            eprintf(ic, "\n");
                        }

                        ic->allowed_results =
                            ic->continuation->allowed_results;
                        STORE(ic->g, NULL,
                                     ic->output_error_info,
                                     ic->continuation->output_error_info);
                        STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                        tail = ic->continuation->tail;
                        STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                        reroot(ic, ic->frame);

                        STORE(ic->g, NULL, ic->current_primitive,
                                           ic->continuation->u.apply_c.oper);
                        STORE(ic->g, NULL, value, oper->u.subr.func(ic, values));
                        if(ic->debug_apply) {
                            eprintf(ic, "    Got back: ");
                            eprint_sexpr(ic, value);
                            eprintf(ic, "\n");
                        }

                        if(value != ic->g_unbound &&
                           !(ic->continuation->allowed_results & VALUE_OK)) {
                            dont_say(ic, ic->continuation->output_error_info, value);
                        }
                        if(value == ic->g_unbound &&
                           !(ic->continuation->allowed_results & NO_VALUE_OK)) {
                            didnt_output(ic, ic->continuation->output_error_info);
                        }

                        STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                        goto apply;
                    } else if(oper->t == CONTINUATION) {
                        /* The operator is a CONTINUATION sexpr object.
                           Pull out the "struct continuation *" part,
                           make it our new continuation, and send it a value,
                           or ic->g_unbound, if no arguments. */
                        if(!is_nil(ic, values))
                            STORE(ic->g, NULL, value, car(values));
                        else
                            STORE(ic->g, NULL, value, ic->g_unbound);
                        STORE(ic->g, NULL, ic->continuation,
                                    oper->u.continuation.parent);
                        goto apply;
                    } else if(name_eq(ic->continuation->u.apply_c.oper,
                                      ic->n_internal_apply)) {
                        sexpr *new_oper = car(values);
                        sexpr *proc, *funarg, *macro, *oper;

                        /* Parent continuation is the continuation to
                           which the output should be returned.  It starts
                           as the parent of the current continuation, but will
                           be reset to an EVAL_C below if we are applying
                           a macro. */
                        if(get_apply_parts(ic,
                                            new_oper,
                                            &proc,
                                            &funarg,
                                            &macro,
                                            &oper,
                                            0) &&
                            !is_nil(ic, macro)) {
                            /* We are APPLY'ing a macro.  It's too late to
                               worry about whether to evaluate the arguments,
                               but we need to set up an EVAL_C continuation
                               to evaluate the output of the macro call. */
                            struct continuation *parent_continuation =
                              mk_eval_c(ic,
                                        ic->continuation->parent,
                                        ic->continuation->frame,
                                        ic->continuation->expr,
                                        ic->continuation->line,
                                        ic->continuation->allowed_results,
                                        ic->continuation->output_error_info,
                                        ic->continuation->tail);
                            /* Need to protect parent_continuation from
                               garbage collection during reverse(). */
                            protect(ic->g, parent_continuation);
                            STORE(ic->g,
                                  NULL,
                                  ic->continuation,
                                  mk_apply_c(ic,
                                             parent_continuation,
                                             ic->continuation->frame,
                                             ic->continuation->expr,
                                             ic->continuation->line,
                                             (ic->continuation->allowed_results
                                              & OUTPUT_MASK) |
                                             VALUE_OK,
                                             ic->continuation->output_error_info,
                                             0,
                                             new_oper,
                                             ic->g_nil,
                                             reverse(ic, car(cdr(values)))));
                            unprotect(ic->g);
                        } else {
                            /* We are applying a non-macro.  Just set up the
                               procedure and arguments and jump back into
                               APPLY_C. */
                            STORE(ic->g,
                                  NULL,
                                  ic->continuation,
                                  mk_apply_c(ic,
                                             ic->continuation->parent,
                                             ic->continuation->frame,
                                             ic->continuation->expr,
                                             ic->continuation->line,
                                             ic->continuation->allowed_results,
                                             ic->continuation->output_error_info,
                                             ic->continuation->tail,
                                             new_oper,
                                             ic->g_nil,
                                             reverse(ic, car(cdr(values)))));
                        }
                        STORE(ic->g, NULL, value, ic->g_no_value);
                        goto apply;
                    } else {
                        /* Normal procedure call.  We need to set up the
                           environment.  Here is where we handle FUNARG's */
                        tail = ic->continuation->tail;
                        if(!is_nil(ic, funarg)) {
                            /* We are processing a FUNARG a.k.a. a closure that
                               is closed over the dynamic environment.
                               Switch to the frame of the FUNARG. */
                            STORE(ic->g, NULL, ic->frame, funarg->u.funarg.frame);
                            reroot(ic, ic->frame);
                            tail = 0;
                            if(ic->debug_apply) {
                                eprintf(ic, "Applying FUNARG.  New oper: ");
                                eprint_sexpr(ic, oper);
                                eprintf(ic, "\n");
                            }
                        } else {
                            /* Not a FUNARG.  Switch to the frame stored in
                               the continuation. */
                            STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                            reroot(ic, ic->frame);
                        }

                        /* Here's the tail call magic.
                           When making a tail call in a dynamically scoped
                           language, we cannot just throw away the current
                           environment frame, like we can in Scheme.
                           We must extend it.  However, we cannot mutate
                           it due to the possibility of captured continuations
                           jumping into a procedure that has already ended
                           with a tail call.  However, we need to have
                           mutations that happen to a binding be visible in
                           all places where that binding is visible.
                           Therefore, extending a frame during a tail call
                           creates a new frame that has the same parent as
                           the old, that shares bindings with the
                           old for all variables that are not shadowed,
                           and that contains new bindings for the shadowed
                           variables.  Only sibling frames can share bindings
                           or else bad things might happen.

                           extend() and mk_bindings() both advance formals
                           through the list of formal parameters so that
                           we can tell OPTIONALS_C which parameters are
                           left. */
                        sexpr *formals = car(cdr(oper));
                        if(tail) {
                            STORE(ic->g, NULL,
                                  ic->frame,
                                  extend(ic,
                                         ic->frame,
                                         &formals,
                                         values,
                                         ic->continuation->u.apply_c.oper,
                                         ic->continuation->output_error_info,
                                         ic->continuation->parent,
                                         ic->continuation->allowed_results,
                                         ic->frame->continuation,
                                         ic->frame->allowed_results));
                         
                            reroot(ic, ic->frame);
                        } else {
                            STORE(ic->g, NULL,
                                  ic->frame,
                                  mk_frame(ic,
                                           ic->frame,
                                           mk_bindings(ic,
                                                       &formals,
                                                       values,
                                                       ic->g_nil),
                                           ic->continuation->u.apply_c.oper,
                                           ic->continuation->output_error_info,
                                           ic->continuation->parent,
                                           ic->continuation->allowed_results,
                                           ic->frame->continuation,
                                           ic->frame->allowed_results));
                            reroot(ic, ic->frame);
                        }
                        STORE(ic->g, NULL, value, ic->g_no_value);
                        STORE(ic->g, NULL,
                              ic->continuation,
                              mk_optionals_c(ic,
                                             ic->continuation->parent,
                                             ic->frame,
                                             expr,
                                             ic->continuation->line,
                                             ic->continuation->allowed_results,
                                             ic->continuation->output_error_info,
                                             ic->continuation->tail,
                                             oper,
                                             formals));
                        goto apply;
                    }
                } else {
                    /* We still have arguments to evaluate.  Set up the
                       environment for the first one, and set up an
                       APPLY_C continuation to handle the result. */
                    STORE(ic->g, NULL, expr, car(ic->continuation->u.apply_c.exprs));
                    ic->allowed_results =
                        (ic->continuation->allowed_results & OUTPUT_MASK) | VALUE_OK;
                    STORE(ic->g, NULL, ic->output_error_info,
                          mk_output_error_info(ic, ic->continuation->u.apply_c.oper, expr,
                                                   ic->continuation->line));
                    STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                    tail = 0;
                    STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                    reroot(ic, ic->frame);
                    STORE(ic->g, NULL, ic->continuation,
                          mk_apply_c(ic,
                                     ic->continuation->parent,
                                     ic->continuation->frame,
                                     expr,
                                     ic->continuation->line,
                                     ic->continuation->allowed_results,
                                     ic->continuation->output_error_info,
                                     ic->continuation->tail,
                                     ic->continuation->u.apply_c.oper,
                                     cdr(ic->continuation->u.apply_c.exprs),
                                     values));
                     goto eval;
                }
                
            case OPTIONALS_C:
                /* These continuations are for processing the
                   optional/default arguments in user defined procedures.
                   They will:
                   Receive a value (if any) and bind it to the next
                   optional formal parameter.
                   Set up the next default value (if any) for evaluation.
                   Set any "rest" arguments to nil. (A non-nil rest argument
                   would have been caught in APPLY_C by extend() or
                   mk_bindings().)
                   Perform the actual procedure call by running the body
                   of the procedure.

                   Some of the argument processing logic from APPLY_C is
                   duplicated here.  The environment must be extended
                   separately for each default value so that defaults can
                   depend upon previous values in the argument list. */
                {
                    struct sexpr *formals =
                        ic->continuation->u.optionals_c.formals;

                    STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                    reroot(ic, ic->frame);
                    STORE(ic->g, NULL, ic->current_line, ic->continuation->line);

                    if(value != ic->g_no_value) {
                        STORE(ic->g, NULL,
                              ic->frame,
                              extend(ic,
                                     ic->frame,
                                     &formals,
                                     protect(ic->g, cons(ic, value, ic->g_nil)),
                                     ic->frame->procedure,
                                     ic->continuation->output_error_info,
                                     ic->frame->continuation,
                                     ic->frame->allowed_results,
                                     ic->frame->parent_continuation,
                                     ic->frame->parent_allowed_results));
                        unprotect(ic->g);
                        reroot(ic, ic->frame);
                    }

                    if(!is_nil(ic, formals) && car(formals)->t == CONS &&
                       is_nil(ic, cdr(car(formals)))) {
                        /* The next argument is a "rest" argument with
                           a symbol by itself in a list. */
                        STORE(ic->g, NULL,
                              ic->frame,
                              extend(ic,
                                     ic->frame,
                                     &formals,
                                     ic->g_nil,
                                     ic->frame->procedure,
                                     ic->continuation->output_error_info,
                                     ic->frame->continuation,
                                     ic->frame->allowed_results,
                                     ic->frame->parent_continuation,
                                     ic->frame->parent_allowed_results));
                        reroot(ic, ic->frame);
                    }

                    if(!is_nil(ic, formals) && car(formals)->t == CONS &&
                       !is_nil(ic, cdr(car(formals)))) {
                        /* We have a default value.  Set up the environment
                           to evaluate it and have the result returned to
                           an OPTIONALS_C continuation. */
                        STORE(ic->g, NULL, expr, car(cdr(car(formals))));
                        ic->allowed_results =
                           (ic->continuation->allowed_results & OUTPUT_MASK) |
                           VALUE_OK;
                        STORE(ic->g, NULL, ic->output_error_info,
                              mk_output_error_info(ic, ic->continuation->u.optionals_c.oper, expr,
                                                       ic->continuation->line));
                        STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                        tail = 0;
                        STORE(ic->g, NULL,
                              ic->continuation,
                              mk_optionals_c(ic,
                                             ic->continuation->parent,
                                             ic->frame,
                                             expr,
                                             ic->continuation->line,
                                             ic->continuation->allowed_results,
                                             ic->continuation->output_error_info,
                                             ic->continuation->tail,
                                             ic->continuation->u.optionals_c.oper,
                                             formals));
                        goto eval;
                    } else {
                        /* We are done with all of the arguments.
                           It is time to run the body of the procedure. */
                        STORE(ic->g, NULL,
                              expr,
                              cons(ic,
                                   ic->n_begin,
                                   cdr(cdr(ic->continuation->u.optionals_c.oper))));
                        ic->allowed_results =
                          (ic->continuation->allowed_results & VALUE_MASK) |
                          (ic->frame->allowed_results & VALUE_OK ?
                           (OUTPUT_OK) : 0) |
                          (ic->frame->allowed_results & NO_VALUE_OK ?
                           (STOP_OK) : 0 );
                        STORE(ic->g, NULL, ic->output_error_info,
                                           ic->continuation->output_error_info);
                        STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                        tail = 1;
                        STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);

                        goto eval;
                    }
                }

            case EVAL_C:
                /* Evaluate the value that we received in the environment
                   of the continuation. */
                if(ic->debug_apply) eprintf(ic, "    eval_c\n");
                STORE(ic->g, NULL, expr, value);
                ic->allowed_results = ic->continuation->allowed_results;
                STORE(ic->g, NULL, ic->output_error_info,
                                   ic->continuation->output_error_info);
                STORE(ic->g, NULL, ic->current_line, ic->continuation->line);
                tail = ic->continuation->tail;
                STORE(ic->g, NULL, ic->frame, ic->continuation->frame);
                reroot(ic, ic->frame);
                STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                goto eval;

            case BEGINRESULT_C:
                /* This receives the value returned from a (beginresult ...)
                   form.  It returns an empty list if it is given the
                   unbound object, otherwise it returns a list containing
                   just the returned value. */
                if(value->t == UNBOUND) {
                    STORE(ic->g, NULL, value, ic->g_nil);
                } else {
                    STORE(ic->g, NULL, value, cons(ic, value, ic->g_nil));
                }
                STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                goto apply;

            case DONT_SAY_C:
                /* This continuation is for generating a delayed error
                   message.  See the calls to mk_dont_say_c() above. */
                dont_say(ic, ic->continuation->output_error_info, value);

            case DIDNT_OUTPUT_C:
                /* This continuation is for generating a delayed error
                   message.  See the calls to mk_didnt_output_c() above. */
                didnt_output(ic, ic->continuation->output_error_info);

            case TRACE_C:
                tprint_sexpr(ic, ic->continuation->u.trace_c.procedure);
                tprintf(ic, " outputs ");
                if(value->t == NAME)
                    tprintf(ic, "\"");
                tprint_sexpr(ic, value);
                tprintf(ic, "\n");
                STORE(ic->g, NULL, ic->continuation, ic->continuation->parent);
                goto apply;
        }

    return NULL;
}


/* Prints a stack trace.  Sometimes useful for debugging, but often
   the evidence has been removed by tail call optimization.
 */
void print_stacktrace(IC *ic) {
  /* frame *f = ic->frame; */
  continuation *c = ic->continuation;

  for(;;) {
    eprintf(ic, "    ");
    eprint_sexpr(ic, c->expr);
    eprintf(ic, "\n");
    if(c->parent == NULL)
      break;
    c = c->parent;
  }
}

/* Prints an error message and longjmp's to the beginning of eval
   where things can be cleaned up.
 */
#ifdef __GNUC__
__attribute__((noreturn))
#endif
void interrupt_execution(IC *ic, sexpr *line, enum error_type error_type) {
    if(line != NULL) {
        eprintf(ic, " in ");
        eprint_sexpr(ic, line->u.line.procedure);
        eprintf(ic, " at:\n");
        eprint_sexpr(ic, line->u.line.parsed_line);
    }

    eprintf(ic, "\n");

    if(ic->n_error_catcher->u.name.symbol->value == ic->g_unbound) {
        /* If there is no error catcher, we just print the accumulated
           error message. */
        tprintf(ic, "%s", get_cstring(ic, word_from_byte_buffer(ic, 
           ic->error_byte_buffer->u.byte_bufferp.byte_buffer)));
    } else {
        /* If there is an error catcher, which will be called
           by the error handler in eval(), we stash the accumulated
           error message so that it can be retrieved by ERRROR. */
        STORE(ic->g,
              ic->n_error->u.name.symbol,
              ic->n_error->u.name.symbol->value,
              word_from_byte_buffer(ic,
                  ic->error_byte_buffer->u.byte_bufferp.byte_buffer));
    }

    /* Wipe out the error buffer so that it can receive later messages. */
    clear_byte_buffer(ic->error_byte_buffer->u.byte_bufferp.byte_buffer);

    /* Jump to the error handler in eval(). */
    longjmp(ic->abort, error_type);
}

#ifdef __GNUC__
__attribute__((noreturn))
#endif
void throw_error(IC *ic, sexpr *line) {
    interrupt_execution(ic, line, CATCHABLE_ERROR);
}

#ifdef __GNUC__
__attribute__((noreturn))
#endif
void fatal_error(IC *ic, sexpr *line) {
    interrupt_execution(ic, line, FATAL_ERROR);
}

/* Used by primitives to generate "X doesn't like Y as input" errors. */
void bad_argument(IC *ic, sexpr *arg) {
    eprint_sexpr(ic, ic->current_primitive);
    eprintf(ic, " doesn't like ");
    eprint_sexpr(ic, arg);
    eprintf(ic, " as input");
    throw_error(ic, ic->current_line);
}
