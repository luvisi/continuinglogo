
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





/* This file contains most of the pimitive procedures that are in the
   global Logo environment at startup.

   Most procedures take an Interpreter Context and a list of arguments.

   Most procedures are SUBR's, meaning their arguments are evaluated before
   they are called.

   QUOTE is an FSUBR, meaning that its arguments are not evaluated before
   it is called.

   Most procedures assume they will be passed the correct number of arguments,
   or a number within appropriate limits, because the interpreter enforces
   maximum and minimum argument counts.  The counts for each function are
   specified in initialize_global_environment at the end of this file.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <unistd.h>
#include <setjmp.h>
#include "global_environment.h"
#include "interpreter.h"
#include "pcgc.h"
#include "list_memory.h"
#include "reader.h"
#include "logoreader.h"
#include "treeify.h"
#include "io.h"
#include "config.h"
#include "wxui.h"
#include "turtles.h"
#include "audio.h"
#include "get_apply_parts.h"


/* Increment a number by one. */
sexpr *inc(IC *ic, sexpr *s) {
    return mk_number(ic, to_number(ic, car(s))->u.number.value + 1);
}

/* Decrement a number by one. */
sexpr *dec(IC *ic, sexpr *s) {
    return mk_number(ic, to_number(ic, car(s))->u.number.value - 1);
}

/* Non-recursive test for equality. */
static sexpr *eq_helper(IC *ic, sexpr *first, sexpr *second) {
    protect_ptr(ic->g, (void **)&first);
    protect_ptr(ic->g, (void **)&second);

    sexpr *ret = NULL;

    /* An object is always equal to itself. */
    if(first == second) {
        STORE(ic->g, NULL, ret, ic->n_true);
        goto end;
    }

    /* If we are ignoring case, then two names that refer to the same
       symbol (essentially, two names that only differ in case)
       are equal. */
    if(name_eq(ic->n_caseignoredp->u.name.symbol->value, ic->n_true) &&
       name_eq(first, second)) {
        STORE(ic->g, NULL, ret, ic->n_true);
        goto end;
    }
      
    /* Only attempt a numeric comparison if both arguments could be
       converted to numbers. */
    if(numberp(ic, first) && numberp(ic, second)) {
        /* Store values in case NUMBER object created from first
           is garbage collected while creating a NUMBER from second. */
        double first_value = to_number(ic, first)->u.number.value;
        double second_value = to_number(ic, second)->u.number.value;
        if(first_value == second_value) {
            STORE(ic->g, NULL, ret, ic->n_true);
            goto end;
        }
    }

    STORE(ic->g, NULL, ret, ic->n_false);

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

sexpr *eq(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));

    return eq_helper(ic, first, second);
}

/* .EQ Object identity test. */
sexpr *dot_eq(IC *ic, sexpr *s) {
    if(car(s) == car(cdr(s)))
        return ic->n_true;
    else
        return ic->n_false;
}

/* The only FSUBR.  Receives its argument unevaluated and returns it
   unevaluated. */
sexpr *quote(IC *ic, sexpr *s) {
    return car(s);
}

/* Creates FUNARG's.
   The full version of FUNCTION is in initialize.txt, and handles
   a broader range of arguments than this does. */
sexpr *internal_function(IC *ic, sexpr *s) {
    return mk_funarg(ic, car(s), ic->frame);
}

/* Logo Assignment. */
sexpr *make(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **) &s);

    sexpr *first = to_name(ic, car(s));
    protect_ptr(ic->g, (void **) &first);

    sexpr *second = car(cdr(s));
    protect_ptr(ic->g, (void **) &second);

    if(first->u.name.symbol->flags & VAL_TRACED) {
        sexpr *old_fullprint = ic->n_fullprintp->u.name.symbol->value;
        protect_ptr(ic->g, (void **) &old_fullprint);

        STORE(ic->g,
              ic->n_fullprintp->u.name.symbol,
              ic->n_fullprintp->u.name.symbol->value,
              ic->n_true);
        tprintf(ic, "Make \"");
        tprint_sexpr(ic, first);
        tprintf(ic, " ");
        if(second->t == NAME)
            tprintf(ic, "\"");
        tprint_sexpr(ic, second);
        tprintf(ic, "\n");
        STORE(ic->g,
              ic->n_fullprintp->u.name.symbol,
              ic->n_fullprintp->u.name.symbol->value,
              old_fullprint);

        unprotect_ptr(ic->g);
    }

    STORE(ic->g, first->u.name.symbol,
                 first->u.name.symbol->value, second);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}
    
/* Sets the function slot of a symbol.
   Used for procedure definition in initialize.txt. */
sexpr *fset(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(first->t == NAME) {
        STORE(ic->g, first->u.name.symbol,
                     first->u.name.symbol->function, second);
        return ic->g_unbound;
    }
    bad_argument(ic, first);
}

/* Fetches the function slot of a symbol. */
sexpr *symbol_function(IC *ic, sexpr *s) {
    if(car(s)->t == NAME) {
        return car(s)->u.name.symbol->function;
    }
    bad_argument(ic, car(s));
}
    
/* Sets the source slot of a symbol. */
sexpr *source_set(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(first->t == NAME) {
        STORE(ic->g, first->u.name.symbol,
                     first->u.name.symbol->function_source, second);
        return ic->g_unbound;
    }
    bad_argument(ic, first);
}

/* Fetches the source slot of a symbol. */
sexpr *symbol_source(IC *ic, sexpr *s) {
    if(car(s)->t == NAME) {
        return car(s)->u.name.symbol->function_source;
    }
    bad_argument(ic, car(s));
}
    
/* Boolean negation.
   "false -> "true
   "true  -> "false
 */
sexpr *not_subr(IC *ic, sexpr *s) {
    if(name_eq(car(s), ic->n_true)) {
        return ic->n_false;
    } else if(name_eq(car(s), ic->n_false)) {
        return ic->n_true;
    } else {
        bad_argument(ic, car(s));
    }
}

/* PRINT
   Spaces between the arguments.
   Arguments that are lists are printed without brackets around them.
   (Sublists still have brackets.)
   Newline at the end.
 */
sexpr *print_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int printed_any = 0; /* State variable for printing a space before
                            every item but the first. */
    while(s->t == CONS) {
        if(printed_any)
            lprintf(ic, " ");
        else
            printed_any = 1;

        /* Special handling for lists so that we do not print
           brackets around top level lists. */
        if(car(s)->t == EMPTY_LIST) {
            /* Do nothing */
        } else if(car(s)->t == CONS) {
            sexpr *l = car(s);
            protect_ptr(ic->g, (void **)&l);

            int sub_printed_any = 0;
            while(l->t == CONS) {
                if(sub_printed_any)
                    lprintf(ic, " ");
                else
                    sub_printed_any = 1;

                lprint_sexpr(ic, car(l));
                STORE(ic->g, NULL, l, cdr(l));
            }
            unprotect_ptr(ic->g);
        } else {
            lprint_sexpr(ic, car(s));
        }
        STORE(ic->g, NULL, s, cdr(s));
    }

    lprintf(ic, "\n");

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

/* SHOW
   Spaces between arguments.
   Brackets shown around lists.
   Newline at the end.
 */
   
sexpr *show_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int printed_any = 0; /* State variable for printing a space before
                            every item but the first. */
    while(s->t == CONS) {
        if(printed_any)
            lprintf(ic, " ");
        else
            printed_any = 1;
        lprint_sexpr(ic, car(s));
        STORE(ic->g, NULL, s, cdr(s));
    }

    lprintf(ic, "\n");
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

/* TYPE
   No spaces between arguments.
   List arguments are printed without brackets, but still have spaces
   between their individual members.
   No newline at the end.
 */
sexpr *type_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    while(s->t == CONS) {
        /* Special handling for lists so that we do not print
           brackets around top level lists. */
        if(car(s)->t == CONS) {
            sexpr *l = car(s);
            protect_ptr(ic->g, (void **)&l);

            int sub_printed_any = 0;  /* We don't separate arguments with
                                         spaces, but we do separate elements
                                         of a list we are passed. */
            while(l->t == CONS) {
                if(sub_printed_any)
                    lprintf(ic, " ");
                else
                    sub_printed_any = 1;

                lprint_sexpr(ic, car(l));
                STORE(ic->g, NULL, l, cdr(l));
            }
            unprotect_ptr(ic->g);
        } else {
            lprint_sexpr(ic, car(s));
        }
        STORE(ic->g, NULL, s, cdr(s));
    }

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *newline(IC *ic, sexpr *s) {
    lprintf(ic, "\n");
    return ic->g_unbound;
}

sexpr *cons_subr(IC *ic, sexpr *s) {
    return cons(ic, car(s), car(cdr(s)));
}

sexpr *car_subr(IC *ic, sexpr *s) {
    if(car(s)->t == CONS)
        return car(car(s));
    else
        return ic->g_nil;
}

sexpr *cdr_subr(IC *ic, sexpr *s) {
    if(car(s)->t == CONS)
        return cdr(car(s));
    else
        return ic->g_nil;
}

/* Used for caching procedures created to wrap template expressions. */
sexpr *cons_proc_cache_subr(IC *ic, sexpr *s) {
    sexpr *pair = car(s);
    if(pair->t != CONS)
        bad_argument(ic, pair);
    return pair->u.cons.proc_cache;
}

sexpr *set_cons_proc_cache_subr(IC *ic, sexpr *s) {
    sexpr *pair = car(s);
    sexpr *new_tree = car(cdr(s));

    if(pair->t != CONS)
        bad_argument(ic, pair);

    STORE(ic->g, pair, pair->u.cons.proc_cache, new_tree);
    return ic->g_unbound;
}

/* Experimental code used for caching treeified versions of lists
   run with RUN.  See (caching_run) in initialize.txt. */
sexpr *cons_tree_cache_subr(IC *ic, sexpr *s) {
    sexpr *pair = car(s);
    if(pair->t != CONS)
        bad_argument(ic, pair);
    return pair->u.cons.tree_cache;
}

sexpr *set_cons_tree_cache_subr(IC *ic, sexpr *s) {
    sexpr *pair = car(s);
    sexpr *new_tree = car(cdr(s));

    if(pair->t != CONS)
        bad_argument(ic, pair);

    STORE(ic->g, pair, pair->u.cons.tree_cache, new_tree);
    return ic->g_unbound;
}

/* Internal gensym.
   Generate an uninterned symbol with the provided name.
   A GENSYM library procedure defaults to no arguments and normally
   behaves like Ucblogo GENSYM (returning interned symbols) but calls
   this if passed an argument. */
sexpr *gensym_subr(IC *ic, sexpr *s) {
    sexpr *name;
    if(s->t == CONS)
        name = car(s);
    else
        name = ic->n_empty;

    if(name->t == NAME)
        return new_name_gc(ic, name->u.name.head,
                               name->u.name.start,
                               name->u.name.length);
    else
        bad_argument(ic, name);
}

/* Create a list containing the arguments.
   How convenient that that's what we're passed as an argument! */
sexpr *list_subr(IC *ic, sexpr *s) {
    return s;
}

/* Is the argument a CONS? */
sexpr *consp(IC *ic, sexpr *s) {
    if(car(s)->t == CONS)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Is the argument a list?
   Both CONS cells and the empty list are lists. */
sexpr *listp(IC *ic, sexpr *s) {
    if(car(s)->t == CONS || car(s)->t == EMPTY_LIST)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Is the argument a word?
   Both names and numbers are words. */
sexpr *wordp(IC *ic, sexpr *s) {
    if(car(s)->t == NAME || car(s)->t == NUMBER)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Is the argument a number? */
sexpr *numberp_sexpr(IC *ic, sexpr *s) {
    if(numberp(ic, car(s)))
        return ic->n_true;
    else
        return ic->n_false;
}

/* Is the argument a NAME with a bound value? */
sexpr *namep(IC *ic, sexpr *s) {
    sexpr *name = to_name(ic, car(s));
    if(name->u.name.symbol->value != ic->g_unbound)
        return ic->n_true;
    else
        return ic->n_false;
}


/* Add numbers. */
sexpr *add(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    double ret = 0;

    while(!is_nil(ic, s)) {
        ret = ret + to_number(ic, car(s))->u.number.value;
        STORE(ic->g, NULL, s, cdr(s));
    }

    unprotect_ptr(ic->g);
    return mk_number(ic, ret);
}

/* Subtract.
   If given no arguments, returns 0.
   If given one argument, return its negation.
   If given multiple arguments, returns the first minus the remaining
   arguments. */
sexpr *sub(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    double ret = 0;

    if(!is_nil(ic, s)) {
        if(is_nil(ic, cdr(s))) {
            ret = -to_number(ic, car(s))->u.number.value;
        } else {
            ret = to_number(ic, car(s))->u.number.value;
            STORE(ic->g, NULL, s, cdr(s));
            while(!is_nil(ic, s)) {
                ret = ret - to_number(ic, car(s))->u.number.value;
                STORE(ic->g, NULL, s, cdr(s));
            }
        }
    }

    unprotect_ptr(ic->g);
    return mk_number(ic, ret);
}

/* Multiply all of the arguments together. */
sexpr *mul(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    double ret = 1;
    while(!is_nil(ic, s)) {
        ret = ret * to_number(ic, car(s))->u.number.value;
        STORE(ic->g, NULL, s, cdr(s));
    }
    unprotect_ptr(ic->g);
    return mk_number(ic, ret);
}

/* Divide two numbers. */
sexpr *div_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    double first = to_number(ic, car(s))->u.number.value;
    double second = to_number(ic, car(cdr(s)))->u.number.value;

    unprotect_ptr(ic->g);
    return mk_number(ic, first/second);
}

/* Take the remainder of first/second.
   Result is the same sign as first. */
sexpr *remainder_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    double first = to_number(ic, car(s))->u.number.value;
    double second = to_number(ic, car(cdr(s)))->u.number.value;
    double ret = fmod(first, second);
    if(first < 0 && ret > 0) ret -= abs(second);
    if(first > 0 && ret < 0) ret += abs(second);

    unprotect_ptr(ic->g);
    return mk_number(ic, ret);
}

/* Take the remainder of first/second.
   Result is the same sign as second. */
sexpr *modulo_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    double first = to_number(ic, car(s))->u.number.value;
    double second = to_number(ic, car(cdr(s)))->u.number.value;
    double ret = fmod(first, second);
    if(second < 0 && ret > 0) ret -= abs(second);
    if(second > 0 && ret < 0) ret += abs(second);

    unprotect_ptr(ic->g);
    return mk_number(ic, ret);
}

static int compare(IC *ic, sexpr *first, sexpr *second) {
    protect_ptr(ic->g, (void **)&first);
    protect_ptr(ic->g, (void **)&second);

    double dfirst = to_number(ic, first)->u.number.value;
    double dsecond = to_number(ic, second)->u.number.value;

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    if(dfirst > dsecond) return 1;
    if(dfirst < dsecond) return -1;
    return 0;
}

/* greater than */
sexpr *gt(IC *ic, sexpr *s) {
    if(compare(ic, car(s), car(cdr(s))) > 0)
        return ic->n_true;
    else
        return ic->n_false;
}


/* less than */
sexpr *lt(IC *ic, sexpr *s) {
    if(compare(ic, car(s), car(cdr(s))) < 0)
        return ic->n_true;
    else
        return ic->n_false;
}

/* greater than or equal */
sexpr *ge(IC *ic, sexpr *s) {
    if(compare(ic, car(s), car(cdr(s))) >= 0)
        return ic->n_true;
    else
        return ic->n_false;
}

/* less than or equal */
sexpr *le(IC *ic, sexpr *s) {
    if(compare(ic, car(s), car(cdr(s))) <= 0)
        return ic->n_true;
    else
        return ic->n_false;
}

/* not equal */
sexpr *ne(IC *ic, sexpr *s) {
    if(compare(ic, car(s), car(cdr(s))) <= 0)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Lisp reader */
sexpr *read_subr(IC *ic, sexpr *s) {
    return readobj(ic);
}

/* Logo reader */
sexpr *logoread_subr(IC *ic, sexpr *s) {
    return logoreadobj(ic->lr);
}

/* Read a line of input as a Logo list */
sexpr *readlist_subr(IC *ic, sexpr *s) {
    return readlist(ic->lr);
}

/* Read a line of input as a single Logo word */
sexpr *readword_subr(IC *ic, sexpr *s) {
    return readword(ic->lr);
}

/* Read a raw line of input as a single Logo word */
sexpr *readrawline_subr(IC *ic, sexpr *s) {
    return readrawline(ic->lr);
}

/* Read a character */
sexpr *readchar_subr(IC *ic, sexpr *s) {
    return readchar(ic->lr);
}

/* Read a given number of characters */
sexpr *readchars_subr(IC *ic, sexpr *s) {
    return readchars(ic->lr, to_number(ic, car(s))->u.number.value);
}

/* Read a line and create a LINE, which contains a procedure name
   (supplied as an argument), the raw line (a word) and the parsed
   line (a list).
   If reading from the terminal, prompts with a prompt (another argument). */
sexpr *readline_subr(IC *ic, sexpr *s) {
    return readline(ic->lr, car(s), car(cdr(s)));
}

/* Is there a keystroke waiting for us to read it? */
sexpr *keyp_subr(IC *ic, sexpr *s) {
    return keyp(ic->lr);
}

/* Return a list of interned names. */
sexpr *oblist(IC *ic, sexpr *s) {
    sexpr *ret = cons(ic, ic->g_nil, ic->g_nil);
    protect_ptr(ic->g, (void **)&ret);

    sexpr *tail = ret;
    protect_ptr(ic->g, (void **)&tail);

    int i;

    for(i = 0; i < NAME_TABLE_HASH_BUCKETS; i++) {
        sexpr *e = ic->name_table[i];
        protect_ptr(ic->g, (void **)&e);

        while(!is_nil(ic, e)) {
            if(!is_nil(ic, car(e)))
                push_back(ic, &tail, car(e));
            STORE(ic->g, NULL, e, cdr(e));
        }
        unprotect_ptr(ic->g);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return cdr(ret);
}

/* Manually run the garbage collector. */
sexpr *collect_garbage_subr(IC *ic, sexpr *s) {
    collect_garbage(ic->g);
    return ic->g_unbound;
}

/* The internal version of FIRST.
   This version, which takes the actual argument as e, is exported
   for use elsewhere in the interpreter.
 */
sexpr *first(IC *ic, sexpr *e) {
    protect_ptr(ic->g, (void **)&e);

    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **)&ret);

    /* Return the first element of a list */
    if(e->t == CONS) {
        STORE(ic->g, NULL, ret, car(e));
        goto end;
    }

    /* Return the origin, also known as the first element's index. */
    if(e->t == ARRAY) {
        STORE(ic->g, NULL, ret, mk_number(ic, e->u.array.origin));
        goto end;
    }

    /* Attempt to convert the argument to a name and return a name
       containing only the first letter of the argument. */
    STORE(ic->g, NULL, e, to_name(ic, e));
    if(e->u.name.length >= 1) {
        STORE(ic->g, NULL, ret, 
              intern_len_gc(ic, e->u.name.head, e->u.name.start, 1));
        goto end;
    }

    bad_argument(ic, e);

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* The SUBR for FIRST. */
sexpr *first_subr(IC *ic, sexpr *s) {
    return first(ic, car(s));
}

/* The internal version of BUTFIRST.
   This version, which takes the actual argument as e, is exported
   for use elsewhere in the interpreter.
 */
sexpr *butfirst(IC *ic, sexpr *e) {
    protect_ptr(ic->g, (void **)&e);

    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **)&ret);

    /* cdr of a list */
    if(e->t == CONS) {
        STORE(ic->g, NULL, ret, cdr(e));
        goto end;
    }

    /* For names, create a new name with everything but the first
       character. */
    STORE(ic->g, NULL, e, to_name(ic, e));
    if(e->t == NAME) {
        if(e->u.name.length >= 1) {
            STORE(ic->g, NULL, ret,
                  intern_len_gc(ic, e->u.name.head, e->u.name.start+1, e->u.name.length-1));
            goto end;
        }
    }

    bad_argument(ic, e);

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* SUBR for BUTFIRST. */
sexpr *butfirst_subr(IC *ic, sexpr *s) {
    return butfirst(ic, car(s));
}

/* LAST - fetch the last member of a list or name. */
sexpr *last(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *e = car(s);
    protect_ptr(ic->g, (void **)&e);

    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **)&ret);

    if(e->t == CONS) {
        while(e->t == CONS && !is_nil(ic, cdr(e)))
            STORE(ic->g, NULL, e, cdr(e));
        if(e->t == CONS) {
            STORE(ic->g, NULL, ret, car(e));
            goto end;
        }
    }
    STORE(ic->g, NULL, e, to_name(ic, e));

    if(e->u.name.length >= 1) {
        STORE(ic->g, NULL, ret,
              intern_len_gc(ic,
                            e->u.name.head,
                            e->u.name.start+e->u.name.length-1,
                            1));
        goto end;
    }
    bad_argument(ic, car(s));

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* BUTLAST - Return all but the last element of a list or a name. */
sexpr *butlast(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *e = car(s);
    protect_ptr(ic->g, (void **)&e);

    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **)&ret);

    if(e->t == CONS) {
        /* It's a list.  Create a new list destructively so we can
           do it in a loop. */
        STORE(ic->g, NULL, ret, cons(ic, ic->g_nil, ic->g_nil));
        sexpr *tail = ret;
        protect_ptr(ic->g, (void **)&tail);

        while(e->t == CONS && !is_nil(ic, cdr(e))) {
            push_back(ic, &tail, car(e));
            STORE(ic->g, NULL, e, cdr(e));
        }

        unprotect_ptr(ic->g);

        STORE(ic->g, NULL, ret, cdr(ret));
        goto end;
    }

    /* Try to convert it to a name and create a new name not containing
       the last character. */
    STORE(ic->g, NULL, e, to_name(ic, e));
    if(e->u.name.length >= 1) {
        STORE(ic->g, NULL, ret, intern_len_gc(ic,
                                e->u.name.head,
                                e->u.name.start,
                                e->u.name.length-1));
        goto end;
    }
    bad_argument(ic, car(s));

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Merge zero or more words into one word. */
sexpr *word(IC *ic, sexpr *e) {
    protect_ptr(ic->g, (void **)&e);

    char *new_word = NULL;
    protect_ptr(ic->g, (void **)&new_word);

    sexpr *current = NULL;
    protect_ptr(ic->g, (void **)&current);

    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **)&ret);

    int i, len;

    /* Calculate the full length the new name will need. */
    STORE(ic->g, NULL, current, e);
    len = 0;
    while(!is_nil(ic, current)) {
        len += to_name(ic, car(current))->u.name.length;
        STORE(ic->g, NULL, current, cdr(current));
    }

    /* Create the char array for the new name */
    STORE(ic->g, NULL, new_word, ic_xmalloc(ic, len, mark_cstring));

    STORE(ic->g, NULL, current, e);
    i = 0;
    while(!is_nil(ic, current)) {
        sexpr *tmp = to_name(ic, car(current));
        protect_ptr(ic->g, (void **)&tmp);

        strncpy(new_word+i, tmp->u.name.head+tmp->u.name.start, 
                            tmp->u.name.length);
        i += tmp->u.name.length;
        
        unprotect_ptr(ic->g);

        STORE(ic->g, NULL, current, cdr(current));
    }
    STORE(ic->g, NULL, ret, intern_len_gc(ic, new_word, 0, len));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Is the argument the empty word or the empty list? */
sexpr *emptyp(IC *ic, sexpr *s) {
    /* There's only ever one empty name, so just do a pointer equality
       test. */
    if(is_nil(ic, car(s)) || car(s) == ic->n_empty)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Merge multiple things into one sentence.
   Sentences in the arguments get flattened. */
sexpr *sentence(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *ret = cons(ic, ic->g_nil, ic->g_nil);
    protect_ptr(ic->g, (void **)&ret);

    sexpr *tail = ret;
    protect_ptr(ic->g, (void **)&tail);

    while(!is_nil(ic, s)) {
        if(car(s)->t == CONS) {
            sexpr *e = car(s);
            protect_ptr(ic->g, (void **)&e);
            while(e->t == CONS) {
                push_back(ic, &tail, car(e));
                STORE(ic->g, NULL, e, cdr(e));
            }
            unprotect_ptr(ic->g);
        } else if(!is_nil(ic, car(s))) {
            push_back(ic, &tail, car(s));
        }
        STORE(ic->g, NULL, s, cdr(s));
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return cdr(ret);
}

/* FPUT - the Logo version of cons.
   Requires the second argument to be a list. */
sexpr *fput_subr(IC *ic, sexpr *s) {
    sexpr *thing = car(s);
    sexpr *list = car(cdr(s));
    if(list->t != CONS && list->t != EMPTY_LIST)
        bad_argument(ic, list);

    return cons(ic, thing, list);
}

/* COMBINE - Combine thing1 with thing2.
             If thing2 is a word, outputs WORD thing1 thing2.
             If thing2 is a list, outputs FPUT thing1 thing2.
 */
sexpr *combine(IC *ic, sexpr *s) {
    sexpr *thing2 = car(cdr(s));

    if(thing2->t == NAME || thing2->t == NUMBER)
        return word(ic, s);
    else
        return fput_subr(ic, s);
}


/* Make treeify available for use in initialize.txt. */
sexpr *treeify_subr(IC *ic, sexpr *s) {
    return treeify(ic, car(s));
}

/* Generating random integers.
   RANDOM N -> X   such that 0 <= X <= N-1
   RANDOM N M -> X such that N <= X <= M
 */
sexpr *random_subr(IC *ic, sexpr *s) {
    if(is_nil(ic, cdr(s))) {
        int num = (int)abs(trunc(to_number(ic, car(s))->u.number.value));
        if(num > 0)
            return mk_number(ic, random() % num);
        else
            return mk_number(ic, 0);
    } else {
        protect_ptr(ic->g, (void **)&s);
        int num1 = (int)trunc(to_number(ic, car(s))->u.number.value);
        int num2 = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
        unprotect_ptr(ic->g);

        int range;
        if(num1 > num2)
            return mk_number(ic, 0);
        /* 0 <= random() % range <= num2 - num1
           If random() % range == 0
             num1 + (random() % range) = num1, the lower end of the output
           If random() % range == num2 - num1
             num1 + (random() % range) = num2, the upper end of the output
         */
        range = num2 - num1 + 1;
        return mk_number(ic, (random() % range) + num1);
    }
}

/* RERANDOM - Logo interface to srandom. */
sexpr *rerandom_subr(IC *ic, sexpr *s) {
    if(is_nil(ic, s))
        srandom(0);
    else
        srandom((int)abs(trunc(to_number(ic, car(s))->u.number.value)));
    return ic->g_unbound;
}

/* Extend the current frame, if necessary, to ensure that the name
   or names passed are local variables in the current frame. */
sexpr *local(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    /* For handling the one argument version where the argument is
       a list of names. */
    if(s->t == CONS && (car(s)->t == CONS || car(s)->t == EMPTY_LIST))
        STORE(ic->g, NULL, s, car(s));

    while(s->t == CONS) {
        sexpr *n = to_name(ic, car(s));
        protect_ptr(ic->g, (void **)&n);
        add_local(ic, ic->continuation->frame, n);
        unprotect_ptr(ic->g);
        STORE(ic->g, NULL, s, cdr(s));
    }
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

/* Fetch the value of the name passed. */
sexpr *thing(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *name = to_name(ic, car(s));
    protect_ptr(ic->g, (void **)&name);

    if(name->u.name.symbol->value == ic->g_unbound) {
        eprint_sexpr(ic, car(s));
        eprintf(ic, " has no value");
        throw_error(ic, ic->continuation->line);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return name->u.name.symbol->value;
}

/* Make a LINE object.
   Used while reading in a Logo procedure.
   Arguments are:
       raw_line     - A word containing the entire line.
       parsed_line  - A sentence containing the parsed line.
       procedure    - The procedure containing the line.
 */
sexpr *mk_line_subr(IC *ic, sexpr *s) {
    return mk_line(ic, car(s), car(cdr(s)), car(cdr(cdr(s))));
}

sexpr *linep(IC *ic, sexpr *s) {
    if(car(s)->t == LINE)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Fetch the line's raw representation. */
sexpr *line_raw(IC *ic, sexpr *s) {
    if(car(s)->t == LINE)
        return car(s)->u.line.raw_line;
    else
        bad_argument(ic, car(s));
}

/* Fetch the line's parsed representation. */
sexpr *line_parsed(IC *ic, sexpr *s) {
    if(car(s)->t == LINE)
        return car(s)->u.line.parsed_line;
    else
        bad_argument(ic, car(s));
}

/* Fetch the line's procedure. */
sexpr *line_procedure(IC *ic, sexpr *s) {
    if(car(s)->t == LINE)
        return car(s)->u.line.procedure;
    else
        bad_argument(ic, car(s));
}

/* Used by (treeify_body) in initialize.txt to set the current line
   before calling (treeify) so that error messages from (treeify) will
   have the line being parsed and not the line being executed. */
sexpr *set_current_line(IC *ic, sexpr *s) {
    if(car(s)->t == LINE) {
        if(ic->continuation->parent != NULL) {
            STORE(ic->g, ic->continuation->parent, ic->continuation->parent->line, car(s));
        }
        return ic->g_unbound;
    } else {
        bad_argument(ic, car(s));
    }
}

/* Create a one character word by ascii value.
   CHAR 65 -> A
 */
sexpr *char_subr(IC *ic, sexpr *s) {
    char ch = trunc(to_number(ic, car(s))->u.number.value);
    return intern_len_static(ic, &ch, 0, 1);
}

/* File management.  Mostly wrappers around routines in io.c */
sexpr *openread_subr(IC *ic, sexpr *s) {
    openread(ic, car(s));
    return ic->g_unbound;
}

sexpr *openwrite_subr(IC *ic, sexpr *s) {
    openwrite(ic, car(s));
    return ic->g_unbound;
}

sexpr *openappend_subr(IC *ic, sexpr *s) {
    openappend(ic, car(s));
    return ic->g_unbound;
}

sexpr *openupdate_subr(IC *ic, sexpr *s) {
    openupdate(ic, car(s));
    return ic->g_unbound;
}

sexpr *close_subr(IC *ic, sexpr *s) {
    lclose(ic, car(s));
    return ic->g_unbound;
}

/* EOFP defaults to Ucblogo behavior of reading and putting back a character.
   If passed an argument, tests whether the argument is an EOF object.
   The version with an argument can be used in interactive applications
   to avoid an appearance of hanging between lines. */
sexpr *eofp_subr(IC *ic, sexpr *s) {
    if(s->t == CONS) {
        if(car(s) == ic->eof)
            return ic->n_true; 
        else
            return ic->n_false;
    } else {
        return leofp(ic);
    }
}

sexpr *dribble_subr(IC *ic, sexpr *s) {
    dribble(ic, car(s));
    return ic->g_unbound;
}

sexpr *nodribble_subr(IC *ic, sexpr *s) {
    nodribble(ic);
    return ic->g_unbound;
}

sexpr *setread_subr(IC *ic, sexpr *s) {
    setread(ic, car(s));
    return ic->g_unbound;
}

sexpr *setwrite_subr(IC *ic, sexpr *s) {
    setwrite(ic, car(s));
    return ic->g_unbound;
}

sexpr *allopen_subr(IC *ic, sexpr *s) {
    return allopen(ic);
}

sexpr *closeall_subr(IC *ic, sexpr *s) {
    closeall(ic);
    return ic->g_unbound;
}

sexpr *erasefile_subr(IC *ic, sexpr *s) {
    erasefile(ic, car(s));
    return ic->g_unbound;
}

sexpr *reader_subr(IC *ic, sexpr *s) {
    return lreader(ic);
}

sexpr *writer_subr(IC *ic, sexpr *s) {
    return writer(ic);
}

sexpr *readpos_subr(IC *ic, sexpr *s) {
    return readpos(ic);
}

sexpr *writepos_subr(IC *ic, sexpr *s) {
    return writepos(ic);
}

sexpr *setreadpos_subr(IC *ic, sexpr *s) {
    setreadpos(ic, car(s));
    return ic->g_unbound;
}

sexpr *setwritepos_subr(IC *ic, sexpr *s) {
    setwritepos(ic, car(s));
    return ic->g_unbound;
}

sexpr *filep_subr(IC *ic, sexpr *s) {
    return filep(ic, car(s));
}

/* Property list procedures.  Mostly wrappers around the procedures in
   list_memory.c */
sexpr *pprop_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *var = NULL, *tag = NULL, *value = NULL;
    protect_ptr(ic->g, (void **) &var);
    protect_ptr(ic->g, (void **) &tag);
    protect_ptr(ic->g, (void **) &value);

    STORE(ic->g, NULL, var, to_name(ic, car(s)));
    STORE(ic->g, NULL, tag, to_name(ic, car(cdr(s))));
    STORE(ic->g, NULL, value, car(cdr(cdr(s))));

    if(var->t == NAME &&
       var->u.name.symbol->flags & PLIST_TRACED) {
        sexpr *old_fullprint = ic->n_fullprintp->u.name.symbol->value;
        protect_ptr(ic->g, (void **) &old_fullprint);
        STORE(ic->g,
              ic->n_fullprintp->u.name.symbol,
              ic->n_fullprintp->u.name.symbol->value,
              ic->n_true);

        tprintf(ic, "Pprop \"");
        tprint_sexpr(ic, var);
        tprintf(ic, " ");

        if(tag->t == NAME)
            tprintf(ic, "\"");
        tprint_sexpr(ic, tag);
        tprintf(ic, " ");

        if(value->t == NAME)
            tprintf(ic, "\"");
        tprint_sexpr(ic, value);
        tprintf(ic, "\n");

        STORE(ic->g,
              ic->n_fullprintp->u.name.symbol,
              ic->n_fullprintp->u.name.symbol->value,
              old_fullprint);
        unprotect_ptr(ic->g);
    }

    pprop(ic, var, tag, value);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *gprop_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *var = NULL, *tag = NULL, *ret = NULL;
    protect_ptr(ic->g, (void **) &var);
    protect_ptr(ic->g, (void **) &tag);

    STORE(ic->g, NULL, var, to_name(ic, car(s)));
    STORE(ic->g, NULL, tag, to_name(ic, car(cdr(s))));

    ret = gprop(ic, var, tag);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

sexpr *remprop_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *var = NULL, *tag = NULL;
    protect_ptr(ic->g, (void **) &var);
    protect_ptr(ic->g, (void **) &tag);

    STORE(ic->g, NULL, var, to_name(ic, car(s)));
    STORE(ic->g, NULL, tag, to_name(ic, car(cdr(s))));

    remprop(ic, var, tag);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *plist_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *var = NULL, *ret = NULL;
    protect_ptr(ic->g, (void **) &var);
    STORE(ic->g, NULL, var, to_name(ic, car(s)));
    ret = plist(ic, var);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Fetch the last error caught with CATCH "ERROR [...] */
sexpr *error_subr(IC *ic, sexpr *s) {
    sexpr *ret;
    if(ic->n_error->u.name.symbol->value == ic->g_unbound)
        return ic->g_nil;
    ret = ic->n_error->u.name.symbol->value;
    STORE(ic->g, ic->n_error->u.name.symbol,
                 ic->n_error->u.name.symbol->value,
                 ic->g_unbound);
    return ret;
}

/* Low level interface for throwing an error.  Used by
   THROW "ERROR ... in initialize.txt */
sexpr *raise_error_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    if(s->t == CONS)
        eprint_sexpr(ic, car(s));
    throw_error(ic, ic->continuation->line);

    /* Not reached */
    unprotect_ptr(ic->g);
}

/* Creates a MACRO object.
   The first argument indicates whether we are creating a Lisp macro
   (arguments are not evaluated) or a Logo macro (arguments are evaluated).
   The second argument is the procedure being wrapped. */
sexpr *macro_subr(IC *ic, sexpr *s) {
    sexpr *macro_type = car(s);
    sexpr *expander = car(cdr(s));

    if(!name_eq(macro_type, ic->n_lisp_macro) &&
       !name_eq(macro_type, ic->n_logo_macro))
        bad_argument(ic, macro_type);

    return mk_macro(ic, macro_type, expander);
}

/* Parse a word as if read by READLIST. */
sexpr *parse_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    logoreader *lr = mk_logoreader(ic);
    protect_ptr(ic->g, (void **) &lr);

    sexpr *name = to_name(ic, car(s));
    protect_ptr(ic->g, (void **) &name);

    char *wordstring = get_cstring(ic, name);
    protect_ptr(ic->g, (void **) &wordstring);

    logoread_from_string(lr, wordstring);

    sexpr *ret = readlist(lr);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}



/* These are the helper functions that manipulate flags.  They are used
   by BURY,  UNBURY,  BURIEDP,
      TRACE, UNTRACE, TRACEDP, 
      STEP,  UNSTEP,  STEPPEDP
 */

/* flag_helper() sets or clears a flag or flags based on mask and flag */
static void flag_helper(IC *ic, sexpr *n, int mask, int flag,
                        sexpr *orig_arg) {
    if(n->t != NAME)
        bad_argument(ic, orig_arg);
    n->u.name.symbol->flags =
        (n->u.name.symbol->flags & mask) | flag;
}

/* Maps flag_helper over a list of names. */
static void flaglist_helper(IC *ic, sexpr *l, int mask, int flag,
                            sexpr *orig_arg) {
    while(!is_nil(ic, l) && l->t == CONS && car(l)->t == NAME) {
        flag_helper(ic, car(l), mask, flag, orig_arg);
        l = cdr(l); /* Note STORE() since l is not protected. */
    }
    if(!is_nil(ic, l))
        bad_argument(ic, orig_arg);
}


/* contentslist_helper maps flaglist_helper over up to three lists
   with up to three different sets of masks and flags.

   Argument is:
       A symbol - symbol is processed as a procedure.
       A list of symbols - symbols are processed as procedures.
       A list of up to three lists
           First processed as procedures
           Second processed as variables
           Third processed as property lists
 */
static void contentslist_helper(
                IC *ic,
                sexpr *args,
                int procmask,
                int procflag,
                int varmask,
                int varflag,
                int plmask,
                int plflag) {
    protect_ptr(ic->g, (void **)&args);
    sexpr *arg = car(args);
    protect_ptr(ic->g, (void **)&arg);
    sexpr *orig_arg = arg; /* In case we need to throw an error */
    protect_ptr(ic->g, (void **)&orig_arg);

    if(arg->t == NAME) {
        flaglist_helper(ic, args, procmask, procflag, orig_arg);
    } else if(arg->t == CONS && car(arg)->t == NAME) {
        flaglist_helper(ic, arg, procmask, procflag, orig_arg);
    } else {
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                flaglist_helper(ic, car(arg), procmask, procflag, orig_arg);
            STORE(ic->g, NULL, arg, cdr(arg));
        }
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                flaglist_helper(ic, car(arg), varmask, varflag, orig_arg);
            STORE(ic->g, NULL, arg, cdr(arg));
        }
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                flaglist_helper(ic, car(arg), plmask, plflag, orig_arg);
            STORE(ic->g, NULL, arg, cdr(arg));
        }
        if(!is_nil(ic, arg))
            bad_argument(ic, orig_arg);
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

/* Tests one flag in one name */
static sexpr *testflag_helper(IC *ic, sexpr *name, int flag, sexpr *orig_arg) {
    if(name->t != NAME)
        bad_argument(ic, orig_arg);
    if(name->u.name.symbol->flags & flag)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Tests the appropriate flag against the first symbol found in
   the contentslist */
static sexpr *testlist_subr(IC *ic, sexpr *arg,
                            int procflag, int varflag, int plflag) {

    if(arg->t == NAME)
        /* Argument is a name.  Test procflag. */
        return testflag_helper(ic, arg, procflag, arg);
    else if(arg->t == CONS && car(arg)->t == NAME)
        /* Argument is a list with a name.  Test procflag. */
        return testflag_helper(ic, car(arg), procflag, arg);
    else if(arg->t == CONS &&
            car(arg)->t == CONS &&
            car(car(arg))->t == NAME)
        /* Argument is a list whose first list contians a name. Test procflag. */
        return testflag_helper(ic, car(car(arg)), procflag, arg);
    else if(arg->t == CONS &&
            cdr(arg)->t == CONS &&
            car(cdr(arg))->t == CONS &&
            car(car(cdr(arg)))->t == NAME)
        /* Argument is a list whose second list contains a name.
           Test varflag. */
        return testflag_helper(ic, car(car(cdr(arg))), varflag, arg);
    else if(arg->t == CONS &&
            cdr(arg)->t == CONS &&
            cdr(cdr(arg))->t == CONS &&
            car(cdr(cdr(arg)))->t == CONS &&
            car(car(cdr(cdr(arg))))->t == NAME)
        /* Argument is a list whose third list contains a name. Test plflag. */
        return testflag_helper(ic, car(car(cdr(cdr(arg)))), plflag, arg);
    else
        bad_argument(ic, arg);
}

/* The SUBR for BURY.
   A mask of ~0 means keep all existing flags and just add the supplied
   flag. */
sexpr *bury_subr(IC *ic, sexpr *s) {
    contentslist_helper(ic, s,
                        ~0, PROC_BURIED,
                        ~0, VAL_BURIED,
                        ~0, PLIST_BURIED);
    return ic->g_unbound;
}

/* The SUBR for UNBURY.
   A mask of ~PROC_BURIED means remove the PROC_BURIED flag (if present). */
sexpr *unbury_subr(IC *ic, sexpr *s) {
    contentslist_helper(ic, s,
                        ~PROC_BURIED,  0,
                        ~VAL_BURIED,   0,
                        ~PLIST_BURIED, 0);
    return ic->g_unbound;
}

/* Is the first item in the supplied contentslist buried? */
sexpr *buriedp_subr(IC *ic, sexpr *s) {
    return testlist_subr(ic, car(s), PROC_BURIED, VAL_BURIED, PLIST_BURIED);
}

/* TRACE */
sexpr *trace_subr(IC *ic, sexpr *s) {
    contentslist_helper(ic, s,
                        ~0, PROC_TRACED,
                        ~0, VAL_TRACED,
                        ~0, PLIST_TRACED);
    return ic->g_unbound;
}

/* UNTRACE */
sexpr *untrace_subr(IC *ic, sexpr *s) {
    contentslist_helper(ic, s,
                        ~PROC_TRACED,  0,
                        ~VAL_TRACED,   0,
                        ~PLIST_TRACED, 0);
    return ic->g_unbound;
}

/* Is the first item in the supplied contentslist traced? */
sexpr *tracedp_subr(IC *ic, sexpr *s) {
    return testlist_subr(ic, car(s), PROC_TRACED, VAL_TRACED, PLIST_TRACED);
}


/* It is not meaningful for a variable or a property list to be stepped,
   but the flags exist to let us more easily reuse the helper procedures
   above. */

/* STEP */
sexpr *step_subr(IC *ic, sexpr *s) {
    contentslist_helper(ic, s,
                        ~0, PROC_STEPPED,
                        ~0, VAL_STEPPED,
                        ~0, PLIST_STEPPED);
    return ic->g_unbound;
}

/* UNSTEP */
sexpr *unstep_subr(IC *ic, sexpr *s) {
    contentslist_helper(ic, s,
                        ~PROC_STEPPED,  0,
                        ~VAL_STEPPED,   0,
                        ~PLIST_STEPPED, 0);
    return ic->g_unbound;
}

/* Is the first item in the supplied contentslist stepped? */
sexpr *steppedp_subr(IC *ic, sexpr *s) {
    return testlist_subr(ic, car(s), PROC_STEPPED, VAL_STEPPED, PLIST_STEPPED);
}

/* We erase a name by setting its value to ic->g_unbound.
   This is what the value is set to by default. */
static void erase_name(IC *ic, sexpr *name, sexpr *orig_arg) {
    if(name->t != NAME)
        bad_argument(ic, orig_arg);

    STORE(ic->g, name->u.name.symbol, name->u.name.symbol->value, ic->g_unbound);
}

/* When erasing a procedure, we also need to wipe out the stored source code.
 */
static void erase_procedure(IC *ic, sexpr *name, sexpr *orig_arg) {
    if(name->t != NAME)
        bad_argument(ic, orig_arg);

    STORE(ic->g, name->u.name.symbol, name->u.name.symbol->function, ic->g_unbound);
    STORE(ic->g, name->u.name.symbol, name->u.name.symbol->function_source, ic->g_nil);
}

/* To erase a property list, just set it to the empty list. */
static void erase_plist(IC *ic, sexpr *name, sexpr *orig_arg) {
    if(name->t != NAME)
        bad_argument(ic, orig_arg);

    STORE(ic->g, name->u.name.symbol, name->u.name.symbol->properties, ic->g_nil);
}

/* Erase all members of the passed contentslist. */
sexpr *erase_subr(IC *ic, sexpr *args) {
    sexpr *arg = car(args);
    sexpr *orig_arg = arg; /* In case we need to throw an error */
    sexpr *s;

    if(arg->t == NAME) {
        erase_procedure(ic, arg, orig_arg);
    } else if(arg->t == CONS && car(arg)->t == NAME) {
        for(s = arg; !is_nil(ic, s); s = cdr(s))
            erase_procedure(ic, car(s), orig_arg);
    } else {
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                for(s = car(arg); !is_nil(ic, s); s = cdr(s))
                    erase_procedure(ic, car(s), orig_arg);
            arg = cdr(arg);
        }
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                for(s = car(arg); !is_nil(ic, s); s = cdr(s))
                    erase_name(ic, car(s), orig_arg);
            arg = cdr(arg);
        }
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                for(s = car(arg); !is_nil(ic, s); s = cdr(s))
                    erase_plist(ic, car(s), orig_arg);
            arg = cdr(arg);
        }
        if(!is_nil(ic, arg))
            bad_argument(ic, orig_arg);
    }

    return ic->g_unbound;
}

/* OPENSHELL - wrapper for openshell() in io.c */
sexpr *openshell_subr(IC *ic, sexpr *s) {
    openshell(ic, car(s), car(cdr(s)));
    return ic->g_unbound;
}

/* SYSTEM - wrapper for system() in io.c */
sexpr *system_subr(IC *ic, sexpr *s) {
    return systemf(ic, car(s));
}

/* BYE - exit the interpreter. */
sexpr *bye_subr(IC *ic, sexpr *s) {
    closeall(ic);
    tprintf(ic, "Exiting.\n");
    longjmp(ic->quit, 1);
}

/* Fetch an environmental variable. */
sexpr *getenv_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    char *cstring = get_cstring(ic, to_name(ic, car(s)));
    protect_ptr(ic->g, (void **)&cstring);
    char *value = getenv(cstring); /* Static, not protected */

    sexpr *ret;

    if(value == NULL)
        ret = ic->g_nil;
    else
        ret = intern(ic, value);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

/* How many garbage collector nodes are allocated? */
sexpr *nodes_subr(IC *ic, sexpr *s) {
    return mk_number(ic, ic->g->node_count);
}

/* How deep is the continuation stack?
   Useful for debugging tail call optimizations. */
sexpr *continuation_depth_subr(IC *ic, sexpr *s) {
    int i = 0;
    struct continuation *c = ic->continuation;
    while(c != NULL) {
        i++;
        c = c->parent;
    }
    return mk_number(ic, i);
}

/* How deep is the environment frame stack?
   Useful for debugging tail call optimizations. */
sexpr *frame_depth_subr(IC *ic, sexpr *s) {
    int i = 0;
    struct frame *f = ic->continuation->frame;
    while(f != NULL) {
        i++;
        f = f->parent;
    }
    return mk_number(ic, i);
}

/* What is the minimum, default, and maximum arity of the argument? */
sexpr *arity_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *arg = car(s);
    protect_ptr(ic->g, (void **)&arg);

    sexpr *proc = NULL, *funarg = NULL, *macro = NULL, *oper = NULL;
    sexpr *min = NULL, *def = NULL, *max = NULL;

    protect_ptr(ic->g, (void **)&proc);
    protect_ptr(ic->g, (void **)&funarg);
    protect_ptr(ic->g, (void **)&macro);
    protect_ptr(ic->g, (void **)&oper);
    protect_ptr(ic->g, (void **)&min);
    protect_ptr(ic->g, (void **)&def);
    protect_ptr(ic->g, (void **)&max);

    if(!get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 1))
        bad_argument(ic, arg);

    if(is_nil(ic, proc))
        bad_argument(ic, arg);

    STORE(ic->g, NULL, min, mk_number(ic, proc->u.proc.minargs));
    STORE(ic->g, NULL, def, mk_number(ic, proc->u.proc.defargs));

    if(proc->u.proc.maxargs == INT_MAX)
        STORE(ic->g, NULL, max, mk_number(ic, -1));
    else
        STORE(ic->g, NULL, max, mk_number(ic, proc->u.proc.maxargs));

    sexpr *ret = listl(ic, min, def, max, NULL);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Is the argument the name of a procedure of any kind? */
sexpr *procedurep_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *arg = car(s);
    protect_ptr(ic->g, (void **)&arg);

    sexpr *proc = NULL, *funarg = NULL, *macro = NULL, *oper = NULL;
    protect_ptr(ic->g, (void **)&proc);
    protect_ptr(ic->g, (void **)&funarg);
    protect_ptr(ic->g, (void **)&macro);
    protect_ptr(ic->g, (void **)&oper);

    sexpr *ret = NULL;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, oper))
        ret = ic->n_false;
    else
        ret = ic->n_true;

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

/* Is the argument the name of a primitive?
   We cheat a little here.  Procedures defined in Lisp are
   considered primitives because you can't get their TEXT in
   Logo. */
sexpr *primitivep_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *arg = car(s);
    protect_ptr(ic->g, (void **)&arg);
    sexpr *proc = NULL, *funarg = NULL, *macro = NULL, *oper = NULL;
    protect_ptr(ic->g, (void **)&proc);
    protect_ptr(ic->g, (void **)&funarg);
    protect_ptr(ic->g, (void **)&macro);
    protect_ptr(ic->g, (void **)&oper);

    sexpr *ret = NULL;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, oper) ||
       !is_nil(ic, arg->u.name.symbol->function_source))
        ret = ic->n_false;
    else
        ret = ic->n_true;

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

/* Is the argument the name of a procedure that was defined in Logo? */
sexpr *definedp_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *arg = car(s);
    protect_ptr(ic->g, (void **)&arg);
    sexpr *proc = NULL, *funarg = NULL, *macro = NULL, *oper = NULL;
    protect_ptr(ic->g, (void **)&proc);
    protect_ptr(ic->g, (void **)&funarg);
    protect_ptr(ic->g, (void **)&macro);
    protect_ptr(ic->g, (void **)&oper);

    sexpr *ret = NULL;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, oper) ||
       is_nil(ic, arg->u.name.symbol->function_source))
        ret = ic->n_false;
    else
        ret = ic->n_true;

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

/* Is the argument the name of a macro? */
sexpr *macrop_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *arg = car(s);
    protect_ptr(ic->g, (void **)&arg);
    sexpr *proc = NULL, *funarg = NULL, *macro = NULL, *oper = NULL;
    protect_ptr(ic->g, (void **)&proc);
    protect_ptr(ic->g, (void **)&funarg);
    protect_ptr(ic->g, (void **)&macro);
    protect_ptr(ic->g, (void **)&oper);

    sexpr *ret = NULL;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, macro) ||
       is_nil(ic, arg->u.name.symbol->function_source))
        ret = ic->n_false;
    else
        ret = ic->n_true;

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

/* COPYDEF "FOO "BAR
   Makes FOO's behavior as a procedure be a copy of BAR's
   behavior as a procedure. */
sexpr *copydef_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *new_s = car(s);
    protect_ptr(ic->g, (void **)&new_s);
    sexpr *old = car(cdr(s));
    protect_ptr(ic->g, (void **)&old);

    sexpr *newproc = NULL, *newfunarg = NULL, *newmacro = NULL, *newoper = NULL;
    sexpr *oldproc = NULL, *oldfunarg = NULL, *oldmacro = NULL, *oldoper = NULL;

    protect_ptr(ic->g, (void **)&newproc);
    protect_ptr(ic->g, (void **)&newfunarg);
    protect_ptr(ic->g, (void **)&newmacro);
    protect_ptr(ic->g, (void **)&newoper);
    protect_ptr(ic->g, (void **)&oldproc);
    protect_ptr(ic->g, (void **)&oldfunarg);
    protect_ptr(ic->g, (void **)&oldmacro);
    protect_ptr(ic->g, (void **)&oldoper);

    /* Fetch the default arity of the two procedures and compare them.
       If they are different, we need to reassign
       treeify_cache_generation to invalidate the cached treeified bodies
       of all procedures. */
    if(get_apply_parts(ic, new_s, &newproc, &newfunarg, &newmacro, &newoper, 0)
       && !is_nil(ic, newproc)) {
        if(!get_apply_parts(ic, old, 
                           &oldproc, &oldfunarg, &oldmacro, &oldoper, 1) ||
           is_nil(ic, oldproc)) {
            bad_argument(ic, old);
        } else {
            int newdefargs = new_s->u.proc.defargs;
            int olddefargs = old->u.proc.defargs;
            if(newdefargs != olddefargs) {
                sexpr *new_gen = cons(ic, ic->g_nil, ic->g_nil);
                protect_ptr(ic->g, (void **)&new_gen);
                STORE(ic->g,
                      ic->n_treeify_cache_generation->u.name.symbol,
                      ic->n_treeify_cache_generation->u.name.symbol->value,
                      new_gen);
                unprotect_ptr(ic->g);
            }
        }
        
    }



    if(new_s->t != NAME)
        bad_argument(ic, new_s);
    if(old->t != NAME)
        bad_argument(ic, old);



    STORE(ic->g, new_s->u.name.symbol,
                 new_s->u.name.symbol->function,
                 old->u.name.symbol->function);
    STORE(ic->g, new_s->u.name.symbol,
                 new_s->u.name.symbol->function_source,
                 old->u.name.symbol->function_source);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

/* FORM - Create a formated version of a number. */
sexpr *form_subr(IC *ic, sexpr *args) {
    protect_ptr(ic->g, (void **)&args);

    char *buf = NULL;
    protect_ptr(ic->g, (void **)&buf);

    int len = 0;

    double num = to_number(ic, car(args))->u.number.value;
    double width = to_number(ic, car(cdr(args)))->u.number.value;
    sexpr *precision = car(cdr(cdr(args)));
    protect_ptr(ic->g, (void **)&precision);

    if(width >= 0) {
        STORE(ic->g, NULL, precision, to_number(ic, precision));
    } else {
        STORE(ic->g, NULL, precision, to_name(ic, precision));
    }


    if(width >= 0) {
        /* Normal */
        int widthi = (int)trunc(width),
            precisioni = (int)trunc(precision->u.number.value);
        
        len = snprintf(NULL, 0, "%*.*f",
                                widthi,
                                precisioni,
                                num);
        buf = (char *)ic_xmalloc(ic, len+1, mark_cstring);
        len = snprintf(buf, len+1, "%*.*f",
                                   widthi,
                                   precisioni,
                                   num);
    } else {
        /* hack where the precision is treated as a
           format string for sprintf */
        char *format = get_cstring(ic, precision);
        protect_ptr(ic->g, (void **) &format);
        len = snprintf(NULL, 0, format, num);
        buf = (char *)ic_xmalloc(ic, len+1, mark_cstring);
        len = snprintf(buf, len+1, format, num);
        unprotect_ptr(ic->g);
    }

    sexpr *ret = intern_len_gc(ic, buf, 0, len);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

/* WAIT N
   Pauses for N 60th's of a second.
 */
sexpr *wait_subr(IC *ic, sexpr *s) {
    double count;
    unsigned int seconds;
    useconds_t useconds;

    count = to_number(ic, car(s))->u.number.value;
    seconds = trunc(count) / 60;
    useconds = round(fmod(count, 60) * 16667);
                                     /* 16667 is about the number of
                                        microseconds in a sixtieth of a
                                        second.  1,000,000/60. */
    if(seconds) sleep(seconds);
    if(useconds) usleep(useconds);
    return ic->g_unbound;
}

/* INT N truncates N to an integer. */
sexpr *int_subr(IC *ic, sexpr *s) {
    return mk_number(ic, trunc(to_number(ic, car(s))->u.number.value));
}

/* ROUND N rounds N to the nearest integer. */
sexpr *round_subr(IC *ic, sexpr *s) {
    return mk_number(ic, round(to_number(ic, car(s))->u.number.value));
}

/* SQRT - Square Root */
sexpr *sqrt_subr(IC *ic, sexpr *s) {
    return mk_number(ic, sqrt(to_number(ic, car(s))->u.number.value));
}

/* EXP N -> e^N */
sexpr *exp_subr(IC *ic, sexpr *s) {
    return mk_number(ic, exp(to_number(ic, car(s))->u.number.value));
}

/* LOG10 - Log to the base 10. */
sexpr *log10_subr(IC *ic, sexpr *s) {
    return mk_number(ic, log10(to_number(ic, car(s))->u.number.value));
}

/* LN - Natural logarithm */
sexpr *ln_subr(IC *ic, sexpr *s) {
    return mk_number(ic, log(to_number(ic, car(s))->u.number.value));
}


static double degrad = 3.141592653589793227020265931059839203954/180.0;

/* SIN N
   Takes the sin of N degrees.
   Complicated math to minimize rounding errors.
 */
double degreesin(double degrees) {
    /* Brian Harvey claims that Kahan says that you should only convert
       degrees in the range of 0-45 into radians for sin and cos functions
       in order to reduce the impact of round off errors in the conversion.

       This funky algorithm uses identities to get "degrees" into that range
       before converting and calling sin() or cos().
     */
    int sign;
    double ret;

    /* Set up an invariant.  Call d0 the original value of degrees.
       sin(d0) = sin(degrees) * (sign ? -1 : 1)
 
       First, we make sure that degrees is positive.
       If not, we set sign and force degrees to be positive.
     */
    sign = (degrees < 0.0);
    if (sign) degrees = -degrees;

    /* Force 0 <= degrees <= 360.  Use the identity sin(x) = sin(x + n*360). */
    degrees = fmod(degrees,360.0);

    /* Force 0 <= degrees <= 180.  sin(x) = -sin(x+180). */
    if (degrees > 180.0) {
        degrees -= 180.0;
        sign = !sign;
    }

    /* Force 0 <= degrees <= 90.  sin(x) = sin(180 - x). */
    if (degrees > 90.0) degrees = 180.0 - degrees;

    /* If degrees <= 45, just convert it and use it.
       If degrees > 45, force 0 <= degrees <= 45 with the identity
       sin(x) = cos(90 - x). */
    if (degrees > 45.0)
        ret = cos((90.0-degrees)*degrad);
    else
        ret = sin(degrees*degrad);
    if (sign) ret = -ret;

    return ret;
}

/* COS N
   Takes the cos of N degrees.
 */
double degreecos(double degrees) {
    return degreesin(90.0-degrees);
}

/* ARCTAN N
   Return the arctan of N in degrees.
 */
double degreeatan(double degrees) {
    return atan(degrees)/degrad;
}

/* Wrapper functions for the above. */
sexpr *sin_subr(IC *ic, sexpr *s) {
    return mk_number(ic, degreesin(to_number(ic, car(s))->u.number.value));
}

sexpr *cos_subr(IC *ic, sexpr *s) {
    return mk_number(ic, degreecos(to_number(ic, car(s))->u.number.value));
}

sexpr *arctan_subr(IC *ic, sexpr *s) {
    return mk_number(ic, degreeatan(to_number(ic, car(s))->u.number.value));
}

/* Radian versions - Much simpler because the standard library
   uses radians. */
sexpr *radsin_subr(IC *ic, sexpr *s) {
    return mk_number(ic, sin(to_number(ic, car(s))->u.number.value));
}

sexpr *radcos_subr(IC *ic, sexpr *s) {
    return mk_number(ic, cos(to_number(ic, car(s))->u.number.value));
}

sexpr *radarctan_subr(IC *ic, sexpr *s) {
    return mk_number(ic, atan(to_number(ic, car(s))->u.number.value));
}

/* Bitwise operations.
   Truncate to integers before operating.
 */
sexpr *bitnot_subr(IC *ic, sexpr *s) {
    return mk_number(ic, ~(int)trunc(to_number(ic, car(s))->u.number.value));
}

sexpr *bitand_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int n1 = (int)trunc(to_number(ic, car(s))->u.number.value);
    int n2 = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    unprotect_ptr(ic->g);

    return mk_number(ic, n1 & n2);
}

sexpr *bitor_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int n1 = (int)trunc(to_number(ic, car(s))->u.number.value);
    int n2 = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    unprotect_ptr(ic->g);

    return mk_number(ic, n1 | n2);
}

sexpr *bitxor_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int n1 = (int)trunc(to_number(ic, car(s))->u.number.value);
    int n2 = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    unprotect_ptr(ic->g);

    return mk_number(ic, n1 ^ n2);
}

/* Arithmetic shift
   Sign extended.
   Negative values represent a right shift.
 */
sexpr *ashift_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int val = (int)trunc(to_number(ic, car(s))->u.number.value);
    int shift = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    unprotect_ptr(ic->g);

    if(shift >= 0)
        val <<= shift;
    else 
        val >>= -shift;

    return mk_number(ic, val);
}

/* Logical shift
   Not sign extended.
   Negative values represent a right shift.
 */
sexpr *lshift_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int val = (int)trunc(to_number(ic, car(s))->u.number.value);
    int shift = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    unprotect_ptr(ic->g);

    if(shift >= 0)
        val <<= shift;
    else 
        val = (unsigned)val >> -shift;

    return mk_number(ic, val);
}

/* POWER N M -> N^M */
sexpr *power_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    double base = to_number(ic, car(s))->u.number.value;
    double exponent = to_number(ic, car(cdr(s)))->u.number.value;
    unprotect_ptr(ic->g);
    return mk_number(ic, pow(base, exponent));
}

/* Dangerous mutators .SETFIRST and .SETBF */
sexpr *dot_setfirst_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *l = car(s);
    protect_ptr(ic->g, (void **)&l);
    sexpr *thing = car(cdr(s));
    protect_ptr(ic->g, (void **)&thing);

    if(l->t != CONS)
        bad_argument(ic, l);

    STORE(ic->g, l, l->u.cons.car, thing);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *dot_setbf_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *l = car(s);
    protect_ptr(ic->g, (void **)&l);
    sexpr *thing = car(cdr(s));
    protect_ptr(ic->g, (void **)&thing);

    if(l->t != CONS)
        bad_argument(ic, l);

    STORE(ic->g, l, l->u.cons.cdr, thing);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

/* Return the ascii value of the first character in the word.
   ASCII "A -> 65
 */
sexpr *ascii_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *arg = to_name(ic, car(s));
    protect_ptr(ic->g, (void **)&arg);

    if(arg->u.name.length < 1)
        bad_argument(ic, arg);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return mk_number(ic, arg->u.name.head[arg->u.name.start]);
}

/* Apply a mapping to every letter in a string.
   Used in uppercase_subr and lowercase_subr below. */
static sexpr *mapstr(IC *ic, sexpr *s, int (*mapper)(int)) {
    protect_ptr(ic->g, (void **)&s);
    int i;

    sexpr *arg = to_name(ic, car(s));
    protect_ptr(ic->g, (void **) &arg);

    int len = arg->u.name.length;

    char *buf = (char *)ic_xmalloc(ic, len, mark_cstring);
    protect_ptr(ic->g, (void **) &buf);

    for(i = 0; i < len; i++)
        buf[i] = (*mapper)(arg->u.name.head[arg->u.name.start+i]);

    sexpr *ret = intern_len_gc(ic, buf, 0, len);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Uppercase all of the letters in the argument. */
sexpr *uppercase_subr(IC *ic, sexpr *s) {
    return mapstr(ic, s, toupper);
}

/* Lowercase all of the letters in the argument. */
sexpr *lowercase_subr(IC *ic, sexpr *s) {
    return mapstr(ic, s, tolower);
}

/* Does the first string appear in the second one?
   Does not work on strings containing NULL bytes. */
sexpr *substringp_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *needle = car(s);
    protect_ptr(ic->g, (void **)&needle);

    sexpr *haystack = car(cdr(s));
    protect_ptr(ic->g, (void **)&haystack);

    if((needle->t != NAME && needle->t != NUMBER) ||
       (haystack->t != NAME && haystack->t != NUMBER))
        return ic->n_false;

    char *ns = get_cstring(ic, to_name(ic, needle));
    protect_ptr(ic->g, (void **) &ns);
    char *hs = get_cstring(ic, to_name(ic, haystack));
    protect_ptr(ic->g, (void **) &hs);

    if(name_eq(ic->n_caseignoredp->u.name.symbol->value, ic->n_true)) {
        /* If CASEIGNOREDP is set to "TRUE then we forcibly uppercase
           all characters in both strings before the search. */
        char *cp;
        for(cp = ns; *cp; cp++)
            *cp = toupper(*cp);
        for(cp = hs; *cp; cp++)
            *cp = toupper(*cp);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    if(strstr(hs, ns))
        return ic->n_true;
    else
        return ic->n_false;
}

/* BEFOREP N M
      -> TRUE if N comes before M in ASCII coalating sequence.
      -> FALSE otherwise
 */
sexpr *beforep_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *first = car(s);
    protect_ptr(ic->g, (void **)&first);
    sexpr *second = car(cdr(s));
    protect_ptr(ic->g, (void **)&second);

    char *fs, *ss;
    int comparison;

    fs = get_cstring(ic, to_name(ic, first));
    protect_ptr(ic->g, (void **) &fs);
    ss = get_cstring(ic, to_name(ic, second));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    /* If CASEIGNOREDP is "TRUE, then we use strcasecmp to get a case
       insensitive comparison. */
    if(name_eq(ic->n_caseignoredp->u.name.symbol->value, ic->n_true))
        comparison = strcasecmp(fs, ss);
    else
        comparison = strcmp(fs, ss);

    if(comparison < 0)
        return ic->n_true;
    else
        return ic->n_false;
}

/* ARRAY N [ORIGIN] creates an array with N members, with the origin
   set to ORIGIN or (if omitted) 1. */
sexpr *array_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int length = (int)trunc(to_number(ic, car(s))->u.number.value);
    int origin;

    if(is_nil(ic, cdr(s)))
        origin = 1;
    else
        origin = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);

    unprotect_ptr(ic->g);
    return array(ic, length, origin);
}

static void index_range_error(IC *ic, int index, sexpr *thing) {
    protect_ptr(ic->g, (void **)&thing);
    eprintf(ic, "Range error - %d is out of range for ", index);
    eprint_sexpr(ic, thing);
    eprintf(ic, "\n");
    unprotect_ptr(ic->g);
    throw_error(ic, ic->continuation->line);
}

/* SETITEM N ARRAY I
   Set item N of ARRAY to I.
 */
sexpr *setitem_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int index = (int)trunc(to_number(ic, car(s))->u.number.value);
    unsigned int realindex;

    sexpr *array = car(cdr(s));
    protect_ptr(ic->g, (void **)&array);
    sexpr *value = car(cdr(cdr(s)));
    protect_ptr(ic->g, (void **)&value);

    if(array->t != ARRAY)
        bad_argument(ic, array);

    realindex = (unsigned int) (index - array->u.array.origin);
    if(realindex < 0 || realindex >= array->u.array.length)
        index_range_error(ic, index, array);

    STORE(ic->g, array->u.array.members,
                 array->u.array.members[realindex],
                 value);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

/* ITEM N THING -> Item N of THING
   THING can be an array, a list, or a name.
 */
sexpr *item_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int index = (int)trunc(to_number(ic, car(s))->u.number.value);

    sexpr *thing = car(cdr(s));
    protect_ptr(ic->g, (void **)&thing);

    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **)&ret);

    switch(thing->t) {
        case NAME:
            if(index < 1 || (unsigned int)index > thing->u.name.length)
                index_range_error(ic, index, thing);
            STORE(ic->g, NULL, ret,
                  intern_len_gc(ic, thing->u.name.head,
                                    thing->u.name.start + index - 1,
                                    1));
            goto end;
        case CONS:
            {
                int i = index;
                sexpr *e = thing;
                protect_ptr(ic->g, (void **)&e);

                if(i < 1) index_range_error(ic, i, thing);

                while(i > 1) {
                    i--;
                    STORE(ic->g, NULL, e, cdr(e));
                    if(is_nil(ic, e))
                        index_range_error(ic, index, thing);
                }

                STORE(ic->g, NULL, ret, car(e));
                unprotect_ptr(ic->g);
                goto end;
            }
               
        case ARRAY:
            {
                unsigned int realindex = index - thing->u.array.origin;
                if(realindex < 0 || realindex >= thing->u.array.length)
                    index_range_error(ic, index, thing);
                STORE(ic->g, NULL, ret, thing->u.array.members[realindex]);
                goto end;
            }
        default:
            bad_argument(ic, thing);
    }

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* COUNT THING
   How many members does THING have?
   THING can be a list, a name, a number (treated as a name),
   or an array.
 */
sexpr *count_subr(IC *ic, sexpr *s) {
    sexpr *e = car(s);

    int i;

    if(e->t == EMPTY_LIST)
        return mk_number(ic, 0);

    if(e->t == NAME)
        return mk_number(ic, e->u.name.length);

    if(e->t == NUMBER)
        return mk_number(ic, to_name(ic, e)->u.name.length);

    if(e->t == ARRAY)
        return mk_number(ic, e->u.array.length);

    if(e->t == CONS) {
        for(i = 0; !is_nil(ic, e); i++, e = cdr(e))
            ;
        return mk_number(ic, i);
    }

    bad_argument(ic, e);
}

/* Is the argument an array? */
sexpr *arrayp_subr(IC *ic, sexpr *s) {
    if(car(s)->t == ARRAY)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Are all of the arguments true? */
sexpr *and_subr(IC *ic, sexpr *s) {
    for(; !is_nil(ic, s); s = cdr(s)) {
        if(name_eq(car(s), ic->n_false))
            return ic->n_false;
        if(!name_eq(car(s), ic->n_true))
            bad_argument(ic, car(s));
    }
    return ic->n_true;
}

/* Are any of the arguments true? */
sexpr *or_subr(IC *ic, sexpr *s) {
    for(; !is_nil(ic, s); s = cdr(s)) {
        if(name_eq(car(s), ic->n_true))
            return ic->n_true;
        if(!name_eq(car(s), ic->n_false))
            bad_argument(ic, car(s));
    }
    return ic->n_false;
}

/* Subroutine for #.
   Returns :template.number.
 */
sexpr *hash_subr(IC *ic, sexpr *s) {
    return ic->n_template_number->u.name.symbol->value;
}

/* Handles ? */
sexpr *question_subr(IC *ic, sexpr *s) {
    if(is_nil(ic, s))
      return ic->n_q1->u.name.symbol->value;

    switch((int)trunc(to_number(ic, car(s))->u.number.value)) {
        case 1:  return ic->n_q1->u.name.symbol->value;
        case 2:  return ic->n_q2->u.name.symbol->value;
        case 3:  return ic->n_q3->u.name.symbol->value;
        case 4:  return ic->n_q4->u.name.symbol->value;
        case 5:  return ic->n_q5->u.name.symbol->value;
        case 6:  return ic->n_q6->u.name.symbol->value;
        case 7:  return ic->n_q7->u.name.symbol->value;
        case 8:  return ic->n_q8->u.name.symbol->value;
        case 9:  return ic->n_q9->u.name.symbol->value;
        case 10: return ic->n_q10->u.name.symbol->value;
        default: bad_argument(ic, car(s));
    }
}

sexpr *continuation_stacktrace(IC *ic, sexpr *s) {
  protect_ptr(ic->g, (void **)&s);
  continuation *c = ic->continuation;
  protect_ptr(ic->g, (void **)&c);

  for(;;) {
    tprintf(ic, "    ");
    tprint_sexpr(ic, c->expr);
    tprintf(ic, "\n");
    if(c->parent == NULL)
      break;
    STORE(ic->g, NULL, c, c->parent);
  }

  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  return ic->g_unbound;
}

sexpr *environment_stacktrace(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int i = 0;
    struct frame *f = ic->continuation->frame;
    protect_ptr(ic->g, (void **)&f);
    while(f != NULL) {
        tprintf(ic, "   %5d ", i);
        tprint_sexpr(ic, f->procedure);
        tprintf(ic, "\n");
        i++;
        STORE(ic->g, NULL, f, f->parent);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *gc_node_size(IC *ic, sexpr *s) {
    return mk_number(ic, sizeof(struct node));
}


/*************************************************************************\
                          BEGIN GUI PROCEDURES
\*************************************************************************/

wxString wxwaitfor_string(IC *ic, wxEvent *event) {
    wxMessageQueueError res;
    wxString response;
    wxTheApp->QueueEvent(event);
    do {
        res = wxGetApp().GetEditedQueue()->ReceiveTimeout(100, response);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(ic->quit, 1);
    } while(res == wxMSGQUEUE_TIMEOUT);


    if(res == wxMSGQUEUE_NO_ERROR)
        return response;

    eprintf(ic, "Error receiving wxString from GUI thread");
    throw_error(ic, ic->continuation->line);
}

double wxwaitfor_number_noevent(IC *ic) {
    wxMessageQueueError res;
    double response = 0;

    do {
        res = wxGetApp().GetNumberQueue()->ReceiveTimeout(100, response);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(ic->quit, 1);
    } while(res == wxMSGQUEUE_TIMEOUT);

    if(res == wxMSGQUEUE_NO_ERROR)
        return response;

    eprintf(ic, "Error receiving double from GUI thread");
    throw_error(ic, ic->continuation->line);
}

double wxwaitfor_number(IC *ic, wxEvent *event) {
    wxTheApp->QueueEvent(event);
    return wxwaitfor_number_noevent(ic);
}

void wxwaitfor_condition(IC *ic, wxEvent *event) {
    wxCondError res;

    turtleConditionLocker.Lock();
    wxTheApp->QueueEvent(event);
    do {
        res = turtleCondition.WaitTimeout(100);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy()) {
            turtleConditionLocker.Unlock();
            longjmp(ic->quit, 1);
        }
    } while(res == wxCOND_TIMEOUT);
    turtleConditionLocker.Unlock();
}

int toint(IC *ic, sexpr *s) {
    return (int)trunc(to_number(ic, s)->u.number.value);
}

sexpr *wxedit_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *first = car(s);
    protect_ptr(ic->g, (void **)&first);

    if(first->t == NAME) {
        wxString wxs(get_cstring(ic, first));
        wxCommandEvent *editE = new wxCommandEvent(EDIT_STRING);
        editE->SetString(wxs);
        unprotect_ptr(ic->g);
        unprotect_ptr(ic->g);
        return intern(ic, wxwaitfor_string(ic, editE));
    }

    bad_argument(ic, first);
}

    
sexpr *tsetxy(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double x = to_number(ic, car(cdr(s)))->u.number.value;
    double y = to_number(ic, car(cdr(cdr(s))))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETXY, turtle, x, y));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsetx(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double x = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETX, turtle, x));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsety(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double y = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETY, turtle, y));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *txcor(IC *ic, sexpr *s) {
    return mk_number(ic, wxwaitfor_number(ic, 
                                new DrawEvent(XCOR, toint(ic, car(s)))));
}

sexpr *tycor(IC *ic, sexpr *s) {
    return mk_number(ic, wxwaitfor_number(ic, 
                                new DrawEvent(YCOR, toint(ic, car(s)))));
}

sexpr *tsetrotation(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double rotation = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETROTATION, turtle, rotation));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *trotation(IC *ic, sexpr *s) {
    return mk_number(ic, wxwaitfor_number(ic, 
                                new DrawEvent(ROTATION, toint(ic, car(s)))));
}

sexpr *trotate(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double degrees = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(ROTATE, turtle, degrees));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsetheading(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double heading = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETHEADING, turtle, heading));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *theading(IC *ic, sexpr *s) {
    return mk_number(ic, wxwaitfor_number(ic, 
                                new DrawEvent(HEADING, toint(ic, car(s)))));
}

void right(IC *ic, int turtle, double degrees) {
    wxwaitfor_condition(ic, new DrawEvent(RIGHT, turtle, degrees));
}

sexpr *tright_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));

    right(ic, turtle, to_number(ic, car(cdr(s)))->u.number.value);

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tleft_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));

    right(ic, turtle, -to_number(ic, car(cdr(s)))->u.number.value);

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

void forward(IC *ic, int turtle, double dist) {
    wxwaitfor_condition(ic, new DrawEvent(FORWARD, turtle, dist));
}

sexpr *tforward_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));

    forward(ic, turtle, to_number(ic, car(cdr(s)))->u.number.value);

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tback_subr(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));

    forward(ic, turtle, -to_number(ic, car(cdr(s)))->u.number.value);

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *clearscreen(IC *ic, sexpr *s) {
    wxwaitfor_condition(ic, new DrawEvent(CLEAR_SCREEN));
    return ic->g_unbound;
}

sexpr *clean(IC *ic, sexpr *s) {
    wxwaitfor_condition(ic, new DrawEvent(CLEAN_SCREEN));
    return ic->g_unbound;
}

sexpr *tarc(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double angle = to_number(ic, car(cdr(s)))->u.number.value;
    double radius = to_number(ic, car(cdr(cdr(s))))->u.number.value;

    wxwaitfor_condition(ic, new DrawEvent(DRAW_ARC, turtle, angle, radius));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *fetchturtles(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **)&ret);

    int i, count, turtle;

    count = (int) wxwaitfor_number(ic, new DrawEvent(FETCHTURTLES));

    for(i = 0; i < count; i++) {
        turtle = (int) wxwaitfor_number_noevent(ic);
        sexpr *tnum = mk_number(ic, turtle);
        protect_ptr(ic->g, (void **)&tnum);
        STORE(ic->g, NULL, ret, cons(ic, tnum, ret));
        unprotect_ptr(ic->g);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *deleteturtle(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(DELETETURTLE, turtle));
    return ic->g_unbound;
}

sexpr *tpenup(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(PENUP, turtle));
    return ic->g_unbound;
}

sexpr *tpendown(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(PENDOWN, turtle));
    return ic->g_unbound;
}

sexpr *tshowturtle(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(SHOWTURTLE, turtle));
    return ic->g_unbound;
}

sexpr *thideturtle(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(HIDETURTLE, turtle));
    return ic->g_unbound;
}

sexpr *tbitmap(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int turtle = toint(ic, car(s));
    wxString path(get_cstring(ic, to_name(ic, car(cdr(s)))));
    double x = to_number(ic, car(cdr(cdr(s))))->u.number.value;
    double y = to_number(ic, car(cdr(cdr(cdr(s)))))->u.number.value;

    unprotect_ptr(ic->g);
    if(wxwaitfor_number(ic, new DrawEvent(BITMAPTURTLE, turtle, path, x, y))) {
        return ic->n_true;
    } else {
        return ic->n_false;
    }
}

sexpr *tpath(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(PATHTURTLE, turtle));
    return ic->g_unbound;
}

sexpr *twrap(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(WRAP, turtle));
    return ic->g_unbound;
}

sexpr *twindow(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(WINDOW, turtle));
    return ic->g_unbound;
}

sexpr *tfence(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(FENCE, turtle));
    return ic->g_unbound;
}

sexpr *tturtlemode(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    TurtleTypes::TurtleMode mode = (TurtleTypes::TurtleMode)
        wxwaitfor_number(ic, new DrawEvent(TURTLEMODE, turtle));

    if(mode == TurtleTypes::WRAP)
        return ic->n_wrap;
    else if(mode == TurtleTypes::WINDOW)
        return ic->n_window;
    else if(mode == TurtleTypes::FENCE)
        return ic->n_fence;

    return ic->g_nil;
}

sexpr *tshown(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    if(wxwaitfor_number(ic, new DrawEvent(SHOWN, turtle)))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *tpendownp(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    if(wxwaitfor_number(ic, new DrawEvent(PENDOWNP, turtle)))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *intsetbg(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    unsigned char red = toint(ic, car(s));
    unsigned char green = toint(ic, car(cdr(s)));
    unsigned char blue = toint(ic, car(cdr(cdr(s))));
    wxwaitfor_condition(ic, new DrawEvent(SETBG, red, green, blue));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsetpencolor(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    unsigned char red = toint(ic, car(cdr(s)));
    unsigned char green = toint(ic, car(cdr(cdr(s))));
    unsigned char blue = toint(ic, car(cdr(cdr(cdr(s)))));
    wxwaitfor_condition(ic, new DrawEvent(SETPENCOLOR, turtle, red, green, blue));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsetpensize(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    int size = toint(ic, car(cdr(s)));
    wxwaitfor_condition(ic, new DrawEvent(SETPENSIZE, turtle, size));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tpensize(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    return mk_number(ic, wxwaitfor_number(ic, new DrawEvent(PENSIZE, turtle)));
}

sexpr *tcapturebitmap(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic,                                   car(s));
    int x1 = toint(ic,                                   car(cdr(s)));
    int y1 = toint(ic,                               car(cdr(cdr(s))));
    int x2 = toint(ic,                           car(cdr(cdr(cdr(s)))));
    int y2 = toint(ic,                       car(cdr(cdr(cdr(cdr(s))))));
    int xoffset = toint(ic,              car(cdr(cdr(cdr(cdr(cdr(s)))))));
    int yoffset = toint(ic,          car(cdr(cdr(cdr(cdr(cdr(cdr(s))))))));
    sexpr *transparent =         car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(s))))))));
    wxwaitfor_condition(ic, new DrawEvent(CAPTUREBITMAP, turtle, x1, y1, x2, y2, xoffset, yoffset, name_eq(transparent, ic->n_true)));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *captureshape(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int shape = toint(ic,                                   car(s));
    int x1    = toint(ic,                                   car(cdr(s)));
    int y1    = toint(ic,                               car(cdr(cdr(s))));
    int x2    = toint(ic,                           car(cdr(cdr(cdr(s)))));
    int y2    = toint(ic,                       car(cdr(cdr(cdr(cdr(s))))));
    int xoffset = toint(ic,              car(cdr(cdr(cdr(cdr(cdr(s)))))));
    int yoffset = toint(ic,          car(cdr(cdr(cdr(cdr(cdr(cdr(s))))))));
    sexpr *transparent =         car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(s))))))));
    wxwaitfor_condition(ic, new DrawEvent(CAPTURESHAPE, shape, x1, y1, x2, y2, xoffset, yoffset, name_eq(transparent, ic->n_true)));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsetshape(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic,     car(s));
    int shape  = toint(ic, car(cdr(s)));
    wxwaitfor_condition(ic, new DrawEvent(SETSHAPE, turtle, shape));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *autorefreshon(IC *ic, sexpr *s) {
    wxwaitfor_condition(ic, new DrawEvent(AUTOREFRESHON));
    return ic->g_unbound;
}

sexpr *autorefreshoff(IC *ic, sexpr *s) {
    wxwaitfor_condition(ic, new DrawEvent(AUTOREFRESHOFF));
    return ic->g_unbound;
}

sexpr *refresh(IC *ic, sexpr *s) {
    wxwaitfor_condition(ic, new DrawEvent(REFRESH));
    return ic->g_unbound;
}

sexpr *tsetxscale(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double scale = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETXSCALE, turtle, scale));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsetyscale(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double scale = to_number(ic, car(cdr(s)))->u.number.value;
    wxwaitfor_condition(ic, new DrawEvent(SETYSCALE, turtle, scale));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *txscale(IC *ic, sexpr *s) {
    return mk_number(ic, wxwaitfor_number(ic, 
                                new DrawEvent(XSCALE, toint(ic, car(s)))));
}

sexpr *tyscale(IC *ic, sexpr *s) {
    return mk_number(ic, wxwaitfor_number(ic, 
                                new DrawEvent(YSCALE, toint(ic, car(s)))));
}

sexpr *saveturtle(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    wxString path(get_cstring(ic, to_name(ic, car(cdr(s)))));
    wxwaitfor_condition(ic, new DrawEvent(SAVE_TURTLE, turtle, path));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *savepict(IC *ic, sexpr *s) {
    wxString path(get_cstring(ic, to_name(ic, car(s))));
    wxCommandEvent *e = new wxCommandEvent(SAVEPICT);
    e->SetString(path);
    wxwaitfor_condition(ic, e);
    return ic->g_unbound;
}

sexpr *loadpict(IC *ic, sexpr *s) {
    wxString path(get_cstring(ic, to_name(ic, car(s))));
    wxCommandEvent *e = new wxCommandEvent(LOADPICT);
    e->SetString(path);
    wxwaitfor_condition(ic, e);
    return ic->g_unbound;
}

sexpr *tpenpaint(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(PENPAINT, turtle));
    return ic->g_unbound;
}

sexpr *tpenerase(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(PENERASE, turtle));
    return ic->g_unbound;
}

sexpr *tover(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    unsigned char red = toint(ic, car(cdr(s)));
    unsigned char green = toint(ic, car(cdr(cdr(s))));
    unsigned char blue = toint(ic, car(cdr(cdr(cdr(s)))));
    unprotect_ptr(ic->g);
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_OVER,
                                                   turtle,
                                                   PixelCriteria(NONTRANSPARENT),
                                                   PixelCriteria(COLOUR, wxColour(red, green, blue)))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *tpasttop(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_PASTTOP,
                                                   turtle,
                                                   PixelCriteria(NONTRANSPARENT))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *tpastbottom(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_PASTBOTTOM,
                                                   turtle,
                                                   PixelCriteria(NONTRANSPARENT))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *tpastleft(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_PASTLEFT,
                                                   turtle,
                                                   PixelCriteria(NONTRANSPARENT))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *tpastright(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_PASTRIGHT,
                                                   turtle,
                                                   PixelCriteria(NONTRANSPARENT))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *txyover(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic,                             car(s));
    int x = toint(ic,                              car(cdr(s)));
    int y = toint(ic,                          car(cdr(cdr(s))));
    unsigned char red =          toint(ic, car(cdr(cdr(cdr(s)))));
    unsigned char green =    toint(ic, car(cdr(cdr(cdr(cdr(s))))));
    unsigned char blue = toint(ic, car(cdr(cdr(cdr(cdr(cdr(s)))))));
    unprotect_ptr(ic->g);

    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLEXY_OVER,
                                                   turtle,
                                                   x,
                                                   y,
                                                   PixelCriteria(COLOUR, wxColour(red, green, blue)))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *tmoveuntil(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    double angle = to_number(ic, car(cdr(s)))->u.number.value;
    double distance = to_number(ic, car(cdr(cdr(s))))->u.number.value;
    double stepsize = to_number(ic, car(cdr(cdr(cdr(s)))))->u.number.value;

    sexpr *conditions = car(cdr(cdr(cdr(cdr(s)))));
    protect_ptr(ic->g, (void **)&conditions);

    sexpr *orig_conditions = conditions;
    protect_ptr(ic->g, (void **)&orig_conditions);

    sexpr *turtles = car(cdr(cdr(cdr(cdr(cdr(s))))));
    protect_ptr(ic->g, (void **)&turtles);

    sexpr *orig_turtles = turtles;
    protect_ptr(ic->g, (void **)&orig_turtles);

    PixelCriteriaVector criteria;
    IntVector turtlevec;

    while(!is_nil(ic, conditions)) {
        sexpr *cond = car(conditions);
        protect_ptr(ic->g, (void **)&cond);

        if(cond->t == CONS) {
                if(length(ic, cond) != 3)
                    bad_argument(ic, orig_conditions);
            unsigned char red = toint(ic, car(cond));
            unsigned char green = toint(ic, car(cdr(cond)));
            unsigned char blue = toint(ic, car(cdr(cdr(cond))));
            criteria.push_back(PixelCriteria(COLOUR, wxColour(red, green, blue)));
        } else if(name_eq(cond, ic->n_pasttop)) {
            criteria.push_back(PixelCriteria(PAST_TOP));
        } else if(name_eq(cond, ic->n_pastbottom)) {
            criteria.push_back(PixelCriteria(PAST_BOTTOM));
        } else if(name_eq(cond, ic->n_pastleft)) {
            criteria.push_back(PixelCriteria(PAST_LEFT));
        } else if(name_eq(cond, ic->n_pastright)) {
            criteria.push_back(PixelCriteria(PAST_RIGHT));
        } else {
            bad_argument(ic, orig_conditions);
        }

        unprotect_ptr(ic->g);

        STORE(ic->g, NULL, conditions, cdr(conditions));
    }

    while(!is_nil(ic, turtles)) {
        turtlevec.push_back(toint(ic, car(turtles)));
        STORE(ic->g, NULL, turtles, cdr(turtles));
    }

    int result = wxwaitfor_number(ic,
        new DrawEvent(MOVEUNTIL, turtle, angle, distance, stepsize, criteria,
                      turtlevec));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return mk_number(ic, result);
}



sexpr *ttouching(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    int turtle2 = toint(ic, car(cdr(s)));
    unprotect_ptr(ic->g);
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_TOUCHING,
                                                   turtle,
                                                   turtle2,
                                                   PixelCriteria(NONTRANSPARENT),
                                                   PixelCriteria(NONTRANSPARENT))))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *txytouching(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic,                             car(s));
    int x = toint(ic,                              car(cdr(s)));
    int y = toint(ic,                          car(cdr(cdr(s))));
    int turtle2 = toint(ic,                car(cdr(cdr(cdr(s)))));
    unprotect_ptr(ic->g);
    if(wxwaitfor_number(ic, new CollisionTestEvent(TURTLEXY_TOUCHING,
                                                   turtle,
                                                   x,
                                                   y,
                                                   turtle2)))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *twhostouching(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    sexpr *ret = ic->g_nil;
    int i, count;
    protect_ptr(ic->g, (void **) &ret);

    count = wxwaitfor_number(ic, new CollisionTestEvent(TURTLE_WHOSTOUCHING,
                                                        turtle,
                                                        PixelCriteria(NONTRANSPARENT),
                                                        PixelCriteria(NONTRANSPARENT)));

    for(i = 0; i < count; i++) {
        turtle = (int) wxwaitfor_number_noevent(ic);
        sexpr *tnum = mk_number(ic, turtle);
        protect_ptr(ic->g, (void **)&tnum);
        STORE(ic->g, NULL, ret, cons(ic, tnum, ret));
        unprotect_ptr(ic->g);
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *txywhostouching(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic,                             car(s));
    int x = toint(ic,                              car(cdr(s)));
    int y = toint(ic,                          car(cdr(cdr(s))));
    int i, count;
    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    count = wxwaitfor_number(ic, new CollisionTestEvent(TURTLEXY_WHOSTOUCHING,
                                                        turtle,
                                                        x,
                                                        y));

    for(i = 0; i < count; i++) {
        turtle = (int) wxwaitfor_number_noevent(ic);
        sexpr *tnum = mk_number(ic, turtle);
        protect_ptr(ic->g, (void **)&tnum);
        STORE(ic->g, NULL, ret, cons(ic, tnum, ret));
        unprotect_ptr(ic->g);
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *whosover(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int x = toint(ic,     car(s));
    int y = toint(ic, car(cdr(s)));
    int turtle, i, count;
    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    count = wxwaitfor_number(ic, new CollisionTestEvent(GLOBALXY_WHOSTOUCHING,
                                                        x,
                                                        y,
                                                        PixelCriteria(NONTRANSPARENT)));

    for(i = 0; i < count; i++) {
        turtle = (int) wxwaitfor_number_noevent(ic);
        sexpr *tnum = mk_number(ic, turtle);
        protect_ptr(ic->g, (void **)&tnum);
        STORE(ic->g, NULL, ret, cons(ic, tnum, ret));
        unprotect_ptr(ic->g);
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *background(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    sexpr *blue = mk_number(ic, (int) wxwaitfor_number(ic, new DrawEvent(BACKGROUND)));
    protect_ptr(ic->g, (void **)&blue);
    sexpr *green = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&green);
    sexpr *red = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&red);

    STORE(ic->g, NULL, ret, listl(ic, red, green, blue, NULL));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *tpencolor(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    int turtle = toint(ic, car(s));

    sexpr *blue = mk_number(ic, (int) wxwaitfor_number(ic, new DrawEvent(PENCOLOR, turtle)));
    protect_ptr(ic->g, (void **)&blue);
    sexpr *green = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&green);
    sexpr *red = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&red);

    STORE(ic->g, NULL, ret, listl(ic, red, green, blue, NULL));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *tcolorunder(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    int turtle = toint(ic, car(s));

    sexpr *blue = mk_number(ic, (int) wxwaitfor_number(ic, new DrawEvent(COLORUNDER, turtle)));
    protect_ptr(ic->g, (void **)&blue);
    sexpr *green = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&green);
    sexpr *red = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&red);

    STORE(ic->g, NULL, ret, listl(ic, red, green, blue, NULL));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *txycolorunder(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    sexpr *ret = ic->g_nil;
    protect_ptr(ic->g, (void **) &ret);

    int turtle = toint(ic,           car(s));
    double x = to_number(ic,     car(cdr(s)))->u.number.value;
    double y = to_number(ic, car(cdr(cdr(s))))->u.number.value;

    sexpr *blue = mk_number(ic, (int) wxwaitfor_number(ic, new DrawEvent(XYCOLORUNDER, turtle, x, y)));
    protect_ptr(ic->g, (void **)&blue);
    sexpr *green = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&green);
    sexpr *red = mk_number(ic, (int) wxwaitfor_number_noevent(ic));
    protect_ptr(ic->g, (void **)&red);
    STORE(ic->g, NULL, ret, listl(ic, red, green, blue, NULL));

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return ret;
}

sexpr *tpenmode(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    TurtleTypes::DrawMode drawmode;


    drawmode = (TurtleTypes::DrawMode) wxwaitfor_number(ic, new DrawEvent(PENMODE, turtle));
    if(drawmode == TurtleTypes::PAINT)
        return ic->n_paint;
    if(drawmode == TurtleTypes::ERASE)
        return ic->n_erase;
    return ic->g_nil;
}

sexpr *tfill(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(FILL, turtle));
    return ic->g_unbound;
}

sexpr *tlabel(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    wxString label(get_cstring(ic, to_name(ic, car(cdr(s)))));
    wxwaitfor_condition(ic, new DrawEvent(LABEL, turtle, label));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tsettext(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int turtle = toint(ic, car(s));
    wxString text(get_cstring(ic, to_name(ic, car(cdr(s)))));
    wxwaitfor_condition(ic, new DrawEvent(SETTEXT, turtle, text));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tgettext(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));

    return intern(ic, wxwaitfor_string(ic, new DrawEvent(GETTEXT, turtle)));
}

    

sexpr *tstamp(IC *ic, sexpr *s) {
    int turtle = toint(ic, car(s));
    wxwaitfor_condition(ic, new DrawEvent(STAMP, turtle));
    return ic->g_unbound;
}

sexpr *pollkey(IC *ic, sexpr *s) {
    int key = toint(ic, car(s));

    wxTheApp->QueueEvent(new wxCommandEvent(POLL_MODE));

    wxCommandEvent *e = new wxCommandEvent(POLL_KEY);
    e->SetInt(key);

    if(wxwaitfor_number(ic, e))
        return ic->n_true;
    else
        return ic->n_false;
}

sexpr *toot(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int voice = toint(ic, car(s));
    double frequency = to_number(ic, car(cdr(s)))->u.number.value;
    double duration = to_number(ic, car(cdr(cdr(s))))->u.number.value;
    double volume = to_number(ic, car(cdr(cdr(cdr(s)))))->u.number.value;
    double pan = to_number(ic, car(cdr(cdr(cdr(cdr(s))))))->u.number.value;
    double attack = to_number(ic, car(cdr(cdr(cdr(cdr(cdr(s)))))))->u.number.value;
    double decay = to_number(ic, car(cdr(cdr(cdr(cdr(cdr(cdr(s))))))))->u.number.value;
    AudioCommands.Post(AudioCommand(AudioCommand::TOOT,
                                    voice,
                                    AudioGenerator(frequency,
                                                   duration,
                                                   volume,
                                                   pan,
                                                   attack,
                                                   decay)));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tootreplace(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    int voice = toint(ic, car(s));
    double frequency = to_number(ic, car(cdr(s)))->u.number.value;
    double duration = to_number(ic, car(cdr(cdr(s))))->u.number.value;
    double volume = to_number(ic, car(cdr(cdr(cdr(s)))))->u.number.value;
    double pan = to_number(ic, car(cdr(cdr(cdr(cdr(s))))))->u.number.value;
    double attack = to_number(ic, car(cdr(cdr(cdr(cdr(cdr(s)))))))->u.number.value;
    double decay = to_number(ic, car(cdr(cdr(cdr(cdr(cdr(cdr(s))))))))->u.number.value;
    AudioCommands.Post(AudioCommand(AudioCommand::TOOTREPLACE,
                                    voice,
                                    AudioGenerator(frequency,
                                                   duration,
                                                   volume,
                                                   pan,
                                                   attack,
                                                   decay)));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *tootclear(IC *ic, sexpr *s) {
    AudioCommands.Post(AudioCommand(AudioCommand::TOOTCLEAR,
                                    0,
                                    AudioGenerator()));
    return ic->g_unbound;
}

sexpr *tootend(IC *ic, sexpr *s) {
    int voice = toint(ic, car(s));
    AudioCommands.Post(AudioCommand(AudioCommand::TOOTEND,
                                    voice,
                                    AudioGenerator()));
    return ic->g_unbound;
}

sexpr *timeleft(IC *ic, sexpr *s) {
    int voice = toint(ic, car(s));
    wxMessageQueueError res;
    double response = 0;

    AudioCommands.Post(AudioCommand(AudioCommand::TIMELEFT,
                                    voice,
                                    AudioGenerator()));

    do {
        res = AudioResponses.ReceiveTimeout(100, response);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(ic->quit, 1);
    } while(res == wxMSGQUEUE_TIMEOUT);

    if(res == wxMSGQUEUE_NO_ERROR)
        return mk_number(ic, response);

    eprintf(ic, "Error receiving double from GUI thread");
    throw_error(ic, ic->continuation->line);
}

double recordwaitfor_number_noevent(IC *ic) {
    wxMessageQueueError res;
    double response = 0;

    do {
        res = RecordingResponses.ReceiveTimeout(100, response);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(ic->quit, 1);
    } while(res == wxMSGQUEUE_TIMEOUT);

    if(res == wxMSGQUEUE_NO_ERROR)
        return response;

    eprintf(ic, "Error receiving double from recording thread");
    throw_error(ic, ic->continuation->line);
}

double recordwaitfor_number(IC *ic, RecordingCommand const &command) {
    RecordingCommands.Post(command);
    return recordwaitfor_number_noevent(ic);
}

AudioDeviceList waitfor_audiodevicelist(IC *ic,
                                        RecordingCommand const &command) {
    wxMessageQueueError res;
    AudioDeviceList response;

    RecordingCommands.Post(command);

    do {
        res = AudioDeviceLists.ReceiveTimeout(100, response);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(ic->quit, 1);
    } while(res == wxMSGQUEUE_TIMEOUT);

    if(res == wxMSGQUEUE_NO_ERROR)
        return response;

    eprintf(ic, "Error receiving device list from recording thread");
    throw_error(ic, ic->continuation->line);
}

sexpr *audiodevices(IC *ic, sexpr *s) {
    AudioDeviceList response = waitfor_audiodevicelist(ic, RecordingCommand(RecordingCommand::QUERY_DEVICES));

    sexpr *ret = cons(ic, ic->g_nil, ic->g_nil);
    protect_ptr(ic->g, (void **)&ret);

    sexpr *tail = ret;
    protect_ptr(ic->g, (void **)&tail);

    for(AudioDeviceList::iterator i = response.begin(); i != response.end(); i++) {
        sexpr *sublist = ic->g_nil;
        protect_ptr(ic->g, (void **)&sublist);

        sexpr *rate = mk_number(ic, i->default_sample_rate);
        STORE(ic->g, NULL, sublist, cons(ic, rate, sublist));

        sexpr *max_outputs = mk_number(ic, i->max_outputs);
        STORE(ic->g, NULL, sublist, cons(ic, max_outputs, sublist));

        sexpr *max_inputs = mk_number(ic, i->max_inputs);
        STORE(ic->g, NULL, sublist, cons(ic, max_inputs, sublist));

        sexpr *name = intern(ic, i->name);
        STORE(ic->g, NULL, sublist, cons(ic, name, sublist));

        push_back(ic, &tail, sublist);
        unprotect_ptr(ic->g);
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return cdr(ret);
}

sexpr *startcapture(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);

    int dev = toint(ic, car(s));
    int rate = toint(ic, car(cdr(s)));
    int frame_size = toint(ic, car(cdr(cdr(s))));


    RecordingCommands.Post(RecordingCommand(RecordingCommand::START,
                                            dev, rate, frame_size));

    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *stopcapture(IC *ic, sexpr *s) {
    RecordingCommands.Post(RecordingCommand(RecordingCommand::STOP));
    return ic->g_unbound;
}

sexpr *frequencypowers(IC *ic, sexpr *s) {
    wxMessageQueueError res;
    protect_ptr(ic->g, (void **)&s);

    STORE(ic->g, NULL, s, car(s));

    RecordingCommand command(RecordingCommand::QUERY_FREQUENCIES);
    while(!is_nil(ic, s)) {
        if(car(s)->t != CONS) bad_argument(ic, car(s));
        if(cdr(car(s))->t != CONS) bad_argument(ic, car(s));
        double frequency = to_number(ic, car(car(s)))->u.number.value;
        double variation = to_number(ic, car(cdr(car(s))))->u.number.value;
        command.frequency_ranges.push_back(FrequencyRange(frequency, variation));
        STORE(ic->g, NULL, s, cdr(s));
    }

    RecordingCommands.Post(command);

    FrequencyPowerList response;

    do {
        res = FrequencyPowers.ReceiveTimeout(100, response);
        if(!ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(ic->quit, 1);
    } while(res == wxMSGQUEUE_TIMEOUT);

    if(res != wxMSGQUEUE_NO_ERROR) {
        eprintf(ic, "Error receiving power list from recording thread");
        throw_error(ic, ic->continuation->line);
    }

    sexpr *ret = cons(ic, ic->g_nil, ic->g_nil);
    protect_ptr(ic->g, (void **)&ret);

    sexpr *tail = ret;
    protect_ptr(ic->g, (void **)&tail);

    for(FrequencyPowerList::iterator i = response.begin();
        i != response.end();
        i++) {
        push_back(ic, &tail, mk_number(ic, *i));
    }

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);

    return cdr(ret);
}

sexpr *startdtmf(IC *ic, sexpr *s) {
    protect_ptr(ic->g, (void **)&s);
    float dtmf_high_threshold = (float)to_number(ic, car(s))->u.number.value;
    float dtmf_low_threshold = (float)to_number(ic, car(cdr(s)))->u.number.value;
    RecordingCommands.Post(RecordingCommand(RecordingCommand::START_DTMF,
                                            dtmf_high_threshold,
                                            dtmf_low_threshold));
    unprotect_ptr(ic->g);
    return ic->g_unbound;
}

sexpr *silence_audio_logging(IC *ic, sexpr *s) {
    RecordingCommands.Post(RecordingCommand(RecordingCommand::SILENCE_LOGGING));
    return ic->g_unbound;
}

sexpr *fetchdtmf(IC *ic, sexpr *s) {
    wxMessageQueueError res;
    int response = 0;
    res = DTMFValues.ReceiveTimeout(0, response);
    if(res == wxMSGQUEUE_TIMEOUT)
        return ic->g_nil;

    if(res != wxMSGQUEUE_NO_ERROR) {
        eprintf(ic, "Error receiving DTMF value from recording thread");
        throw_error(ic, ic->continuation->line);
    }

    char ch = (char) response;
    return intern_len_static(ic, &ch, 0, 1);
}

sexpr *time_subr(IC *ic, sexpr *s) {
    return mk_number(ic, wxGetLocalTime());
}

/* END SUBR's */

/* Utility function for adding a SUBR to the global environment. */
/* Garbage collection is not yet enabled when this is run, so
   we can be a little careless about not protecting things. */
void initialize_subr(IC *ic, const char *namestr,
                             efun f,
                             int minargs, 
                             int defargs, 
                             int maxargs) {

    /* Create the name we will store it in. */
    sexpr *name = intern(ic, namestr);
    protect_ptr(ic->g, (void **)&name);

    /* Wrap the C function in a SUBR object. */
    sexpr *func = mk_subr(ic, name, f);
    protect_ptr(ic->g, (void **)&func);

    /* Wrap the SUBR object in a PROC object containing the minimum,
       default, and maximum argument counts. */
    sexpr *proc = mk_proc(ic, func, minargs, defargs, maxargs);
    protect_ptr(ic->g, (void **)&proc);

    /* Store the PROC in the function slot of the symbol associated
       with the name. */
    STORE(ic->g, name->u.name.symbol,
          name->u.name.symbol->function, proc);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

/* Utility function for adding an FSUBR to the global environment. */
/* Garbage collection is not yet enabled when this is run, so
   we can be a little careless about not protecting things. */
void initialize_fsubr(IC *ic, const char *namestr,
                              efun f,
                              int minargs, 
                              int defargs, 
                              int maxargs) {
    /* Create the name we will store it in. */
    sexpr *name = intern(ic, namestr);
    protect_ptr(ic->g, (void **)&name);

    /* Wrap the C function in a FSUBR object. */
    sexpr *func = mk_fsubr(ic, name, f);
    protect_ptr(ic->g, (void **)&func);

    /* Wrap the FSUBR object in a PROC object containing the minimum,
       default, and maximum argument counts. */
    sexpr *proc = mk_proc(ic, func, minargs, defargs, maxargs);
    protect_ptr(ic->g, (void **)&proc);

    /* Store the FSUBR in the function slot of the symbol associated
       with the name. */
    STORE(ic->g, name->u.name.symbol,
          name->u.name.symbol->function, proc);

    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

/* Add all of the primitives to the global environment. */
/* Garbage collection is not yet enabled when this is run, so
   we can be a little careless about not protecting things. */
void initialize_global_environment(IC *ic) {
    sexpr *version = intern(ic, PACKAGE_VERSION);
    protect_ptr(ic->g, (void **)&version);
    STORE(ic->g,
          ic->n_logoversion->u.name.symbol,
          ic->n_logoversion->u.name.symbol->value,
          version);

    unprotect_ptr(ic->g);


    initialize_subr(ic, "inc", inc, 1, 1, 1);
    initialize_subr(ic, "dec", dec, 1, 1, 1);
    initialize_subr(ic, "eq", eq, 2, 2, 2);
    initialize_subr(ic, ".eq", dot_eq, 2, 2, 2);
    initialize_fsubr(ic, "quote", quote, 1, 1, 1);
    initialize_subr(ic, "internal_function", internal_function, 1, 1, 1);
    initialize_subr(ic, "make", make, 2, 2, 2);
    initialize_subr(ic, "fset", fset, 2, 2, 2);
    initialize_subr(ic, "symbol_function", symbol_function, 1, 1, 1);
    initialize_subr(ic, "source_set", source_set, 2, 2, 2);
    initialize_subr(ic, "symbol_source", symbol_source, 1, 1, 1);
    initialize_subr(ic, "not", not_subr, 1, 1, 1);
    initialize_subr(ic, "print", print_subr, 0, 1, INT_MAX);
    initialize_subr(ic, "pr", print_subr, 0, 1, INT_MAX);
    initialize_subr(ic, "show", show_subr, 0, 1, INT_MAX);
    initialize_subr(ic, "type", type_subr, 0, 1, INT_MAX);
    initialize_subr(ic, "newline", newline, 0, 0, 0);
    initialize_subr(ic, "cons", cons_subr, 2, 2, 2);
    initialize_subr(ic, "car", car_subr, 1, 1, 1);
    initialize_subr(ic, "cdr", cdr_subr, 1, 1, 1);
    initialize_subr(ic, "internal_gensym", gensym_subr, 1, 1, 1);
    initialize_subr(ic, "list", list_subr, 0, 2, INT_MAX);
    initialize_subr(ic, "consp", consp, 1, 1, 1);
    initialize_subr(ic, "cons?", consp, 1, 1, 1);
    initialize_subr(ic, "listp", listp, 1, 1, 1);
    initialize_subr(ic, "list?", listp, 1, 1, 1);
    initialize_subr(ic, "wordp", wordp, 1, 1, 1);
    initialize_subr(ic, "word?", wordp, 1, 1, 1);
    initialize_subr(ic, "numberp", numberp_sexpr, 1, 1, 1);
    initialize_subr(ic, "number?", numberp_sexpr, 1, 1, 1);
    initialize_subr(ic, "namep", namep, 1, 1, 1);
    initialize_subr(ic, "name?", namep, 1, 1, 1);
    initialize_subr(ic, "+", add, 0, 2, INT_MAX);
    initialize_subr(ic, "sum", add, 0, 2, INT_MAX);
    initialize_subr(ic, "-", sub, 0, 2, INT_MAX);
    initialize_subr(ic, "difference", sub, 0, 2, INT_MAX);
    initialize_subr(ic, "--", sub, 0, 1, INT_MAX);
    initialize_subr(ic, "minus", sub, 0, 1, INT_MAX);
    initialize_subr(ic, "*", mul, 0, 2, INT_MAX);
    initialize_subr(ic, "product", mul, 0, 2, INT_MAX);
    initialize_subr(ic, "/", div_subr, 2, 2, 2);
    initialize_subr(ic, "quotient", div_subr, 2, 2, 2);
    initialize_subr(ic, "%", remainder_subr, 2, 2, 2);
    initialize_subr(ic, "remainder", remainder_subr, 2, 2, 2);
    initialize_subr(ic, "modulo", modulo_subr, 2, 2, 2);
    initialize_subr(ic, "<", lt, 2, 2, 2);
    initialize_subr(ic, "lessp", lt, 2, 2, 2);
    initialize_subr(ic, "less?", lt, 2, 2, 2);
    initialize_subr(ic, ">", gt, 2, 2, 2);
    initialize_subr(ic, "greaterp", gt, 2, 2, 2);
    initialize_subr(ic, "greater?", gt, 2, 2, 2);
    initialize_subr(ic, "<=", le, 2, 2, 2);
    initialize_subr(ic, "lessequalp", le, 2, 2, 2);
    initialize_subr(ic, "lessequal?", le, 2, 2, 2);
    initialize_subr(ic, ">=", ge, 2, 2, 2);
    initialize_subr(ic, "greaterequalp", ge, 2, 2, 2);
    initialize_subr(ic, "greaterequal?", ge, 2, 2, 2);
    initialize_subr(ic, "ne", ne, 2, 2, 2);
    initialize_subr(ic, "read", read_subr, 0, 0, 0);
    initialize_subr(ic, "logoread", logoread_subr, 0, 0, 0);
    initialize_subr(ic, "readlist", readlist_subr, 0, 0, 0);
    initialize_subr(ic, "rl", readlist_subr, 0, 0, 0);
    initialize_subr(ic, "readword", readword_subr, 0, 0, 0);
    initialize_subr(ic, "rw", readword_subr, 0, 0, 0);
    initialize_subr(ic, "readrawline", readrawline_subr, 0, 0, 0);
    initialize_subr(ic, "readchar", readchar_subr, 0, 0, 0);
    initialize_subr(ic, "rc", readchar_subr, 0, 0, 0);
    initialize_subr(ic, "readchars", readchars_subr, 1, 1, 1);
    initialize_subr(ic, "rcs", readchars_subr, 1, 1, 1);
    initialize_subr(ic, "readline",
                        readline_subr, 2, 2, 2);
    initialize_subr(ic, "keyp", keyp_subr, 0, 0, 0);
    initialize_subr(ic, "key?", keyp_subr, 0, 0, 0);
    initialize_subr(ic, "oblist", oblist, 0, 0, 0);
    initialize_subr(ic, "gc", collect_garbage_subr, 0, 0, 1);
    initialize_subr(ic, "first", first_subr, 1, 1, 1);
    initialize_subr(ic, "butfirst", butfirst_subr, 1, 1, 1);
    initialize_subr(ic, "bf", butfirst_subr, 1, 1, 1);
    initialize_subr(ic, "last", last, 1, 1, 1);
    initialize_subr(ic, "butlast", butlast, 1, 1, 1);
    initialize_subr(ic, "bl", butlast, 1, 1, 1);
    initialize_subr(ic, "word", word, 0, 2, INT_MAX);
    initialize_subr(ic, "emptyp", emptyp, 1, 1, 1);
    initialize_subr(ic, "empty?", emptyp, 1, 1, 1);
    initialize_subr(ic, "sentence", sentence, 0, 2, INT_MAX);
    initialize_subr(ic, "se", sentence, 0, 2, INT_MAX);
    initialize_subr(ic, "fput", fput_subr, 2, 2, 2);
    initialize_subr(ic, "combine", combine, 2, 2, 2);
    initialize_subr(ic, "treeify", treeify_subr, 1, 1, 1);
    initialize_subr(ic, "random", random_subr, 1, 1, 2);
    initialize_subr(ic, "rerandom", rerandom_subr, 0, 0, 1);
    initialize_subr(ic, "local", local, 1, 1, INT_MAX);
    initialize_subr(ic, "thing", thing, 1, 1, 1);
    initialize_subr(ic, "mk_line", mk_line_subr, 3, 3, 3);
    initialize_subr(ic, "linep", linep, 1, 1, 1);
    initialize_subr(ic, "line?", linep, 1, 1, 1);
    initialize_subr(ic, "line_raw", line_raw, 1, 1, 1);
    initialize_subr(ic, "line_parsed", line_parsed, 1, 1, 1);
    initialize_subr(ic, "line_procedure", line_procedure, 1, 1, 1);
    initialize_subr(ic, "set_current_line", set_current_line, 1, 1, 1);
    initialize_subr(ic, "char", char_subr, 1, 1, 1);

    initialize_subr(ic, "openread", openread_subr, 1, 1, 1);
    initialize_subr(ic, "openwrite", openwrite_subr, 1, 1, 1);
    initialize_subr(ic, "openappend", openappend_subr, 1, 1, 1);
    initialize_subr(ic, "openupdate", openupdate_subr, 1, 1, 1);
    initialize_subr(ic, "close", close_subr, 1, 1, 1);
    initialize_subr(ic, "eofp", eofp_subr, 0, 0, 1);
    initialize_subr(ic, "eof?", eofp_subr, 0, 0, 1);
    initialize_subr(ic, "dribble", dribble_subr, 1, 1, 1);
    initialize_subr(ic, "nodribble", nodribble_subr, 0, 0, 0);
    initialize_subr(ic, "setread", setread_subr, 1, 1, 1);
    initialize_subr(ic, "setwrite", setwrite_subr, 1, 1, 1);

    initialize_subr(ic, "allopen", allopen_subr, 0, 0, 0);
    initialize_subr(ic, "closeall", closeall_subr, 0, 0, 0);
    initialize_subr(ic, "erasefile", erasefile_subr, 1, 1, 1);
    initialize_subr(ic, "reader", reader_subr, 0, 0, 0);
    initialize_subr(ic, "writer", writer_subr, 0, 0, 0);
    initialize_subr(ic, "readpos", readpos_subr, 0, 0, 0);
    initialize_subr(ic, "writepos", writepos_subr, 0, 0, 0);
    initialize_subr(ic, "setreadpos", setreadpos_subr, 1, 1, 1);
    initialize_subr(ic, "setwritepos", setwritepos_subr, 1, 1, 1);
    initialize_subr(ic, "filep", filep_subr, 1, 1, 1);
    initialize_subr(ic, "file?", filep_subr, 1, 1, 1);
    initialize_subr(ic, "pprop", pprop_subr, 3, 3, 3);
    initialize_subr(ic, "gprop", gprop_subr, 2, 2, 2);
    initialize_subr(ic, "remprop", remprop_subr, 2, 2, 2);
    initialize_subr(ic, "plist", plist_subr, 1, 1, 1);
    initialize_subr(ic, "error", error_subr, 0, 0, 0);
    initialize_subr(ic, "raise_error", raise_error_subr, 0, 0, 1);
    initialize_subr(ic, "macro", macro_subr, 2, 2, 2);
    initialize_subr(ic, "parse", parse_subr, 1, 1, 1);
    initialize_subr(ic, "bury", bury_subr, 1, 1, 1);
    initialize_subr(ic, "unbury", unbury_subr, 1, 1, 1);
    initialize_subr(ic, "buriedp", buriedp_subr, 1, 1, 1);
    initialize_subr(ic, "buried?", buriedp_subr, 1, 1, 1);
    initialize_subr(ic, "trace", trace_subr, 1, 1, 1);
    initialize_subr(ic, "untrace", untrace_subr, 1, 1, 1);
    initialize_subr(ic, "tracedp", tracedp_subr, 1, 1, 1);
    initialize_subr(ic, "traced?", tracedp_subr, 1, 1, 1);
    initialize_subr(ic, "step", step_subr, 1, 1, 1);
    initialize_subr(ic, "unstep", unstep_subr, 1, 1, 1);
    initialize_subr(ic, "steppedp", steppedp_subr, 1, 1, 1);
    initialize_subr(ic, "stepped?", steppedp_subr, 1, 1, 1);
    initialize_subr(ic, "openshell", openshell_subr, 2, 2, 2);
    initialize_subr(ic, "system", system_subr, 1, 1, 1);
    initialize_subr(ic, "bye", bye_subr, 0, 0, 0);
    initialize_subr(ic, "getenv", getenv_subr, 1, 1, 1);
    initialize_subr(ic, "nodes", nodes_subr, 0, 0, 0);
    initialize_subr(ic, "continuation_depth", continuation_depth_subr, 0, 0, 0);
    initialize_subr(ic, "frame_depth", frame_depth_subr, 0, 0, 0);
    initialize_subr(ic, "arity", arity_subr, 1, 1, 1);
    initialize_subr(ic, "procedurep", procedurep_subr, 1, 1, 1);
    initialize_subr(ic, "procedure?", procedurep_subr, 1, 1, 1);
    initialize_subr(ic, "primitivep", primitivep_subr, 1, 1, 1);
    initialize_subr(ic, "primitive?", primitivep_subr, 1, 1, 1);
    initialize_subr(ic, "definedp", definedp_subr, 1, 1, 1);
    initialize_subr(ic, "defined?", definedp_subr, 1, 1, 1);
    initialize_subr(ic, "macrop", macrop_subr, 1, 1, 1);
    initialize_subr(ic, "macro?", macrop_subr, 1, 1, 1);
    initialize_subr(ic, "copydef", copydef_subr, 2, 2, 2);
    initialize_subr(ic, "form", form_subr, 3, 3, 3);
    initialize_subr(ic, "erase", erase_subr, 1, 1, 1);
    initialize_subr(ic, "wait", wait_subr, 1, 1, 1);
    initialize_subr(ic, "int", int_subr, 1, 1, 1);
    initialize_subr(ic, "round", round_subr, 1, 1, 1);
    initialize_subr(ic, "sqrt", sqrt_subr, 1, 1, 1);
    initialize_subr(ic, "exp", exp_subr, 1, 1, 1);
    initialize_subr(ic, "log10", log10_subr, 1, 1, 1);
    initialize_subr(ic, "ln", ln_subr, 1, 1, 1);
    initialize_subr(ic, "radsin", radsin_subr, 1, 1, 1);
    initialize_subr(ic, "radcos", radcos_subr, 1, 1, 1);
    initialize_subr(ic, "radarctan", radarctan_subr, 1, 1, 1);
    initialize_subr(ic, "sin", sin_subr, 1, 1, 1);
    initialize_subr(ic, "cos", cos_subr, 1, 1, 1);
    initialize_subr(ic, "arctan", arctan_subr, 1, 1, 1);
    initialize_subr(ic, "bitnot", bitnot_subr, 1, 1, 1);
    initialize_subr(ic, "bitand", bitand_subr, 2, 2, 2);
    initialize_subr(ic, "bitor", bitor_subr, 2, 2, 2);
    initialize_subr(ic, "bitxor", bitxor_subr, 2, 2, 2);
    initialize_subr(ic, "ashift", ashift_subr, 2, 2, 2);
    initialize_subr(ic, "lshift", lshift_subr, 2, 2, 2);
    initialize_subr(ic, "power", power_subr, 2, 2, 2);
    initialize_subr(ic, ".setfirst", dot_setfirst_subr, 2, 2, 2);
    initialize_subr(ic, ".setbf", dot_setbf_subr, 2, 2, 2);
    initialize_subr(ic, "ascii", ascii_subr, 1, 1, 1);
    initialize_subr(ic, "uppercase", uppercase_subr, 1, 1, 1);
    initialize_subr(ic, "lowercase", lowercase_subr, 1, 1, 1);
    initialize_subr(ic, "substringp", substringp_subr, 2, 2, 2);
    initialize_subr(ic, "beforep", beforep_subr, 2, 2, 2);

    initialize_subr(ic, "array", array_subr, 1, 1, 2);
    initialize_subr(ic, "setitem", setitem_subr, 3, 3, 3);
    initialize_subr(ic, ".setitem", setitem_subr, 3, 3, 3);
    initialize_subr(ic, "item", item_subr, 2, 2, 2);
    initialize_subr(ic, "count", count_subr, 1, 1, 1);
    initialize_subr(ic, "arrayp", arrayp_subr, 1, 1, 1);
    initialize_subr(ic, "array?", arrayp_subr, 1, 1, 1);

    initialize_subr(ic, "and", and_subr, 0, 2, INT_MAX);
    initialize_subr(ic, "or", or_subr, 0, 2, INT_MAX);
    initialize_subr(ic, "continuation_stacktrace", continuation_stacktrace,
                        0, 0, 0);
    initialize_subr(ic, "environment_stacktrace", environment_stacktrace,
                        0, 0, 0);
    initialize_subr(ic, "cons_proc_cache", cons_proc_cache_subr,
                        1, 1, 1);
    initialize_subr(ic, "set_cons_proc_cache", set_cons_proc_cache_subr,
                        2, 2, 2);
    initialize_subr(ic, "cons_tree_cache", cons_tree_cache_subr,
                        1, 1, 1);
    initialize_subr(ic, "set_cons_tree_cache", set_cons_tree_cache_subr,
                        2, 2, 2);
    initialize_subr(ic, "gc_node_size", gc_node_size, 0, 0, 0);
    initialize_subr(ic, "#", hash_subr, 0, 0, 0);
    initialize_subr(ic, "repcount", hash_subr, 0, 0, 0);
    initialize_subr(ic, "?", question_subr, 0, 0, 1);

    initialize_subr(ic, "%toot", toot, 7, 7, 7);
    initialize_subr(ic, "%tootreplace", tootreplace, 7, 7, 7);
    initialize_subr(ic, "tootend", tootend, 1, 1, 1);
    initialize_subr(ic, "tootclear", tootclear, 0, 0, 0);
    initialize_subr(ic, "%timeleft", timeleft, 1, 1, 1);
    initialize_subr(ic, "audiodevices", audiodevices, 0, 0, 0);
    initialize_subr(ic, "startcapture", startcapture, 3, 3, 3);
    initialize_subr(ic, "stopcapture", stopcapture, 0, 0, 0);
    initialize_subr(ic, "frequencypowers", frequencypowers, 1, 1, 1);
    initialize_subr(ic, "startdtmf", startdtmf, 2, 2, 2);
    initialize_subr(ic, "fetchdtmf", fetchdtmf, 0, 0, 0);
    initialize_subr(ic, "silence_audio_logging", silence_audio_logging, 0, 0, 0);
    initialize_subr(ic, "time", time_subr, 0, 0, 0);

    if(!ic->terminal_mode) {
        initialize_subr(ic, "wxedit", wxedit_subr, 1, 1, 1);
        initialize_subr(ic, "tsetxy", tsetxy, 3, 3, 3);
        initialize_subr(ic, "tsetx", tsetx, 2, 2, 2);
        initialize_subr(ic, "tsety", tsety, 2, 2, 2);
        initialize_subr(ic, "tsetheading", tsetheading, 2, 2, 2);
        initialize_subr(ic, "tleft", tleft_subr, 2, 2, 2);
        initialize_subr(ic, "tright", tright_subr, 2, 2, 2);
        initialize_subr(ic, "tforward", tforward_subr, 2, 2, 2);
        initialize_subr(ic, "tback", tback_subr, 2, 2, 2);
        initialize_subr(ic, "clean", clean, 0, 0, 0);
        initialize_subr(ic, "clearscreen", clearscreen, 0, 0, 0);
        initialize_subr(ic, "cs", clearscreen, 0, 0, 0);
        initialize_subr(ic, "tarc", tarc, 3, 3, 3);
        initialize_subr(ic, "txcor", txcor, 1, 1, 1);
        initialize_subr(ic, "tycor", tycor, 1, 1, 1);
        initialize_subr(ic, "theading", theading, 1, 1, 1);
        initialize_subr(ic, "trotation", trotation, 1, 1, 1);
        initialize_subr(ic, "tsetrotation", tsetrotation, 2, 2, 2);
        initialize_subr(ic, "trotate", trotate, 2, 2, 2);
        initialize_subr(ic, "fetchturtles", fetchturtles, 0, 0, 0);
        initialize_subr(ic, "deleteturtle", deleteturtle, 1, 1, 1);
        initialize_subr(ic, "tpenup", tpenup, 1, 1, 1);
        initialize_subr(ic, "tpendown", tpendown, 1, 1, 1);
        initialize_subr(ic, "tshowturtle", tshowturtle, 1, 1, 1);
        initialize_subr(ic, "thideturtle", thideturtle, 1, 1, 1);
        initialize_subr(ic, "tbitmap", tbitmap, 4, 4, 4);
        initialize_subr(ic, "tpath", tpath, 1, 1, 1);
        initialize_subr(ic, "twrap", twrap, 1, 1, 1);
        initialize_subr(ic, "twindow", twindow, 1, 1, 1);
        initialize_subr(ic, "tfence", tfence, 1, 1, 1);
        initialize_subr(ic, "tturtlemode", tturtlemode, 1, 1, 1);
        initialize_subr(ic, "tshown", tshown, 1, 1, 1);
        initialize_subr(ic, "tpendownp", tpendownp, 1, 1, 1);
        initialize_subr(ic, "%setbg", intsetbg, 3, 3, 3);
        initialize_subr(ic, "tsetpencolor", tsetpencolor, 4, 4, 4);
        initialize_subr(ic, "tsetpensize", tsetpensize, 2, 2, 2);
        initialize_subr(ic, "tpensize", tpensize, 1, 1, 1);
        initialize_subr(ic, "tcapturebitmap", tcapturebitmap, 8, 8, 8);
        initialize_subr(ic, "captureshape", captureshape, 8, 8, 8);
        initialize_subr(ic, "tsetshape", tsetshape, 2, 2, 2);
        initialize_subr(ic, "autorefreshon", autorefreshon, 0, 0, 0);
        initialize_subr(ic, "autorefreshoff", autorefreshoff, 0, 0, 0);
        initialize_subr(ic, "refresh", refresh, 0, 0, 0);
        initialize_subr(ic, "tsetxscale", tsetxscale, 2, 2, 2);
        initialize_subr(ic, "tsetyscale", tsetyscale, 2, 2, 2);
        initialize_subr(ic, "txscale", txscale, 1, 1, 1);
        initialize_subr(ic, "tyscale", tyscale, 1, 1, 1);
        initialize_subr(ic, "saveturtle", saveturtle, 2, 2, 2);
        initialize_subr(ic, "tover", tover, 4, 4, 4);
        initialize_subr(ic, "txyover", txyover, 6, 6, 6);
        initialize_subr(ic, "tmoveuntil", tmoveuntil, 6, 6, 6);
        initialize_subr(ic, "ttouching", ttouching, 2, 2, 2);
        initialize_subr(ic, "txytouching", txytouching, 4, 4, 4);
        initialize_subr(ic, "twhostouching", twhostouching, 1, 1, 1);
        initialize_subr(ic, "txywhostouching", txywhostouching, 3, 3, 3);
        initialize_subr(ic, "whosover", whosover, 2, 2, 2);
        initialize_subr(ic, "tpasttop", tpasttop, 1, 1, 1);
        initialize_subr(ic, "tpastbottom", tpastbottom, 1, 1, 1);
        initialize_subr(ic, "tpastleft", tpastleft, 1, 1, 1);
        initialize_subr(ic, "tpastright", tpastright, 1, 1, 1);
        initialize_subr(ic, "tpenpaint", tpenpaint, 1, 1, 1);
        initialize_subr(ic, "tpenerase", tpenerase, 1, 1, 1);
        initialize_subr(ic, "background", background, 0, 0, 0);
        initialize_subr(ic, "bg", background, 0, 0, 0);
        initialize_subr(ic, "tpencolor", tpencolor, 1, 1, 1);
        initialize_subr(ic, "tcolorunder", tcolorunder, 1, 1, 1);
        initialize_subr(ic, "txycolorunder", txycolorunder, 3, 3, 3);
        initialize_subr(ic, "tpenmode", tpenmode, 1, 1, 1);
        initialize_subr(ic, "tfill", tfill, 1, 1, 1);
        initialize_subr(ic, "tlabel", tlabel, 2, 2, 2);
        initialize_subr(ic, "tsettext", tsettext, 2, 2, 2);
        initialize_subr(ic, "tgettext", tgettext, 1, 1, 1);
        initialize_subr(ic, "tstamp", tstamp, 1, 1, 1);
        initialize_subr(ic, "savepict", savepict, 1, 1, 1);
        initialize_subr(ic, "loadpict", loadpict, 1, 1, 1);
        initialize_subr(ic, "pollkey", pollkey, 1, 1, 1);
    }
}
