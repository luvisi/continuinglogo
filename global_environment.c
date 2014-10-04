
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
#include "global_environment.h"
#include "interpreter.h"
#include "gc.h"
#include "list_memory.h"
#include "reader.h"
#include "logoreader.h"
#include "treeify.h"
#include "io.h"
#include "config.h"


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
    if(first == second)
        return ic->n_true;

    if(name_eq(ic->n_caseignoredp->u.name.symbol->value, ic->n_true) &&
       name_eq(first, second))
        return ic->n_true;
      
    /* Only attempt a numeric comparison if both arguments could be
       converted to numbers. */
    if(numberp(ic, first) && numberp(ic, second) &&
       to_number(ic, first)->u.number.value ==
       to_number(ic, second)->u.number.value)
        return ic->n_true;
    return ic->n_false;
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
    if(s->t == CONS) {
        sexpr *oper = car(s);
        return mk_funarg(ic, oper, ic->frame);
    }
    bad_argument(ic, car(s));
}

/* Logo Assignment. */
sexpr *make(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(first->t == NAME) {
        if(first->u.name.symbol->flags & VAL_TRACED) {
            sexpr *old_fullprint = ic->n_fullprintp->u.name.symbol->value;
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
        }

        STORE(ic->g, first->u.name.symbol,
                     first->u.name.symbol->value, second);
        return ic->g_unbound;
    }
    bad_argument(ic, first);
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
sexpr *not(IC *ic, sexpr *s) {
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
            int sub_printed_any = 0;
            while(l->t == CONS) {
                if(sub_printed_any)
                    lprintf(ic, " ");
                else
                    sub_printed_any = 1;

                lprint_sexpr(ic, car(l));
                l = cdr(l);
            }
        } else {
            lprint_sexpr(ic, car(s));
        }
        s = cdr(s);
    }

    lprintf(ic, "\n");
    return ic->g_unbound;
}

/* SHOW
   Spaces between arguments.
   Brackets shown around lists.
   Newline at the end.
 */
   
sexpr *show_subr(IC *ic, sexpr *s) {
    int printed_any = 0; /* State variable for printing a space before
                            every item but the first. */
    while(s->t == CONS) {
        if(printed_any)
            lprintf(ic, " ");
        else
            printed_any = 1;
        lprint_sexpr(ic, car(s));
        s = cdr(s);
    }

    lprintf(ic, "\n");
    return ic->g_unbound;
}

/* TYPE
   No spaces between arguments.
   List arguments are printed without brackets, but still have spaces
   between their individual members.
   No newline at the end.
 */
sexpr *type_subr(IC *ic, sexpr *s) {
    while(s->t == CONS) {
        /* Special handling for lists so that we do not print
           brackets around top level lists. */
        if(car(s)->t == CONS) {
            sexpr *l = car(s);
            int sub_printed_any = 0;  /* We don't separate arguments with
                                         spaces, but we do separate elements
                                         of a list we are passed. */
            while(l->t == CONS) {
                if(sub_printed_any)
                    lprintf(ic, " ");
                else
                    sub_printed_any = 1;

                lprint_sexpr(ic, car(l));
                l = cdr(l);
            }
        } else {
            lprint_sexpr(ic, car(s));
        }
        s = cdr(s);
    }

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
        return new_name(ic, name->u.name.head,
                            name->u.name.name,
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

/* Is the argument a number?
   The argument is a number if it is a NUMBER or if it is a NAME
   that can be converted to a NUMBER. */
sexpr *numberp_sexpr(IC *ic, sexpr *s) {
    if(numberp(ic, car(s)))
        return ic->n_true;
    else
        return ic->n_false;
}

/* Is the argument a NAME? */
sexpr *namep(IC *ic, sexpr *s) {
    if(car(s)->t == NAME && car(s)->u.name.symbol->value != ic->g_unbound)
        return ic->n_true;
    else
        return ic->n_false;
}


/* Add numbers. */
sexpr *add(IC *ic, sexpr *s) {
    double ret = 0;

    while(!is_nil(ic, s)) {
        ret = ret + to_number(ic, car(s))->u.number.value;
        s = cdr(s);
    }
    return mk_number(ic, ret);
}

/* Subtract.
   If given no arguments, returns 0.
   If given one argument, return its negation.
   If given multiple arguments, returns the first minus the remaining
   arguments. */
sexpr *sub(IC *ic, sexpr *s) {
    double ret = 0;

    if(!is_nil(ic, s)) {
        if(is_nil(ic, cdr(s))) {
            ret = -to_number(ic, car(s))->u.number.value;
        } else {
            ret = to_number(ic, car(s))->u.number.value;
            s = cdr(s);
            while(!is_nil(ic, s)) {
                ret = ret - to_number(ic, car(s))->u.number.value;
                s = cdr(s);
            }
        }
    }

    return mk_number(ic, ret);
}

/* Multiply all of the arguments together. */
sexpr *mul(IC *ic, sexpr *s) {
    double ret = 1;
    while(!is_nil(ic, s)) {
        ret = ret * to_number(ic, car(s))->u.number.value;
        s = cdr(s);
    }
    return mk_number(ic, ret);
}

/* Divide two numbers. */
sexpr *div_subr(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    return mk_number(ic, to_number(ic, first)->u.number.value /
                         to_number(ic, second)->u.number.value);
}

/* Take the remainder of first/second.
   Result is the same sign as first. */
sexpr *remainder_subr(IC *ic, sexpr *s) {
    int first = (int)trunc(to_number(ic, car(s))->u.number.value);
    int second = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    return mk_number(ic, first % second);
}

/* Take the remainder of first/second.
   Result is the same sign as second. */
sexpr *modulo_subr(IC *ic, sexpr *s) {
    int first = (int)trunc(to_number(ic, car(s))->u.number.value);
    int second = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);
    int ret = first % second;
    if((ret < 0) != (second < 0))
        ret += second;
    return mk_number(ic, ret);
}

/* greater than */
sexpr *gt(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(to_number(ic, first)->u.number.value >
       to_number(ic, second)->u.number.value)
        return ic->n_true;
    else
        return ic->n_false;
}


/* less than */
sexpr *lt(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(to_number(ic, first)->u.number.value <
       to_number(ic, second)->u.number.value)
        return ic->n_true;
    else
        return ic->n_false;
}

/* greater than or equal */
sexpr *ge(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(to_number(ic, first)->u.number.value >=
       to_number(ic, second)->u.number.value)
        return ic->n_true;
    else
        return ic->n_false;
}

/* less than or equal */
sexpr *le(IC *ic, sexpr *s) {
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    if(to_number(ic, first)->u.number.value <=
       to_number(ic, second)->u.number.value)
        return ic->n_true;
    else
        return ic->n_false;
}

/* not equal */
sexpr *ne(IC *ic, sexpr *s) {
    sexpr *are_equal = eq(ic, s);
    if(name_eq(are_equal, ic->n_false))
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

/* Return the list of interned names. */
sexpr *oblist(IC *ic, sexpr *s) {
    return ic->name_list;
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
    /* Return the first element of a list */
    if(e->t == CONS)
        return car(e);

    /* Return the origin, also known as the first element's index. */
    if(e->t == ARRAY)
        return mk_number(ic, e->u.array.origin);

    /* Attempt to convert the argument to a name and return a name
       containing only the first letter of the argument. */
    e = to_name(ic, e);
    if(e->t == NAME) {
        if(e->u.name.length >= 1) {
            return intern_len(ic, e->u.name.head, e->u.name.name, 1);
        }
    }
    bad_argument(ic, e);
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
    /* cdr of a list */
    if(e->t == CONS)
        return cdr(e);

    /* For names, create a new name with everything but the first
       character. */
    e = to_name(ic, e);
    if(e->t == NAME) {
        if(e->u.name.length >= 1) {
            return intern_len(ic, e->u.name.head, e->u.name.name+1, e->u.name.length-1);
        }
    }
    bad_argument(ic, e);
}

/* SUBR for BUTFIRST. */
sexpr *butfirst_subr(IC *ic, sexpr *s) {
    return butfirst(ic, car(s));
}

/* LAST - fetch the last member of a list or name. */
sexpr *last(IC *ic, sexpr *s) {
    sexpr *e = car(s);
    if(e->t == CONS) {
        while(e->t == CONS && !is_nil(ic, cdr(e)))
            e = cdr(e);
        if(e->t == CONS)
            return car(e);
    }
    e = to_name(ic, e);
    if(e->t == NAME) {
        if(e->u.name.length >= 1) {
            return intern_len(ic, e->u.name.head, e->u.name.name+e->u.name.length-1, 1);
        }
    }
    bad_argument(ic, car(s));
}

/* BUTLAST - Return all but the last element of a list or a name. */
sexpr *butlast(IC *ic, sexpr *s) {
    sexpr *e = car(s);
    if(e->t == CONS) {
        /* It's a list.  Create a new list destructively so we can
           do it in a loop. */
        sexpr *ret = ic->g_nil;
        sexpr **nextp = &ret;
        protect_ptr(ic->g, (void **)&ret);
        while(e->t == CONS && !is_nil(ic, cdr(e))) {
            STORE(ic->g, NULL, *nextp, cons(ic, car(e), ic->g_nil));
            nextp = &cdr(*nextp);
            e = cdr(e);
        }
        unprotect_ptr(ic->g);
        if(is_nil(ic, cdr(e)))
            return ret;
    }

    /* Try to convert it to a name and create a new name not containing
       the last character. */
    e = to_name(ic, e);
    if(e->t == NAME) {
        if(e->u.name.length >= 1) {
            return intern_len(ic, e->u.name.head, e->u.name.name, e->u.name.length-1);
        }
    }
    bad_argument(ic, car(s));
}

/* Merge zero or more words into one word. */
sexpr *word(IC *ic, sexpr *e) {
    char *new_word;
    int i, len;
    sexpr *current = e;
    sexpr *ret;

    /* Calculate the full length the new name will need. */
    for(current = e, len = 0; !is_nil(ic, current); current = cdr(current))
        if(car(current)->t == NAME || car(current)->t == NUMBER)
            len += to_name(ic, car(current))->u.name.length;

    /* Create the char array for the new name and protect it during
       the calls to to_name() which might allocate memory if passed
       a number. */
    new_word = ic_xmalloc(ic, len, mark_cstring);
    protect(ic->g, new_word);
    for(current = e, i = 0; !is_nil(ic, current); current = cdr(current))
        if(car(current)->t == NAME || car(current)->t == NUMBER) {
            sexpr *tmp = to_name(ic, car(current));
            strncpy(new_word+i, tmp->u.name.name, 
                                tmp->u.name.length);
            i += tmp->u.name.length;
        }
    ret = intern_len(ic, new_word, new_word, len);
    unprotect(ic->g);
    return ret;
}

/* Is the argument the empty word or the empty list? */
sexpr *emptyp(IC *ic, sexpr *s) {
    if(is_nil(ic, car(s)) || car(s) == ic->n_empty)
        return ic->n_true;
    else
        return ic->n_false;
}

/* Merge multiple things into one sentence.
   Sentences in the arguments get flattened. */
sexpr *sentence(IC *ic, sexpr *s) {
    sexpr *ret = ic->g_nil;
    sexpr **nextp = &ret;
    protect_ptr(ic->g, (void **)&ret);

    while(!is_nil(ic, s)) {
        if(car(s)->t == CONS) {
            sexpr *e = car(s);
            while(e->t == CONS) {
                STORE(ic->g, NULL, *nextp, cons(ic, car(e), ic->g_nil));
                nextp = &cdr(*nextp);
                e = cdr(e);
            }
        } else if(!is_nil(ic, car(s))) {
            STORE(ic->g, NULL, *nextp, cons(ic, car(s), ic->g_nil));
            nextp = &cdr(*nextp);
        }
        s = cdr(s);
    }

    unprotect_ptr(ic->g);
    return ret;
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
        int num1 = (int)abs(trunc(to_number(ic, car(s))->u.number.value));
        int num2 = (int)abs(trunc(to_number(ic, car(cdr(s)))->u.number.value));
        int range;
        if(num1 > num2)
            return mk_number(ic, 0);
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
    /* For handling the one argument version where the argument is
       a list of names. */
    if(s->t == CONS && car(s)->t == CONS)
        s = car(s);

    while(s->t == CONS) {
        if(car(s)->t != NAME)
            bad_argument(ic, car(s));
        add_local(ic, ic->continuation->frame, car(s));
        s = cdr(s);
    }
    return ic->g_unbound;
}

/* Fetch the value of the name passed. */
sexpr *thing(IC *ic, sexpr *s) {
    if(car(s)->u.name.symbol->value == ic->g_unbound) {
        eprint_sexpr(ic, car(s));
        eprintf(ic, " has no value");
        throw_error(ic, ic->continuation->line);
    } else if(car(s)->t == NAME) {
        return car(s)->u.name.symbol->value;
    } else {
        bad_argument(ic, car(s));
    }
}

/* Make a LINE object.
   Used while reading in a Logo procedure. */
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

/* Used by (treeify_body) in initialize.txt to set the current line
   before calling (treeify) so that error messages from (treeify) will
   have the line being parsed and not the line being executed. */
sexpr *set_current_line(IC *ic, sexpr *s) {
    if(car(s)->t == LINE) {
        if(ic->continuation->parent != NULL) {
            STORE(ic->g, NULL, ic->continuation->parent->line, car(s));
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
    return intern_len(ic, NULL, &ch, 1);
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
    sexpr *var = car(s);
    sexpr *tag = car(cdr(s));
    sexpr *value = car(cdr(cdr(s)));

    if(var->t == NAME &&
       var->u.name.symbol->flags & PLIST_TRACED) {
        sexpr *old_fullprint = ic->n_fullprintp->u.name.symbol->value;
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
    }

    pprop(ic, car(s), car(cdr(s)), car(cdr(cdr(s))));
    return ic->g_unbound;
}

sexpr *gprop_subr(IC *ic, sexpr *s) {
    return gprop(ic, car(s), car(cdr(s)));
}

sexpr *remprop_subr(IC *ic, sexpr *s) {
    remprop(ic, car(s), car(cdr(s)));
    return ic->g_unbound;
}

sexpr *plist_subr(IC *ic, sexpr *s) {
    return plist(ic, car(s));
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
    if(s->t == CONS)
        eprint_sexpr(ic, car(s));
    throw_error(ic, ic->continuation->line);
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
    sexpr *ret;
    logoreader *lr = mk_logoreader(ic);
    char *wordstring = get_cstring(ic, to_name(ic, car(s)));
    protect(ic->g, wordstring);
    logoread_from_string(lr, get_cstring(ic, to_name(ic, car(s))));
    ret = readlist(lr);
    unprotect(ic->g);
    return ret;
}



/* These are the helper functions that manipulate flags.  They are used
   by BURY, UNBURY, BURIEDP,
      TRACE, UNTRACE, TRACEDP, 
      STEP, STEPPED, STEPPEDP
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
        l = cdr(l);
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
    sexpr *arg = car(args);
    sexpr *orig_arg = arg; /* In case we need to throw an error */

    if(arg->t == NAME) {
        flaglist_helper(ic, args, procmask, procflag, orig_arg);
    } else if(arg->t == CONS && car(arg)->t == NAME) {
        flaglist_helper(ic, arg, procmask, procflag, orig_arg);
    } else {
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                flaglist_helper(ic, car(arg), procmask, procflag, orig_arg);
            arg = cdr(arg);
        }
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                flaglist_helper(ic, car(arg), varmask, varflag, orig_arg);
            arg = cdr(arg);
        }
        if(arg->t == CONS) {
            if(car(arg)->t == CONS)
                flaglist_helper(ic, car(arg), plmask, plflag, orig_arg);
            arg = cdr(arg);
        }
        if(!is_nil(ic, arg))
            bad_argument(ic, orig_arg);
    }
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

/* To erase a property list, just set ot to the empty list. */
static void erase_plist(IC *ic, sexpr *name, sexpr *orig_arg) {
    if(name->t != NAME)
        bad_argument(ic, orig_arg);

    STORE(ic->g, name->u.name.symbol, name->u.name.symbol->properties, ic->g_nil);
}

/* Erase the first thing we find in the passed contentslist. */
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
    exit(0);
}

/* Fetch an environmental variable. */
sexpr *getenv_subr(IC *ic, sexpr *s) {
    char *cstring = get_cstring(ic, to_name(ic, car(s)));
    char *value = getenv(cstring);
    if(value == NULL)
        return ic->g_nil;
    else
        return intern(ic, value);
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
    sexpr *arg = car(s);
    sexpr *proc, *funarg, *macro, *oper;
    sexpr *min, *def, *max, *ret;

    if(!get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 1))
        bad_argument(ic, arg);

    if(is_nil(ic, proc))
        bad_argument(ic, arg);

    min = protect(ic->g, mk_number(ic, proc->u.proc.minargs));
    def = protect(ic->g, mk_number(ic, proc->u.proc.defargs));

    if(proc->u.proc.maxargs == INT_MAX)
        max = protect(ic->g, mk_number(ic, -1));
    else
        max = protect(ic->g, mk_number(ic, proc->u.proc.maxargs));

    ret = cons(ic, min,
                   cons(ic, def,
                            cons(ic, max, ic->g_nil)));
    unprotect(ic->g);
    unprotect(ic->g);
    unprotect(ic->g);
    return ret;
}

/* Is the argument the name of a procedure of any kind? */
sexpr *procedurep_subr(IC *ic, sexpr *s) {
    sexpr *arg = car(s);
    sexpr *proc, *funarg, *macro, *oper;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, oper))
        return ic->n_false;

    return ic->n_true;
}

/* Is the argument the name of a primitive?
   We cheat a little here.  Procedures defined in Lisp are
   considered primitives because you can't get their TEXT in
   Logo. */
sexpr *primitivep_subr(IC *ic, sexpr *s) {
    sexpr *arg = car(s);
    sexpr *proc, *funarg, *macro, *oper;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, oper) ||
       !is_nil(ic, arg->u.name.symbol->function_source))
        return ic->n_false;

    return ic->n_true;
}

/* Is the argument the name of a procedure that was defined in Logo? */
sexpr *definedp_subr(IC *ic, sexpr *s) {
    sexpr *arg = car(s);
    sexpr *proc, *funarg, *macro, *oper;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, oper) ||
       is_nil(ic, arg->u.name.symbol->function_source))
        return ic->n_false;

    return ic->n_true;
}

/* Is the argument the name of a macro? */
sexpr *macrop_subr(IC *ic, sexpr *s) {
    sexpr *arg = car(s);
    sexpr *proc, *funarg, *macro, *oper;

    if(arg->t != NAME ||
       !get_apply_parts(ic, arg, &proc, &funarg, &macro, &oper, 0) ||
       is_nil(ic, macro) ||
       is_nil(ic, arg->u.name.symbol->function_source))
        return ic->n_false;

    return ic->n_true;
}

/* COPYDEF "FOO "BAR
   Makes FOO's behavior as a procedure be a copy of BAR's
   behavior as a procedure. */
sexpr *copydef_subr(IC *ic, sexpr *s) {
    sexpr *new = car(s);
    sexpr *old = car(cdr(s));

    sexpr *newproc, *newfunarg, *newmacro, *newoper;
    sexpr *oldproc, *oldfunarg, *oldmacro, *oldoper;

    /* Fetch the default arity of the two procedures and compare them.
       If they are different, we need to reassign
       treeify_cache_generation to invalidate the cached treeified bodies
       of all procedures. */
    if(get_apply_parts(ic, new, &newproc, &newfunarg, &newmacro, &newoper, 0)
       && !is_nil(ic, newproc)) {
        if(!get_apply_parts(ic, old, 
                           &oldproc, &oldfunarg, &oldmacro, &oldoper, 1) ||
           is_nil(ic, oldproc)) {
            bad_argument(ic, old);
        } else {
            int newdefargs = new->u.proc.defargs;
            int olddefargs = old->u.proc.defargs;
            if(newdefargs != olddefargs) {
                STORE(ic->g,
                      ic->n_treeify_cache_generation->u.name.symbol,
                      ic->n_treeify_cache_generation->u.name.symbol->value,
                      cons(ic, ic->g_nil, ic->g_nil));
            }
        }
        
    }



    if(new->t != NAME)
        bad_argument(ic, new);
    if(old->t != NAME)
        bad_argument(ic, old);



    STORE(ic->g, new->u.name.symbol,
                 new->u.name.symbol->function,
                 old->u.name.symbol->function);
    STORE(ic->g, new->u.name.symbol,
                 new->u.name.symbol->function_source,
                 old->u.name.symbol->function_source);

    return ic->g_unbound;
}

/* FORM - Created formated version of a number. */
sexpr *form_subr(IC *ic, sexpr *args) {
    int len;
    char *buf;
    sexpr *num = car(args),
          *width = car(cdr(args)),
          *precision = car(cdr(cdr(args)));

    num = to_number(ic, num);
    width = to_number(ic, width);


    if(width->u.number.value >= 0) {
        precision = to_number(ic, precision);
    } else {
        precision = to_name(ic, precision);
    }


    if(width->u.number.value >= 0) {
        /* Normal */
        int widthi = (int)trunc(width->u.number.value),
            precisioni = (int)trunc(precision->u.number.value);
        
        len = snprintf(NULL, 0, "%*.*f",
                                widthi,
                                precisioni,
                                num->u.number.value);
        buf = ic_xmalloc(ic, len+1, mark_cstring);
        len = snprintf(buf, len+1, "%*.*f",
                                   widthi,
                                   precisioni,
                                   num->u.number.value);
    } else {
        /* hack where the precision is treated as a
           format string for sprintf */
        char *format = get_cstring(ic, precision);
        len = snprintf(NULL, 0, format, num->u.number.value);
        buf = ic_xmalloc(ic, len+1, mark_cstring);
        len = snprintf(buf, len+1, format, num->u.number.value);
    }

    return intern_len(ic, buf, buf, len);
}

/* WAIT N
   Pauses for N 60th's of a second.
 */
sexpr *wait_subr(IC *ic, sexpr *s) {
    int count;
    unsigned int seconds;
    useconds_t useconds;

    count = (int)trunc(to_number(ic, car(s))->u.number.value);
    seconds = count / 60;
    useconds = (count % 60) * 16667;
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
static double degreesin(double degrees) {
    /* Brian Harvey claims that Kahan says that you should only convert
       degrees in the range of 0-45 into radians for sin and cos functions
       in order to reduce the impact of round off errors in the conversion.

       This funky algorithm uses identities to get "degrees" into that range
       before converting and calling sin() or cos().
     */
    int sign;
    double ret;

    /* Set up an invariant.  Call d0 the original value of degrees.
       sin(d0) = sin(degrees) * (sin ? -1 : 1)
 
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
static double degreecos(double degrees) {
    return degreesin(90.0-degrees);
}

/* ARCTAN N
   Return the arctan of N in degrees.
 */
static double degreeatan(double degrees) {
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
    return mk_number(ic,
                     ((int)trunc(to_number(ic, car(s))->u.number.value)) &
                     ((int)trunc(to_number(ic, car(cdr(s)))->u.number.value)));
}

sexpr *bitor_subr(IC *ic, sexpr *s) {
    return mk_number(ic,
                     ((int)trunc(to_number(ic, car(s))->u.number.value)) |
                     ((int)trunc(to_number(ic, car(cdr(s)))->u.number.value)));
}

sexpr *bitxor_subr(IC *ic, sexpr *s) {
    return mk_number(ic,
                     ((int)trunc(to_number(ic, car(s))->u.number.value)) ^
                     ((int)trunc(to_number(ic, car(cdr(s)))->u.number.value)));
}

/* Arithmetic shift
   Sign extended.
   Negative values represent a right shift.
 */
sexpr *ashift_subr(IC *ic, sexpr *s) {
    int val = (int)trunc(to_number(ic, car(s))->u.number.value);
    int shift = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);

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
    int val = (int)trunc(to_number(ic, car(s))->u.number.value);
    int shift = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);

    if(shift >= 0)
        val <<= shift;
    else 
        val = (unsigned)val >> -shift;

    return mk_number(ic, val);
}

/* POWER N M -> N^M */
sexpr *power_subr(IC *ic, sexpr *s) {
    double base = to_number(ic, car(s))->u.number.value;
    double exponent = to_number(ic, car(cdr(s)))->u.number.value;
    return mk_number(ic, pow(base, exponent));
}

/* Dangerous mutators .SETFIRST and .SETBF */
sexpr *dot_setfirst_subr(IC *ic, sexpr *s) {
    sexpr *l = car(s);
    sexpr *thing = car(cdr(s));

    if(l->t != CONS)
        bad_argument(ic, l);

    STORE(ic->g, l, l->u.cons.car, thing);
    return ic->g_unbound;
}

sexpr *dot_setbf_subr(IC *ic, sexpr *s) {
    sexpr *l = car(s);
    sexpr *thing = car(cdr(s));

    if(l->t != CONS)
        bad_argument(ic, l);

    STORE(ic->g, l, l->u.cons.cdr, thing);
    return ic->g_unbound;
}

/* Return the ascii value of the first character in the word.
   ASCII "A -> 65
 */
sexpr *ascii_subr(IC *ic, sexpr *s) {
    sexpr *arg = to_name(ic, car(s));

    if(arg->u.name.length < 1)
        bad_argument(ic, arg);

    return mk_number(ic, arg->u.name.name[0]);
}

/* Uppercase all of the letters in the argument. */
sexpr *uppercase_subr(IC *ic, sexpr *s) {
    sexpr *arg = to_name(ic, car(s));
    int len = arg->u.name.length;
    char *buf = ic_xmalloc(ic, len, mark_cstring);
    int i;

    for(i = 0; i < len; i++)
        buf[i] = toupper(arg->u.name.name[i]);

    return intern_len(ic, buf, buf, len);
}

/* Lowercase all of the letters in the argument. */
sexpr *lowercase_subr(IC *ic, sexpr *s) {
    sexpr *arg = to_name(ic, car(s));
    int len = arg->u.name.length;
    char *buf = ic_xmalloc(ic, len, mark_cstring);
    int i;

    for(i = 0; i < len; i++)
        buf[i] = tolower(arg->u.name.name[i]);

    return intern_len(ic, buf, buf, len);
}

/* Does the first string appear in the second one?
   Does not work on strings containing NULL bytes. */
sexpr *substringp_subr(IC *ic, sexpr *s) {
    sexpr *needle = car(s);
    sexpr *haystack = car(cdr(s));
    char *ns, *hs, *cp;

    if((needle->t != NAME && needle->t != NUMBER) ||
       (haystack->t != NAME && haystack->t != NUMBER))
        return ic->n_false;

    ns = get_cstring(ic, to_name(ic, needle));
    hs = get_cstring(ic, to_name(ic, haystack));
    if(name_eq(ic->n_caseignoredp->u.name.symbol->value, ic->n_true)) {
        /* If CASEIGNOREDP is set to "TRUE then we forcibly uppercase
           all characters in both strings before the search. */
        for(cp = ns; *cp; cp++)
            *cp = toupper(*cp);
        for(cp = hs; *cp; cp++)
            *cp = toupper(*cp);
    }

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
    sexpr *first = car(s);
    sexpr *second = car(cdr(s));
    char *fs, *ss;
    int comparison;

    fs = get_cstring(ic, to_name(ic, first));
    ss = get_cstring(ic, to_name(ic, second));

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
    int length = (int)trunc(to_number(ic, car(s))->u.number.value);
    int origin;

    if(is_nil(ic, cdr(s)))
        origin = 1;
    else
        origin = (int)trunc(to_number(ic, car(cdr(s)))->u.number.value);

    return array(ic, length, origin);
}

void range_error(IC *ic, int index, sexpr *thing) {
    eprintf(ic, "Range error - %d is out of range for ", index);
    eprint_sexpr(ic, thing);
    eprintf(ic, "\n");
    throw_error(ic, ic->continuation->line);
}

/* SETITEM N ARRAY I
   Set item N of ARRAY to I.
 */
sexpr *setitem_subr(IC *ic, sexpr *s) {
    int index = (int)trunc(to_number(ic, car(s))->u.number.value);
    int realindex;
    sexpr *array = car(cdr(s));
    sexpr *value = car(cdr(cdr(s)));

    if(array->t != ARRAY)
        bad_argument(ic, array);

    realindex = index - array->u.array.origin;
    if(realindex < 0 || realindex >= array->u.array.length)
        range_error(ic, index, array);

    STORE(ic->g, array->u.array.members,
                 array->u.array.members[realindex],
                 value);
    return ic->g_unbound;
}

/* ITEM N THING -> Item N of THING
   THING can be an array, a list, or a name.
 */
sexpr *item_subr(IC *ic, sexpr *s) {
    int index = (int)trunc(to_number(ic, car(s))->u.number.value);
    sexpr *thing = car(cdr(s));

    switch(thing->t) {
        case NAME:
            if(index < 1 || index > thing->u.name.length)
                range_error(ic, index, thing);
            return intern_len(ic, thing->u.name.head,
                                  thing->u.name.name + index - 1,
                                  1);
        case CONS:
            {
                int i = index;
                sexpr *e = thing;
                if(i < 1) range_error(ic, i, thing);
                while(i > 1) {
                    i--;
                    e = cdr(e);
                    if(is_nil(ic, e))
                        range_error(ic, index, thing);
                }
                return car(e);
            }
               
        case ARRAY:
            {
                int realindex = index - thing->u.array.origin;
                if(realindex < 0 || realindex >= thing->u.array.length)
                    range_error(ic, index, thing);
                return thing->u.array.members[realindex];
            }
        default:
            bad_argument(ic, thing);
    }
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

sexpr *continuation_stacktrace(IC *ic, sexpr *s) {
  continuation *c = ic->continuation;

  for(;;) {
    tprintf(ic, "    ");
    tprint_sexpr(ic, c->expr);
    tprintf(ic, "\n");
    if(c->parent == NULL)
      break;
    c = c->parent;
  }
  return ic->g_unbound;
}

sexpr *environment_stacktrace(IC *ic, sexpr *s) {
    int i = 0;
    struct frame *f = ic->continuation->frame;
    while(f != NULL) {
        tprintf(ic, "   %5d ", i);
        tprint_sexpr(ic, f->procedure);
        tprintf(ic, "\n");
        i++;
        f = f->parent;
    }
    return ic->g_unbound;
}



/* END SUBR's */

/* Utility function for adding a SUBR to the global environment. */
void initialize_subr(IC *ic, char *namestr,
                             efun f,
                             int minargs, 
                             int defargs, 
                             int maxargs) {

    /* Create the name we will store it in. */
    sexpr *name = intern(ic, namestr);

    /* Wrap the C function in a SUBR object. */
    sexpr *func = mk_subr(ic, name, f);

    /* Wrap the SUBR object in a PROC object containing the minimum,
       default, and maximum argument counts. */
    sexpr *proc = mk_proc(ic, func, minargs, defargs, maxargs);

    /* Store the PROC in the function slot of the symbol associated
       with the name. */
    STORE(ic->g, name->u.name.symbol,
          name->u.name.symbol->function, proc);

}

/* Utility function for adding an FSUBR to the global environment.
   No arity argument - all fsubrs are of arbitrary arity. */
void initialize_fsubr(IC *ic, char *namestr, efun f) {
    /* Create the name we will store it in. */
    sexpr *name = intern(ic, namestr);

    /* Wrap the C function in a FSUBR object. */
    sexpr *func = mk_fsubr(ic, name, f);

    /* Store the FSUBR in the function slot of the symbol associated
       with the name. */
    STORE(ic->g, name->u.name.symbol,
          name->u.name.symbol->function, func);
}

/* Add all of the primitives to the global environment. */
void initialize_global_environment(IC *ic) {
    STORE(ic->g,
          ic->n_logoversion->u.name.symbol,
          ic->n_logoversion->u.name.symbol->value,
          intern(ic, PACKAGE_VERSION));
       


    initialize_subr(ic, "inc", inc, 1, 1, 1);
    initialize_subr(ic, "dec", dec, 1, 1, 1);
    initialize_subr(ic, "eq", eq, 2, 2, 2);
    initialize_subr(ic, ".eq", dot_eq, 2, 2, 2);
    initialize_fsubr(ic, "quote", quote);
    initialize_subr(ic, "internal_function", internal_function, 1, 1, 1);
    initialize_subr(ic, "make", make, 2, 2, 2);
    initialize_subr(ic, "fset", fset, 2, 2, 2);
    initialize_subr(ic, "symbol_function", symbol_function, 1, 1, 1);
    initialize_subr(ic, "source_set", source_set, 2, 2, 2);
    initialize_subr(ic, "symbol_source", symbol_source, 1, 1, 1);
    initialize_subr(ic, "not", not, 1, 1, 1);
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
}
