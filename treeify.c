
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


/* Recursive descent parser for turning a Logo sentence into a list of
   Lisp expressions.

   Each procedure represents one non-terminal in the grammar.

   It takes two input arguments, the interpreter context "ic", and the
   remaining tokens to parse, named "input".

   It returns three values:
       The return value, a 1 or 0, indicating failure or success at
       parsing the production the procedure parses.

       If successful, the Lisp version of the parsed portion of the input,
       returned by modifying the pointer argument "output".

       If successful, the remaining members of the sentence that were not
       parsed, returned by modifying the pointer argument "remaiing".

   Most of these procedures use goto in order to handle cleaning up
   protected pointers in one place.

   Here is the grammar:
    <nothing> =        <- Blank string
    <expr list> = <nothing> |
                  <expr> <expr list>
    <expr> = <stop expr> |
             <sum> [= <sum>]*  { return (= <sum> <sum>) }
    <if expr> = 'IF' <expr> <run list> [<run list>]
    <stop expr> = 'STOP'
    <run list> = <sentence> { return <expr list>(<sentence>) }
    <sum> = <term> [ ('+'|'-') <term>]* { return (+|- <term> <term>) }
    <term> = <factor> [ ('*'|'/') <factor>]* { return (*|/ <factor> <factor>) }
    <factor> = '-' <factor> { return (- <factor>) } | 
               <if expr> |
               <colon name> |
               <quote name> |
               <name> |
               <number> |
               <sentence> |
               <array> |
               <explicit procedure call> |
               '(' <expr> ')' |
               <implicit procedure call>
    <colon name> = ':' NAME  { return NAME }
    <quote name> = '"' NAME  { return (quote NAME) }
    <name> = NAME { return NAME }
    <number> = NUMBER { return NUMBER }
    <sentence> = SENTENCE { return (quote SENTENCE) }
    <sentence> = ARRAY { return (quote ARRAY) }
    <explicit procedure call> = '(' <name> <explicit tail>
    <explicit tail> = ')' |
                      <expr> <explicit tail>
    <implicit procedure call> = NAME <expr>{default_arity(NAME)}

 */

#include "treeify.h"
#include "list_memory.h"
#include "interpreter.h"
#include "global_environment.h"
#include "gc.h"
#include "io.h"

static int expr(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int if_expr(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int stop_expr(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int run_list(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int sum(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int term(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int factor(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int colon_name(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int quote_name(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int sentence(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int array_lit(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int number(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int explicit_procedure_call(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int explicit_tail(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int implicit_procedure_call(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);
static int name(IC *ic, sexpr *input, sexpr **output, sexpr **remaining);


/* Parse a list of expressions.
   This is used for each line of a procedure and for the input to
   RUN.
 */
static int expr_list(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    /* An empty input is acceptable.  We return an empty output. */
    if(is_nil(ic, input)) {
        STORE(ic->g, NULL, *output, ic->g_nil);
        STORE(ic->g, NULL, *remaining, ic->g_nil);
        ret = 1;
        goto exit;
    }


    /* Attempt to parse an expression.  On success the result is in
       sub_output and the input remaining after parsing the expression
       is in sub_remaining. */
    if(!expr(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    /* Attempt to parse another expression_list in the remaining input
       after the first expression.  We are parsing sub_remaining and
       placing the results in sub_output2 and the part remaining after
       parsing in sub_remaining2. */
    if(expr_list(ic, sub_remaining, &sub_output2, &sub_remaining2)) {
        /* If we succeed, we cons sub_output (the result from parsing
           the first expression) onto sub_output2 (the result from
           parsing the rest of the input, which should be a list of
           parsed expressions) and store it in *output (one of the
           output pointer arguments).  sub_remaining2 is the portion
           remaining after all of our parsing.  In this case it will
           always be an empty list, but in the general case, it will be
           whatever portion of the input still needs to be parsed. */
        STORE(ic->g, NULL, *output, cons(ic, sub_output, sub_output2));
        STORE(ic->g, NULL, *remaining, sub_remaining2);
        ret = 1;
        goto exit;
    }


    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Parse one expression. */
static int expr(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    /* If we find a STOP, we succeeded and we're done. */
    if(stop_expr(ic, input, &sub_output, &sub_remaining)) {
        /* Our results are just the results from stop_expr(). */
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    /* Parse a sum (which may contain only one factor).
       If it fails, we fail. */
    if(!sum(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    /* Keep parsing more sums as long as we have comparison operators.
       By doing this with a loop rather than recursion, we make the
       operators associate to the right. */
    while(!is_nil(ic, sub_remaining) &&
          (name_eq(car(sub_remaining), ic->n_equal) ||
           name_eq(car(sub_remaining), ic->n_lt) ||
           name_eq(car(sub_remaining), ic->n_le) ||
           name_eq(car(sub_remaining), ic->n_gt) ||
           name_eq(car(sub_remaining), ic->n_ge) ||
           name_eq(car(sub_remaining), ic->n_ne))) {
        if(!sum(ic, cdr(sub_remaining), &sub_output2, &sub_remaining2)) {
            ret = 0;
            goto exit;
        }
        sub_output = cons(ic, car(sub_remaining),
                              cons(ic, sub_output,
                                       cons(ic, sub_output2, ic->g_nil)));
        sub_remaining = sub_remaining2;
    }

    STORE(ic->g, NULL, *output, sub_output);
    STORE(ic->g, NULL, *remaining, sub_remaining);
    ret = 1;
    
    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* A stop expression is just the word STOP. */
static int stop_expr(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    if(!is_nil(ic, input) &&
       name_eq(car(input), ic->n_stop)) {
        STORE(ic->g, NULL, *output, car(input));
        STORE(ic->g, NULL, *remaining, cdr(input));
        return 1;
    }
    return 0;
}

/* An IF expression consists of IF or ELSEIF followed by one or two
   sentences.
   They must be literal sentences.
 */
static int if_expr(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    sexpr *sub_output3 = NULL, *sub_remaining3 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_output3);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);
    protect_ptr(ic->g, (void **) &sub_remaining3);

    /* If the input is nil, or doesn't start with IF or ELSEIF, or if
       we can't parse a condition or a run_list (the first literal
       sentence), then we fail. */
    if(is_nil(ic, input) || 
       (!name_eq(car(input), ic->n_if) &&
        !name_eq(car(input), ic->n_ifelse)) ||
       !expr(ic, cdr(input), &sub_output, &sub_remaining) ||
       !run_list(ic, sub_remaining, &sub_output2, &sub_remaining2)) {
        ret = 0;
        goto exit;
    }

    /* Try to parse another run_list (the "else" argument). */
    if(run_list(ic, sub_remaining2, &sub_output3, &sub_remaining3)) {
        /* If we succeed, create a three argument (if) expression. */
        STORE(ic->g, NULL, *output,
                           cons(ic, ic->n_if,
                                    cons(ic, sub_output,
                                             cons(ic, sub_output2,
                                                      cons(ic, sub_output3,
                                                               ic->g_nil)))));
        STORE(ic->g, NULL, *remaining, sub_remaining3);
        ret = 1;
        goto exit;
    } else {
        /* If we fail, create a two argument (if) expression. */
        STORE(ic->g, NULL, *output,
                           cons(ic, ic->n_if,
                                    cons(ic, sub_output,
                                             cons(ic, sub_output2,
                                                      ic->g_nil))));
        STORE(ic->g, NULL, *remaining, sub_remaining2);
        ret = 1;
        goto exit;
    }


    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Parse a sentence as a list of Logo commands.
   For handling the arguments to IF/IFELSE. */
static int run_list(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(is_nil(ic, car(input))) {
        /* An empty list turns into (begin), the empty list of Lisp commands */
        STORE(ic->g, NULL, *output, cons(ic, ic->n_begin, ic->g_nil));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    if(car(input)->t == CONS) {
        /* If the first item on the input is a sentence, then try to
           parse it as an expr_list.
           Notice that we are now parsing car(input). */
        if(expr_list(ic, car(input), &sub_output, &sub_remaining)) {
            /* If we succeed, then we create (begin <parsed expr_list>)
               to return a Lisp expression for running the commands. */
            STORE(ic->g, NULL, *output,
                               cons(ic, ic->n_begin, sub_output));
            STORE(ic->g, NULL, *remaining, cdr(input));
            ret = 1;
            goto exit;
        }
    }

    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Attempt to parse/difference a sum of one or more terms. */
static int sum(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(!term(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    /* Once we've successfully parsed the first term, we keep parsing
       additional terms as long as the remaining input starts with a
       plus or a minus.
       Doing this in a loop, rather than with recursion, lets us make
       the operators associate to the right. */
    while(!is_nil(ic, sub_remaining) &&
          (name_eq(car(sub_remaining), ic->n_plus) ||
           name_eq(car(sub_remaining), ic->n_minus))) {
        if(!term(ic, cdr(sub_remaining), &sub_output2, &sub_remaining2)) {
            ret = 0;
            goto exit;
        }
        sub_output = cons(ic, car(sub_remaining),
                              cons(ic, sub_output,
                                       cons(ic, sub_output2, ic->g_nil)));
        sub_remaining = sub_remaining2;
    }

    STORE(ic->g, NULL, *output, sub_output);
    STORE(ic->g, NULL, *remaining, sub_remaining);
    ret = 1;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Parse a product/quotient of one or more factors. */
static int term(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(!factor(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    /* Once we've successfully parsed the first factor, we keep parsing
       additional factor as long as the remaining input starts with a
       multiplication (*) or division (/).
       Doing this in a loop, rather than with recursion, lets us make
       the operators associate to the right. */
    while(!is_nil(ic, sub_remaining) &&
          (name_eq(car(sub_remaining), ic->n_star) ||
           name_eq(car(sub_remaining), ic->n_slash))) {
        if(!factor(ic, cdr(sub_remaining), &sub_output2, &sub_remaining2)) {
            ret = 0;
            goto exit;
        }
        sub_output = cons(ic, car(sub_remaining),
                              cons(ic, sub_output,
                                       cons(ic, sub_output2, ic->g_nil)));
        sub_remaining = sub_remaining2;
    }

    STORE(ic->g, NULL, *output, sub_output);
    STORE(ic->g, NULL, *remaining, sub_remaining);
    ret = 1;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Parse a factor, which can come in many forms... */
static int factor(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    /* This is the easy unary minus.  If a minus comes at the beginning
       of a factor then it can't be interpreted any other way and we know
       it's a unary minus.
       The hard unary minus comes between factors and is handled in
       logoreader.c */
    if(name_eq(car(input), ic->n_minus)) {
        if(!factor(ic, cdr(input), &sub_output, &sub_remaining)) {
            ret = 0;
            goto exit;
         }
        STORE(ic->g, NULL, *output, cons(ic, ic->n_minus, cons(ic, sub_output, ic->g_nil)));
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    /* If expressions are handled here because they can output
       values. */
    if(if_expr(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    /* If any of these match, we just say that's what <factor> matched. */

    if(colon_name(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    if(quote_name(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    if(sentence(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    if(array_lit(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    if(number(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    /* An explicit procedure call is (<proc> <arg> ...) where we explicitly
       say how many arguments we are giving the procedure.
       This needs to be below the parenthesized expression case, below,
       to avoid misinterpreting expression components as a procedure name.

       Unfortunately, that means we need a few hacks in 
       <explicit_procedure_call> (later on) to avoid catching things that
       should be parenthesized expressions. */
    if(explicit_procedure_call(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }

    /* Parenthesized subexpression. */
    if(name_eq(car(input), ic->n_lparen)) {
        if(expr(ic, cdr(input), &sub_output, &sub_remaining)) {
            if(!is_nil(ic, sub_remaining) &&
               name_eq(car(sub_remaining), ic->n_rparen)) {
                STORE(ic->g, NULL, *output, sub_output);
                STORE(ic->g, NULL, *remaining, cdr(sub_remaining));
                ret = 1;
                goto exit;
            }
        }
    }

    /* Implicit procedure calls are those where we figure out how many
       arguments a procedure gets based on its arity. */
    if(implicit_procedure_call(ic, input, &sub_output, &sub_remaining)) {
        STORE(ic->g, NULL, *output, sub_output);
        STORE(ic->g, NULL, *remaining, sub_remaining);
        ret = 1;
        goto exit;
    }


    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* :FOO -> FOO
   In Lisp, FOO evaluates to the value of FOO. */
static int colon_name(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(car(input)->t == NAME &&
       car(input)->u.name.length >= 1 &&
       car(input)->u.name.name[0] == ':') {
        STORE(ic->g, NULL, *output, butfirst(ic, car(input)));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* "FOO -> (quote FOO) */
static int quote_name(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(car(input)->t == NAME &&
       car(input)->u.name.length >= 1 &&
       car(input)->u.name.name[0] == '"') {
        STORE(ic->g, NULL, *output,
                           cons(ic, ic->n_quote,
                                    cons(ic, butfirst(ic, car(input)),
                                             ic->g_nil)));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}


/* Utility procedure for matching a single name. */
static int name(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(car(input)->t == NAME) {
        STORE(ic->g, NULL, *output, car(input));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* (...) -> (quote (...)) */
static int sentence(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    int ret;

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(car(input)->t == CONS || is_nil(ic, car(input))) {
        STORE(ic->g, NULL, *output,
                           cons(ic, ic->n_quote,
                                    cons(ic, car(input),
                                             ic->g_nil)));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    ret = 0;

    exit:
    return ret;
}

/* {...} -> (quote {...}) */
static int array_lit(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    int ret;

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(car(input)->t == ARRAY) {
        STORE(ic->g, NULL, *output,
                           cons(ic, ic->n_quote,
                                    cons(ic, car(input),
                                             ic->g_nil)));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    ret = 0;

    exit:
    return ret;
}

/* Parse a number. */
static int number(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(numberp(ic, car(input))) {
        STORE(ic->g, NULL, *output, to_number(ic, car(input)));
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Explicit procedure calls are parenthesized calls where we explicitly
   state how many arguments a procedure receives.

   We need to avoid trying to parse a parenthesized subexpression. */
static int explicit_procedure_call(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    sexpr *proc, *funarg, *macro, *oper;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    if(name_eq(car(input), ic->n_lparen)) {
        /* If the token after the left parenthesis is not a name,
           then this is a parenthesized subexpression. */
        if(!name(ic, cdr(input), &sub_output, &sub_remaining)) {
            ret = 0;
            goto exit;
        }

        /* IF needs to be parsed as a subexpression, not an explicit
           call.
           Another open parenthesis means this is a subexpression.
           Either way, we fail attempting to parse it as an explicit
           procedure call. */
        if(name_eq(sub_output, ic->n_if) ||
           name_eq(sub_output, ic->n_ifelse) ||
           name_eq(sub_output, ic->n_lparen)) {
            ret = 0;
            goto exit;
        }
        /* If the name begins with a colon, we treat it as a subexpression
           and fail the attempt at recognizing this as an explicit procedure
           call. */
        if(sub_output->u.name.length > 0 &&
           sub_output->u.name.name[0] == ':') {
            ret = 0;
            goto exit;
        }

        /* Fetch the operator's information, throwing an error if we cannot. */
        if(!get_apply_parts(ic,
                            sub_output,
                            &proc,
                            &funarg,
                            &macro,
                            &oper,
                            1)) {
            throw_error(ic, ic->continuation->line);
        }

        /* Parse the arguments until the matching close parenthesis. */
        if(explicit_tail(ic, sub_remaining, &sub_output2, &sub_remaining2)) {
            STORE(ic->g, NULL, *output, cons(ic, sub_output, sub_output2));
            STORE(ic->g, NULL, *remaining, sub_remaining2);
            int argcount;
            sexpr *e;
            /* Count the number of arguments actually passed. */
            for(argcount=0, e=sub_output2; !is_nil(ic, e); e=cdr(e))
                argcount++;

            /* Throw an error if the number of arguments passed is too
               low or too high for the operator. */
            if(argcount < proc->u.proc.minargs) {
                eprintf(ic, "Not enough inputs to ");
                eprint_sexpr(ic, sub_output);
                eprintf(ic, "\n");
                throw_error(ic, ic->continuation->line);
                ret = 0; /* Never reached.  To quiet a compiler warning. */
                goto exit;
            }
            if(argcount > proc->u.proc.maxargs) {
                eprintf(ic, "Too many inputs to ");
                eprint_sexpr(ic, sub_output);
                eprintf(ic, "\n");
                throw_error(ic, ic->continuation->line);
                ret = 0; /* Never reached.  To quiet a compiler warning. */
                goto exit;
            }
            
            ret = 1;
            goto exit;
        }
    }

    ret = 0;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Parse all of the arguments in an explicit procedure call up to the
   closing parenthesis. */
static int explicit_tail(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    /* On a right parenthesis, we return the empty list. */
    if(name_eq(car(input), ic->n_rparen)) {
        STORE(ic->g, NULL, *output, ic->g_nil);
        STORE(ic->g, NULL, *remaining, cdr(input));
        ret = 1;
        goto exit;
    }

    /* Otherwise, parse an expression, then make a recursive call
       to explicit_tail, and then cons the expression onto the tail. */
    if(!expr(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    if(!explicit_tail(ic, sub_remaining, &sub_output2, &sub_remaining2)) {
        ret = 0;
        goto exit;
    }

    STORE(ic->g, NULL, *output, cons(ic, sub_output, sub_output2));
    STORE(ic->g, NULL, *remaining, sub_remaining2);
    ret = 1;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* In an implicit procedure call, one without parenthesis around it,
   the number of arguments is determined by the arity of the operator.
   This helper function is passed the number of arguments that are expected
   and attempts to parse them. */
static int implicit_procedure_call_tail(IC *ic, sexpr *input, sexpr **output, sexpr **remaining, int args_left) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    /* If we aren't supposed to parse any more, return the empty list. */
    if(args_left == 0) {
        ret = 1;
        STORE(ic->g, NULL, *output, ic->g_nil);
        STORE(ic->g, NULL, *remaining, input);
        goto exit;
    }

    /* If we're supposed to find something, and the remaining input is empty,
       then we fail. */
    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    /* Otherwise, parse an expression, parse the remaining expressions
       recursively, and cons the expression onto the list. */
    if(!expr(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    if(!implicit_procedure_call_tail(ic, sub_remaining, &sub_output2,
                                         &sub_remaining2, args_left-1)) {
        ret = 0;
        goto exit;
    }

    STORE(ic->g, NULL, *output, cons(ic, sub_output, sub_output2));
    STORE(ic->g, NULL, *remaining, sub_remaining2);
    ret = 1;

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* Handle an implicit procedure call, a call without parenthesis around it.
   The number of arguments is determined by the default arity of the operator.
 */
static int implicit_procedure_call(IC *ic, sexpr *input, sexpr **output, sexpr **remaining) {
    sexpr *sub_output = NULL, *sub_remaining = NULL;
    sexpr *sub_output2 = NULL, *sub_remaining2 = NULL;
    sexpr *proc, *funarg, *macro, *oper;
    int ret;

    protect_ptr(ic->g, (void **) &sub_output);
    protect_ptr(ic->g, (void **) &sub_output2);
    protect_ptr(ic->g, (void **) &sub_remaining);
    protect_ptr(ic->g, (void **) &sub_remaining2);

    if(is_nil(ic, input)) {
        ret = 0;
        goto exit;
    }

    /* Make sure the operator is a name. */
    if(!name(ic, input, &sub_output, &sub_remaining)) {
        ret = 0;
        goto exit;
    }

    
    /* Get the information about the procedure to call. */
    if(!get_apply_parts(ic,
                        sub_output,
                        &proc,
                        &funarg,
                        &macro,
                        &oper,
                        1)) {
        throw_error(ic, ic->continuation->line);
    }

    if(is_nil(ic, proc)) {
        eprintf(ic, "Cannot determine argument count for ");
        eprint_sexpr(ic, sub_output);
        eprintf(ic, "\n");
        throw_error(ic, ic->continuation->line);
    }

    /* Parse the arguments, and cons the operator onto the parsed
       output. */
    if(implicit_procedure_call_tail(ic, sub_remaining, &sub_output2, &sub_remaining2, proc->u.proc.defargs)) {
        STORE(ic->g, NULL, *output, cons(ic, sub_output, sub_output2));
        STORE(ic->g, NULL, *remaining, sub_remaining2);
        ret = 1;
        goto exit;
    }

    eprintf(ic, "Not enough inputs to ");
    eprint_sexpr(ic, sub_output);
    eprintf(ic, "\n");
    throw_error(ic, ic->continuation->line);
    ret = 0; /* Never reached.  To quiet a compiler warning. */

    exit:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

/* The main entry point to the treeifier.
   Passed a list of Logo commands.
   Returns a list of Lisp expressions. */
sexpr *treeify(IC *ic, sexpr *input) {
    sexpr *output = NULL, *remaining = NULL;

    if(input->t != CONS && !is_nil(ic, input))
        input = cons(ic, input, ic->g_nil);

    if(!expr_list(ic, input, &output, &remaining)) {
        return ic->g_nil;
    }

    return output;
}


