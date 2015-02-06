
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>

#include "gc.h"
#include "list_memory.h"
#include "interpreter.h"
#include "reader.h"
#include "logoreader.h"
#include "io.h"
#include "ttymodes.h"

/* Argument descriptions for getopt_long() */
char *optstring = "a:sniw:lh";
static const struct option long_options[] = {
  { "gc-allocations-per-collection", required_argument, NULL, 'a' },
  { "gc-stop-the-world",             no_argument,       NULL, 's' },
  { "no-init",                       no_argument,       NULL, 'n' },
  { "no-logo-init",                  no_argument,       NULL, 'i' },
  { "gc-work-per-allocation",        required_argument, NULL, 'w' },
  { "lisp",                          no_argument,       NULL, 'l' },
  { "help",                          no_argument,       NULL, 'h' },
  { NULL,                            0,                 NULL, 0   },
};

/* Print out the usage instructions if the program is called with
   -h or with an invalid argument. */
void print_usage(char *progname) {
    fprintf(stderr, "Usage: %s [flags] <file> ...\n", progname);
    fprintf(stderr, "Flags:\n");
    fprintf(stderr, "  -s, --gc-stop-the-world                     Run the garbage collector in\n");
    fprintf(stderr, "                                              stop the world mode\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -a, --gc-allocations-per-collection=<n>     Perform a garbage collection\n");
    fprintf(stderr, "                                              every <n> allocations when\n");
    fprintf(stderr, "                                              in stop the world mode\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -n, --no-init                               Don't read initialize.txt\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -i, --no-logo-init                          Don't read logoinitialize.txt\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -w, --gc-work-per-allocation=<n>            In incremental garbage\n");
    fprintf(stderr, "                                              collection mode, perfrom <n>\n");
    fprintf(stderr, "                                              marks or sweeps per allocation\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -l, --lisp                                  Run in Lisp (not Logo) mode\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -h, --help                                  Print this message\n");
}



/* Read and execute the contents of fp.
   It is treated as Logo code if run_logo is true,
   and as Lisp code if run_logo is false. */
void process_file(IC *ic, FILE *fp, int run_logo) {
    sexpr *expr_line, *expr, *result;

    /* "|Top Level| is the "procedure" name we use for code executed
       at the top level.  It will be associated with lines read by
       readline() when it is called directly from process_file().
       Procedure definitions are handled in initialize.txt, so
       (readline) is called from there with the procedure name while
       it is being read in. */
    sexpr *toplevel = protect(ic->g, intern(ic, "Top Level"));
    sexpr *prompt = protect(ic->g, intern(ic, "? "));
    int ch;

    /* Set our input to be from the file we were passed. */
    read_from_file(ic, fp);
    logoread_from_file(ic->lr, fp);

    /* If we are not reading from a tty, and if the first character is a '#',
       then we skip the whole first line so that #!/usr/local/bin/clogo
       can work in a script. */
    if(!isatty(fileno(fp))) {
        ch = getc(fp);
        if(ch == '#') {
            do {
                ch = getc(fp);
            } while(ch != '\n');
        } else {
            ungetc(ch, fp);
        }
    }

    for(;;) {
        /* Reset our source to the input file.  We do this every time
           through the loop because we may be reading from elsewhere
           while executing the code. */
        read_from_file(ic, fp);
        logoread_from_file(ic->lr, fp);


        if(run_logo) {
            /* This setjmp, and the associated protection count
               resets, are in case the user presses Control-C while
               typing in a line at the top level. */
            int stored_protect_count = ic->g->protect_count;
            int stored_protect_ptr_count = ic->g->protect_ptr_count;
            if(setjmp(ic->abort)) {
                ic->g->protect_count = stored_protect_count;
                ic->g->protect_ptr_count = stored_protect_ptr_count;
                expr = result = protect(ic->g, ic->g_unbound);
            } else {
                expr_line = readline(ic->lr, toplevel, prompt);

                /* If the user pressed Control-\ run PAUSE */
                if(expr_line == ic->eof) {
                    if(!paused) {
                        break;
                    } else {
                        paused = 0;
                        expr_line = 
                            mk_line(ic, ic->n_pause,
                                        cons(ic, ic->n_pause, ic->g_nil),
                                        toplevel);
                    }
                }

                /* Protect the line we read while we allocate the
                   expression to evaluate. */
                protect(ic->g, expr_line);

                /* Pull out the parsed version of the line. */
                expr = expr_line->u.line.parsed_line;

                /* If we are defining a procedure, transform
                   [to <proc> <arg> ...]
                   into
                   (begin <expr_line>
                          (create_logo_procedure (quote <expr_line>)))

                   The first <expr_line> sets ic->current_line for error
                   reporting.

                   The expression is allowed to STOP or return without
                   outputing a value.
                 */
                if(!is_nil(ic, expr) && name_eq(car(expr), ic->n_to)) {
                    result = eval(ic,
                      cons(ic, ic->n_begin,
                               cons(ic, expr_line,
                                        cons(ic, cons(ic, ic->n_create_logo_procedure,
                                                          cons(ic, cons(ic, ic->n_quote,
                                                                            cons(ic, expr_line, ic->g_nil)),
                                                                    ic->g_nil)),
                                                  ic->g_nil))),
                      STOP_OK | NO_VALUE_OK);
                } else if(!is_nil(ic, expr) &&
                          name_eq(car(expr), ic->n_dot_macro)) {
                    /* Same as above, but for a macro.  Transform
                       [.macro <proc> <arg> ...]
                       into
                       (begin <expr_line>
                              (create_logo_procedure (quote <expr_line>)))
                     */
                    result = eval(ic,
                      cons(ic, ic->n_begin,
                               cons(ic, expr_line,
                                        cons(ic, cons(ic, ic->n_create_logo_macro,
                                                          cons(ic, cons(ic, ic->n_quote,
                                                                            cons(ic, expr_line, ic->g_nil)),
                                                                   ic->g_nil)),
                                                  ic->g_nil))),
                      STOP_OK | NO_VALUE_OK);
                } else {
                    /* Otherwise, we just evaluate the line.
                       This takes two calls to eval().  One to treeify
                       the line (turn it into Lisp), and one to run the
                       treeified line.

                       The First we evaluate:
                           (begin <expr_line>
                                  (treeify (quote <expr>)))

                       Where <expr_line> is the LINE object, and <expr>
                       is the parsed line from <expr_line>.

                       <expr_line> sets ic->current_line for error messages.

                       (treeify (quote <expr>)) generates and returns the
                       treeified version.

                       This expression must return a value.
                     */
                    sexpr *treeified = eval(ic,
                      cons(ic, ic->n_begin,
                               cons(ic, expr_line,
                                        cons(ic, cons(ic, ic->n_treeify,
                                                          cons(ic, cons(ic, ic->n_quote,
                                                                            cons(ic, expr, ic->g_nil)),
                                                                   ic->g_nil)),
                                                 ic->g_nil))),
                      OUTPUT_OK | VALUE_OK);
                    if(treeified->t != CONS) {
                        /* If the treeified version doesn't contain anything,
                           then don't bother running it.  Just spin through
                           the loop again. */
                        result = ic->g_unbound;
                    } else {
                        /* We have a treeified expression to run.
                           Set the input to come from the current input
                           rather than the file we are processing. */
                        read_from_file(ic, ic->input->u.filep.file);
                        logoread_from_file(ic->lr, ic->input->u.filep.file);

                        /* Protect the treeified version from garbage
                           collection while we allocate the expression
                           to evaluate and evaluate it.

                           The expression we evaluate is:
                             (eval <expr_line> . treeified)
                         */
                        protect(ic->g, treeified);
                        result = eval(ic,
                          cons(ic, ic->n_begin,
                                   cons(ic, expr_line, treeified)),
                          STOP_OK | NO_VALUE_OK);
                        unprotect(ic->g);
                    }
                }
            }
        } else {
            /* We are evaluating Lisp code.
               Read an object and eval() it. */
            expr = readobj(ic);
            if(expr == ic->eof)
                break;
            protect(ic->g, expr);
            result = eval(ic, expr, STOP_OK | NO_VALUE_OK);
        }

        /* We run statements, so they should not return values at the top
           level. */
        if(result != ic->g_unbound) {
            protect(ic->g, result);
            tprintf(ic, "You don't say what to do with ");
            tprint_sexpr(ic, result);
            tprintf(ic, " in ");
            tprint_sexpr(ic, expr);
            tprintf(ic, "\n");
            unprotect(ic->g);
        }
        unprotect(ic->g);
    }
    unprotect(ic->g);
    unprotect(ic->g);
}


/* Signal handlers.  It's not safe to do much more than set a flag,
   so that's all we do, and then we check for the flag within the
   reader and the interpreter. */
static void sigint_handler(int signal, siginfo_t *si, void *ctx) {
   interrupted = 1; 
}

static void sigquit_handler(int signal, siginfo_t *si, void *ctx) {
   paused = 1; 
}


int main(int argc, char *argv[], char *envp[]) {
    IC *ic;
    FILE *initf;
    int opt, gc_delay = 10000;
    int read_initialization_file = 1;
    int read_logo_initialization_file = 1;
    int use_incremental_collection = 1;
    int run_logo = 1;
    int work_per_allocation = 3;
    struct sigaction sigint_action, sigquit_action;


    /* Set up the Control-C and Control-\ signal handlers. */
    sigint_action.sa_sigaction = sigint_handler;
    sigemptyset(&sigint_action.sa_mask);
    sigint_action.sa_flags = SA_SIGINFO;
    
    if(sigaction(SIGINT, &sigint_action, NULL) < 0) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    sigquit_action.sa_sigaction = sigquit_handler;
    sigemptyset(&sigquit_action.sa_mask);
    sigquit_action.sa_flags = SA_SIGINFO;
    
    if(sigaction(SIGQUIT, &sigquit_action, NULL) < 0) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    /* Initialize the cached termios structures for cbreak and cooked
       modes, and the flag for keeping track of what mode we are in. */
    if(tty_init(STDIN_FILENO) < 0) {
        perror("tty_init");
        exit(EXIT_FAILURE);
    } 

    while ((opt = getopt_long(argc, argv, optstring, long_options, NULL)) !=
           -1) {
        switch (opt) {
            case 'a':
                gc_delay = atoi(optarg);
                break;
            case 'n':
                read_initialization_file = 0;
                break;
            case 'i':
                read_logo_initialization_file = 0;
                break;
            case 's':
                use_incremental_collection = 0;
                break;
            case 'w':
                work_per_allocation = atoi(optarg);
                break;
            case 'l':
                run_logo = 0;
                break;
            case 'h':
                print_usage(argv[0]);
                exit(EXIT_SUCCESS);

            default: /* '?' */
                print_usage(argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    ic = mk_interpreter(gc_delay);
    if(use_incremental_collection) {
        set_gc_mode(ic->g, INCREMENTAL);
        set_gc_work_per_alloc(ic->g, work_per_allocation);
    }

    if(read_initialization_file) {
        if((initf = fopen(LIBRARYDIR "/initialize.txt", "r")) != NULL) {
            process_file(ic, initf, 0);
            fclose(initf);
        }
    }
    if(read_logo_initialization_file) {
        if((initf = fopen(LIBRARYDIR "/ucblogolib.txt", "r")) != NULL) {
            process_file(ic, initf, 1);
            fclose(initf);
        }
        if((initf = fopen(LIBRARYDIR "/logoinitialize.txt", "r")) != NULL) {
            process_file(ic, initf, 1);
            fclose(initf);
        }
    }

    if(optind < argc) {
        while(optind < argc) {
            if(!strcmp(argv[optind], "-")) {
                process_file(ic, stdin, run_logo);
            } else {
                FILE *fp = fopen(argv[optind], "r");
                if(fp == NULL) {
                    fprintf(stderr, "Error opening %s\n", argv[optind]);
                    exit(EXIT_FAILURE);
                }
                process_file(ic, fp, run_logo);
                fclose(fp);
            }
            optind++;
        }
    }

    process_file(ic, stdin, run_logo);

    /* Peek into the garbage collector protection counters to make sure
       that we at least made the same number of protect/protect_ptr and
       unprotect/unprotect_ptr calls during the program run. */
    if(ic->g->protect_count != 0) {
        fprintf(stderr,
                "Error: ic->g->protected_count == %d\n",
                ic->g->protect_count);
    }
    if(ic->g->protect_ptr_count != 0) {
        fprintf(stderr,
                "Error: ic->g->protected_ptr_count == %d\n",
                ic->g->protect_ptr_count);
    }
    free_interpreter(ic);
    return 0;
}
