
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

#include <wx/wxprec.h>
#ifndef WX_PRECOMP
    #include <wx/wx.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>

#include "pcgc.h"
#include "list_memory.h"
#include "interpreter.h"
#include "reader.h"
#include "logoreader.h"
#include "io.h"
#include "ttymodes.h"
#include "main.h"

#include <pthread.h>
#define GC_THREADS
#include <gc.h>

/* Argument descriptions for getopt_long() */
const char *optstring = "a:sniw:lth";
/* const char *optstring = "a:sniw:lthCc"; */
static const struct option long_options[] = {
  { "gc-allocations-per-collection", required_argument, NULL, 'a' },
  { "gc-stop-the-world",             no_argument,       NULL, 's' },
  { "no-init",                       no_argument,       NULL, 'n' },
  { "no-logo-init",                  no_argument,       NULL, 'i' },
  { "gc-work-per-allocation",        required_argument, NULL, 'w' },
#if 0
  { "gc-conservative",               no_argument,       NULL, 'C' },
  { "gc-copying",                    no_argument,       NULL, 'c' },
#endif
  { "lisp",                          no_argument,       NULL, 'l' },
  { "terminal",                      no_argument,       NULL, 't' },
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
#if 0
    fprintf(stderr, "  -C, --gc-conservative                       Run the conservative garbage\n");
    fprintf(stderr, "                                              collector (DANGER!)\n");
    fprintf(stderr, "  -c, --gc-copying                            Use copying collector\n");
    fprintf(stderr, "\n");
#endif
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
    fprintf(stderr, "                                              (Only works in terminal mode)\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "  -t, --terminal                              Run in terminal mode (no GUI)\n");
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
       the procedure body is being read in. */
    sexpr *toplevel = NULL, *prompt = NULL;
    int ch;

    toplevel = intern(ic, "Top Level");
    protect_ptr(ic->g, (void **) &toplevel);
    prompt = intern(ic, "? ");
    protect_ptr(ic->g, (void **) &prompt);

    /* Set our input to be from the file we were passed. */
    read_from_file(ic, fp);
    logoread_from_file(ic->lr, fp);

    /* If we are not reading from a tty, and if the first character is a '#',
       then we skip the whole first line so that #!/usr/local/bin/clogo
       can work in a script. */
    if(fp != NULL && !isatty(fileno(fp))) {
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
            int stored_protect_ptr_count = ic->g->protect_ptr_count;
            if(setjmp(ic->abort)) {
                ic->g->protect_ptr_count = stored_protect_ptr_count;
                expr = result = ic->g_unbound;
                protect_ptr(ic->g, (void **) &expr);
            } else {
                expr_line = readline(ic->lr, toplevel, prompt);

                /* If the user pressed Control-\ run PAUSE */
                if(expr_line == ic->eof) {
                    signalLocker.Lock();
                    if(!paused) {
                        signalLocker.Unlock();
                        break;
                    } else {
                        paused = 0;
                        expr_line = 
                            mk_line(ic, ic->n_pause,
                                        cons(ic, ic->n_pause, ic->g_nil),
                                        toplevel);
                    }
                    signalLocker.Unlock();
                }

                /* Protect the line we read while we allocate the
                   expression to evaluate. */
                protect_ptr(ic->g, (void **) &expr_line);

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
                             (begin <expr_line> . treeified)
                         */
                        protect_ptr(ic->g, (void **) &treeified);
                        result = eval(ic,
                          cons(ic, ic->n_begin,
                                   cons(ic, expr_line, treeified)),
                          STOP_OK | NO_VALUE_OK);
                        unprotect_ptr(ic->g);
                    }
                }
            }
        } else {
            /* We are evaluating Lisp code.
               Read an object and eval() it. */
            expr = readobj(ic);
            if(expr == ic->eof)
                break;
            protect_ptr(ic->g, (void **) &expr);
            result = eval(ic, expr, STOP_OK | NO_VALUE_OK);
        }

        /* We run statements, so they should not return values at the top
           level. */
        if(result != ic->g_unbound) {
            protect_ptr(ic->g, (void **) &result);
            tprintf(ic, "You don't say what to do with ");
            tprint_sexpr(ic, result);
            tprintf(ic, " in ");
            tprint_sexpr(ic, expr);
            tprintf(ic, "\n");
            unprotect_ptr(ic->g);
        }
        unprotect_ptr(ic->g);
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
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

/* Global variables used for communication between main() and
   start_logo().  start_logo() is called directly by main() in
   terminal mode, but is run by an interpreter thread when
   running in GUI mode. */

static IC *ic;
static int gc_delay = 10000;
static int read_initialization_file = 1;
static int read_logo_initialization_file = 1;
static int use_incremental_collection = 1;
static int run_logo = 1;
static int work_per_allocation = 3;
static int logo_argc;
static char **logo_argv;

int main(int argc, char *argv[], char *envp[]) {
    int opt;
    struct sigaction sigint_action, sigquit_action;
    int terminal_mode = 0;
#if 0
    int use_conservative_collection = 0;
    int use_copying_collection = 0;
#endif

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
#if 0
            case 'c':
                use_copying_collection = 1;
                break;
            case 'C':
                use_conservative_collection = 1;
                break;
#endif
            case 'w':
                work_per_allocation = atoi(optarg);
                break;
            case 'l':
                run_logo = 0;
                break;
            case 't':
                terminal_mode = 1;
                break;
            case 'h':
                print_usage(argv[0]);
                return 0;

            default: /* '?' */
                print_usage(argv[0]);
                return EXIT_FAILURE;
        }
    }

    logo_argc = argc;
    logo_argv = argv;

#if 0
    if(use_copying_collection)
        ic = mk_interpreter(gc_delay, create_copying_gc, terminal_mode);
    else if(use_conservative_collection)
        ic = mk_interpreter(gc_delay, create_conservative_gc, terminal_mode);
    else
#endif
        ic = mk_interpreter(gc_delay, create_ms_gc, terminal_mode);

    if(ic == NULL)
        return EXIT_FAILURE;

    if(use_incremental_collection) {
        set_gc_mode(ic->g, INCREMENTAL);
        set_gc_work_per_alloc(ic->g, work_per_allocation);
    }

    if(terminal_mode) {
        /* Set up the Control-C and Control-\ signal handlers. */
        sigint_action.sa_sigaction = sigint_handler;
        sigemptyset(&sigint_action.sa_mask);
        sigint_action.sa_flags = SA_SIGINFO;
    
        if(sigaction(SIGINT, &sigint_action, NULL) < 0) {
            perror("sigaction");
            return EXIT_FAILURE;
        }

        sigquit_action.sa_sigaction = sigquit_handler;
        sigemptyset(&sigquit_action.sa_mask);
        sigquit_action.sa_flags = SA_SIGINFO;
    
        if(sigaction(SIGQUIT, &sigquit_action, NULL) < 0) {
            perror("sigaction");
            return EXIT_FAILURE;
        }

        /* Initialize the cached termios structures for cbreak and cooked
           modes, and the flag for keeping track of what mode we are in. */
        if(tty_init(STDIN_FILENO) < 0) {
            perror("tty_init");
            return EXIT_FAILURE;
        } 

        /* Let logo code test :TERMINALP to see if we're in terminal
           mode. */
        ic->n_terminalp->u.name.symbol->value = ic->n_true;

        /* Choose the terminal mode IO procedures. */
        ic->output_printer = &terminal_vprintf;
        ic->read_from_user = &read_from_user_terminal;
        ic->logoread_from_user = &logoread_from_user_terminal;
        ic->linemode = &linemode_terminal;
        ic->charmode_blocking = &charmode_blocking_terminal;
        ic->charmode_nonblocking = &charmode_nonblocking_terminal;
        ic->maybe_prompt = &maybe_prompt_terminal;

        /* In terminal mode, we just run the interpreter directly
           and terminate when it finishes. */
        start_logo();
        return 0;
    } else {
        /* Set up :TERMINALP and IO procedures for wxWidgets GUI mode. */
        ic->n_terminalp->u.name.symbol->value = ic->n_false;
        ic->output_printer = &wx_vprintf;
        ic->read_from_user = &read_from_user_terminal;
        ic->logoread_from_user = &logoread_from_user_wx;
        ic->linemode = &linemode_wx;
        ic->charmode_blocking = &charmode_blocking_wx;
        ic->charmode_nonblocking = &charmode_nonblocking_wx;
        ic->maybe_prompt = &maybe_prompt_wx;

        /* wxEntry creates an instance of MyApp (see wxui.cpp), calls
           OnInit, and enters an event loop.  When the event loop
           terminates we return here and quit the program. */
        return wxEntry(argc, argv);
    }
}

void wxstart_logo() {
    ic->g->thread_start();

    /* We need to do this because the interpreter context is gone when
       start_logo() finishes. */
    void (*thread_end)();
    thread_end = ic->g->thread_end;
    start_logo();
    thread_end();

    /* If the GUI thread hasn't asked us to die, then we need to ask
       it to close the application. */
    if(!wxThread::This()->TestDestroy())
        wxTheApp->QueueEvent(new wxCloseEvent(wxEVT_CLOSE_WINDOW));
}

void start_logo() {
    int stored_protect_ptr_count = ic->g->protect_ptr_count;
    FILE *initf;
    ic->linemode(ic->lr);

    if(setjmp(ic->quit)) {
        /* BYE jumps here to quit the interpreter.
           In GUI mode, when the GUI thread wants the interpreter
           to quit, TestDestroy() returns true and eval()
           jumps here to quit. */
        ic->g->protect_ptr_count = stored_protect_ptr_count;
    } else {
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

        if(optind < logo_argc) {
            while(optind < logo_argc) {
                if(!strcmp(logo_argv[optind], "-")) {
                    process_file(ic, NULL, run_logo);
                } else {
                    FILE *fp = fopen(logo_argv[optind], "r");
                    if(fp == NULL) {
                        fprintf(stderr, "Error opening %s\n", logo_argv[optind]);
                        longjmp(ic->quit, 1);
                    }
                    process_file(ic, fp, run_logo);
                    fclose(fp);
                }
                optind++;
            }
        }

        process_file(ic, NULL, run_logo);
    }

    /* Peek into the garbage collector protection counters to make sure
       that we at least made the same number of protect/protect_ptr and
       unprotect/unprotect_ptr calls during the program run. */
    if(ic->g->protect_ptr_count != 0) {
        fprintf(stderr,
                "Error: ic->g->protected_ptr_count == %d\n",
                ic->g->protect_ptr_count);
    }
    free_interpreter(ic);
    ic = NULL;
}
