
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
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "pcgc.h"
#include "list_memory.h"
#include "logoreader.h"
#include "io.h"
#include "ttymodes.h"
#include "global_environment.h"
#include "wxui.h"

static int fetch_char_from_file(logoreader *r);
static int fetch_char_from_wx(logoreader *r);

/* Are we reading from the user? */
static int reading_from_user(logoreader *lr) {
    if( (lr->char_reader == fetch_char_from_file &&
         lr->ifp == stdin &&
         isatty(fileno(stdin))) ||
        lr->char_reader == fetch_char_from_wx)
        return 1;
    else
        return 0;
}

void linemode_terminal(logoreader *lr) {
  if(reading_from_user(lr)) {
      if(tty_blocking(fileno(lr->ifp)) < 0) {
         eprintf(lr->ic, "Error setting terminal to blocking mode");
         throw_error(lr->ic, lr->ic->continuation->line);
      }
      if(tty_cooked(fileno(stdin)) < 0) {
          eprintf(lr->ic, "Error setting terminal back to cooked mode");
          throw_error(lr->ic, lr->ic->continuation->line);
      }
      lr->blocking = 1;
    }
}

void charmode_blocking_terminal(logoreader *lr) {
    if(reading_from_user(lr)) {
        if(tty_blocking(fileno(lr->ifp)) < 0) {
           eprintf(lr->ic, "Error setting terminal to blocking mode");
           throw_error(lr->ic, lr->ic->continuation->line);
        }
        if(tty_cbreak(fileno(stdin)) < 0) {
            eprintf(lr->ic, "Error setting terminal to cbreak mode");
            throw_error(lr->ic, lr->ic->continuation->line);
        }
        lr->blocking = 1;
    }
}

void charmode_nonblocking_terminal(logoreader *lr) {
    if(reading_from_user(lr)) {
        if(tty_nonblocking(fileno(lr->ifp)) < 0) {
           eprintf(lr->ic, "Error setting terminal to nonblocking mode");
           throw_error(lr->ic, lr->ic->continuation->line);
        }
        if(tty_cbreak(fileno(stdin)) < 0) {
            eprintf(lr->ic, "Error setting terminal to cbreak mode");
            throw_error(lr->ic, lr->ic->continuation->line);
        }
        lr->blocking = 0;
    }
}

void logoread_from_user_terminal(logoreader *r) {
    logoread_from_file(r, stdin);
}

void linemode_wx(logoreader *lr) {
    wxCommandEvent *modeE = new wxCommandEvent(LINE_MODE);
    wxTheApp->QueueEvent(modeE);
    lr->blocking = 1;
}

void charmode_blocking_wx(logoreader *lr) {
    wxCommandEvent *modeE = new wxCommandEvent(CHAR_MODE);
    wxTheApp->QueueEvent(modeE);
    lr->blocking = 1;
}

void charmode_nonblocking_wx(logoreader *lr) {
    wxCommandEvent *modeE = new wxCommandEvent(CHAR_MODE);
    wxTheApp->QueueEvent(modeE);
    lr->blocking = 0;
}

static int fetch_char_from_wx(logoreader *r) {
    int ret = 0;

    if(r->char_la_valid) {
        r->char_la_valid = 0;
        return r->char_la;
    } else if(r->source_string == NULL ||
              r->source_string[r->source_position] == '\0') {
        wxString s;
        wxMessageQueueError res;
        if(r->blocking) {
            do {
                res = wxGetApp().GetStringQueue()->ReceiveTimeout(100, s);
                if(!r->ic->terminal_mode && wxThread::This()->TestDestroy())
                    longjmp(r->ic->quit, 1);
                signalLocker.Lock();
                if(interrupted) {
                    /* If the user pressed Ctrl-C, we need to throw an error.
                    We throw a fatal error so the program cannot catch it and
                    the user can always abort the program. */
                    interrupted = 0;
                    eprintf(r->ic, "User interrupt");
                    r->ic->linemode(r);
                    signalLocker.Unlock();
                    fatal_error(r->ic, NULL);
                }
                if(paused) {
                    res = wxMSGQUEUE_TIMEOUT;
                    signalLocker.Unlock();
                    break;
                }
                signalLocker.Unlock();
            } while(res == wxMSGQUEUE_TIMEOUT);
        } else {
            res = wxGetApp().GetStringQueue()->ReceiveTimeout(0, s);
        }

        if(!r->ic->terminal_mode && wxThread::This()->TestDestroy())
            longjmp(r->ic->quit, 1);

        if(res == wxMSGQUEUE_TIMEOUT) {
            ret = EOF;
        } else if(res == wxMSGQUEUE_NO_ERROR) {
            const char *cp = s.c_str();
            STORE(r->ic->g,
                  r,
                  r->source_string,
                  (char *) ic_xmalloc(r->ic,
                                      strlen(cp) + 1,
                                      mark_cstring));
            strcpy(r->source_string, cp);
            r->source_position = 0;
            /* This assumes that source_string will never be
               empty.  We know this because wxui.cpp always
               adds a newline before sending us a string.
               This is sloppy but it works. */
            ret = r->source_string[r->source_position++];
        }
    } else {
        ret = r->source_string[r->source_position++];
    }

    /* Characters read from the terminal must be copied to the dribble
       file. */
    if(ret != EOF &&
       reading_from_user(r) &&
       !is_nil(r->ic, r->ic->dribble_name)) {
        fprintf(r->ic->dribble->u.filep.file, "%c", ret);
    }

    /* If we're logging the raw line, then we need to save the read
       character to the raw_line byte_buffer.
       This is used when defining Logo procedures. */
    if(r->logging_raw_line)
        add_to_byte_buffer(r->ic, r->raw_line, ret);

    return ret;
}

void logoread_from_user_wx(logoreader *lr) {
    lr->source_string = NULL;
    lr->source_position = 0;
    lr->char_reader = fetch_char_from_wx;
    lr->char_la_valid = 0;
}

/* This is the function for reading a character when the source is
   a file. */
static int fetch_char_from_file(logoreader *r) {
    int ret;

    if(r->char_la_valid) {
        /* If we have a lookahead, make the lookahead invalid and
           return the cached character. */
        r->char_la_valid = 0;
        ret = r->char_la;
    } else {
        ret = getc(r->ifp);
        signalLocker.Lock();
        if(interrupted) {
            /* If the user pressed Ctrl-C, we need to throw an error.
               We throw a fatal error so the program cannot catch it and
               the user can always abort the program. */
            interrupted = 0;
            eprintf(r->ic, "User interrupt");
            r->ic->linemode(r);
            signalLocker.Unlock();
            fatal_error(r->ic, NULL);
        }
        signalLocker.Unlock();

        /* Characters read from the terminal must be copied to the dribble
           file. */
        if(ret != EOF &&
           reading_from_user(r) &&
           !is_nil(r->ic, r->ic->dribble_name)) {
            fprintf(r->ic->dribble->u.filep.file, "%c", ret);
        }

        /* If we're logging the raw line, then we need to save the read
           character to the raw_line byte_buffer.
           This is used when defining Logo procedures. */
        if(r->logging_raw_line)
            add_to_byte_buffer(r->ic, r->raw_line, ret);
    }
    return ret;
}

/* Set the input to come from a file. */
void logoread_from_file(logoreader *lr, FILE *fp) {
    if(fp == NULL) {
        lr->ic->logoread_from_user(lr);
        return;
    }
    lr->ifp = fp;
    lr->char_reader = fetch_char_from_file;
    lr->char_la_valid = 0;
}


/* This is the function for reading from a string. */
static int fetch_char_from_string(logoreader *r) {
    int ret;

    if(r->char_la_valid) {
        r->char_la_valid = 0;
        ret = r->char_la;
    } else if(r->source_string[r->source_position] == '\0') {
        return EOF;
    } else {
        ret = r->source_string[r->source_position++];
        if(r->logging_raw_line)
            add_to_byte_buffer(r->ic, r->raw_line, ret);
    }
    return ret;
}

/* Set the input to come from a string. */
void logoread_from_string(logoreader *lr, char *source) {
    lr->source_string = source;
    lr->source_position = 0;
    lr->char_reader = fetch_char_from_string;
    lr->char_la_valid = 0;
}

/* Put back a character.  Store it in the lookahead and mark the
   lookahead as valid. */
static void put_back_char(logoreader *r, int ch) {
    r->char_la = ch;
    r->char_la_valid = 1;
}

/* Used by gettoken() to add a character to the token being created. */
static void add_to_buf(logoreader *r, char ch) {
    add_to_byte_buffer(r->ic, r->bb, ch);
}

/* Put back a token.  Really, just mark the current token as a
   valid lookahead.  Only works when putting back the token just
   read. */
static void putback_token(logoreader *r, sexpr *token) {
    r->token_la_valid = 1;
}

/* Print a prompt if reading from user. */
void maybe_prompt_terminal(logoreader *lr, const char *prompt) {
    if(reading_from_user(lr)) {
        tprintf(lr->ic, "%s", prompt);
    }
}

void maybe_prompt_wx(logoreader *lr, const char *prompt) {
    if(reading_from_user(lr)) {
        wxCommandEvent *setprompte = new wxCommandEvent(SET_PROMPT);
        setprompte->SetString(wxString(prompt));
        wxTheApp->QueueEvent(setprompte);
    }
}


/* Read a token. */
static sexpr *gettoken(logoreader *r) {
  int ch;
  const char *token_terminators; /* List of non-blanks that will terminate
                                    the current token. */

  /* Terminators for tokens that begin with a double quote. */
  static const char *quoted_terminators = "()[]{}";

  /* Terminators for tokens that do not begin with a double quote. */
  static const char *non_quoted_terminators = "()[]{}+-*/=<>;";

  /* If there's a valid lookahead, use it. */
  if(r->token_la_valid) {
    r->token_la_valid = 0;
    return word_from_byte_buffer(r->ic, r->bb);
  }

  /* Before reading a token, there are no escapes. */
  r->token_escaped = 0;

  /* Empty the buffer. */
  clear_byte_buffer(r->bb);

  /* Prime the pump. */
  ch = r->char_reader(r);

  /* Skip whitespaces and comments. */
  for(;;) {
    if(ch == EOF) {
        return NULL;
    } else if(ch == ';') {
      /* We have a comment.  Skip the rest of the line.
         Lines with comments can be continued, but the continuation
         is not treated as a comment.  For example:
             ? print 1 + ; comment ~  
             ~ 2
             3
             ? 
        */
      do {
        ch = r->char_reader(r);
      } while(ch != EOF && ch != '\n' && ch != '~');
    } else if(ch == '~') {
        /* A ~ at the end of the line causes the next to be read and appended
           to this one. */
        int ch2 = r->char_reader(r);
        if(ch2 == EOF) return NULL;
        if(ch2 == '\n') {
            r->ic->maybe_prompt(r, "~ ");
            ch = r->char_reader(r);
        } else {
            put_back_char(r, ch2);
            break;
        }
    } else if(isspace(ch) && ch != '\n') {
        ch = r->char_reader(r);
    } else {
        break;
    }
  }

  if(strchr("()[]{}+-*/=<>;\n", ch)) {
    /* We're dealing with a special character token.
       Either we return just the character, or possibly two characters
       for "<=", ">=", or "<>". */

    add_to_buf(r, ch);
    if(strchr("<>", ch)) {
      /* If we have a "<" or a ">", then add the following character if
         we have "<=", ">=", or "<>". */
      int ch2 = r->char_reader(r);
      /* If it's an EOF, just put it back for later processing. */
      if(ch2 == '=' || (ch == '<' && ch2 == '>'))
          add_to_buf(r, ch2);
      else
          put_back_char(r, ch2);
    }

    if(ch == '-' && r->last_break == BROKE_ON_SPACE) {
        int ch2 = r->char_reader(r);
        /* Need to figure out whether this is a unary or binary minus.
           It is a unary minus if we last broke on a space and the next
           character is not a space. */
        /* If it's an EOF, just put it back for later processing. */
        if(isdigit(ch2) || ch2 == '.') {
            ch = ch2;
            goto skip_specials;
        } else if(isspace(ch2)) {
            /* Space before and after - normal binary minus.
               Do nothing. */
        } else {
            /* Non-space after.  We have a unary minus.
               Add a second dash. */
            add_to_buf(r, ch);
        }
        put_back_char(r, ch2);
    }

    /* Check to see if the next character is a space, for future unary
       minuses. */
    if(ch == '\n') {
        r->last_break = BROKE_ON_SPACE;
    } else if(strchr("([{", ch)) {
        /* Pretend we broke on a space when starting an enclosed
           expression. */
        r->last_break = BROKE_ON_SPACE;
    } else {
        int ch2 = r->char_reader(r);
        /* Don't worry about EOF.  Just put it back for later. */
        if(isspace(ch2))
            r->last_break = BROKE_ON_SPACE;
        else
            r->last_break = BROKE_ON_NONSPACE;
        put_back_char(r, ch2);
    }

    return word_from_byte_buffer(r->ic, r->bb);
  }
  skip_specials:

  /* Tokens that begin with a quote will only break on spaces and
     parenthesis/brackets/braces.  This lets us enter things like "*
     Other tokens also break on any operator. */
  if(ch == '\"')
    token_terminators = quoted_terminators;
  else
    token_terminators = non_quoted_terminators;

  for(;;) {
    if(strchr(token_terminators, ch) || isspace(ch) || ch == EOF) {
        /* We found the end of the token.  Mark whether we ended the
           token on a space or a nonspace and return the token. */
        if(isspace(ch))
            r->last_break = BROKE_ON_SPACE;
        else
            r->last_break = BROKE_ON_NONSPACE;
        put_back_char(r, ch);
        return word_from_byte_buffer(r->ic, r->bb);
    } else if(ch == '|') {
        /* There has been an escape.  Used to distinguish |[| from [ */
        r->token_escaped = 1;
        /* Need to read characters until the next unescaped | */
        ch = r->char_reader(r);
        for(;;) {
            /* Prompt on newline, but include it in the token. */
            if(ch == '\n')
                r->ic->maybe_prompt(r, "| ");

            if(ch == EOF) {
                break;
            } else if(ch == '\\') {
                /* Include the following character no matter what it is.
                   Unless it's an EOF.
                   Prompt if it was a newline. */
                ch = r->char_reader(r);
                if(ch == '\n')
                    r->ic->maybe_prompt(r, "\\ ");

                if(ch == EOF) {
                    /* Do nothing. */
                } else {
                    add_to_buf(r, ch);
                    ch = r->char_reader(r);
                }
            } else if(ch == '|') {
                /* We're done reading the characters between the pipes.
                   We go back to reading characters normally. */
                ch = r->char_reader(r);
                break;
            } else {
                /* Anything else we add as is. */
                add_to_buf(r, ch);
                ch = r->char_reader(r);
            }
        }
    } else if(ch == '\\') {
        /* There has been an escape.  Used to distinguish |[| from [ */
        r->token_escaped = 1;
        /* Include the next character as is. Prompt if it's a newline. */
        ch = r->char_reader(r);
        if(ch == '\n')
            r->ic->maybe_prompt(r, "\\ ");

        if(ch == EOF) {
            /* Do nothing. */
        } else {
            add_to_buf(r, ch);
            ch = r->char_reader(r);
        }
    } else {
        /* Default action.  Add it to the buffer, and begin to process
           the next character. */
        add_to_buf(r, ch);
        ch = r->char_reader(r);
    }
  }
}


static sexpr *logoreadlist(logoreader *lr, sexpr *ending_token);
static sexpr *logoreadarray(logoreader *lr);

/* This is the main procedure to read a single Logo object. */
sexpr *logoreadobj(logoreader *lr) {
  sexpr *token;

  /* These are used by strtod().  Number is the return value, and
     unparsed is set to the portion of the string not parsed.
     If it is not pointing at the terminating '\0' when we are done, then
     we failed to get a number. */
  double number;
  char *unparsed, *possible_number;

  token = gettoken(lr);

  if(token == NULL) return lr->ic->eof;
  if(name_eq(token, lr->ic->n_newline)) return lr->ic->eof;

  /* Check for sentences or arrays. */
  if(name_eq(token, lr->ic->n_lbracket) && !lr->token_escaped)
    return logoreadlist(lr, lr->ic->n_rbracket);
  if(name_eq(token, lr->ic->n_lbrace) && !lr->token_escaped)
    return logoreadarray(lr);

  /* Check to see if it's a valid number. */
  possible_number = get_cstring(lr->ic, token);
  if(strcasecmp(possible_number, "inf") &&
     strcasecmp(possible_number, "infinity") &&
     strcasecmp(possible_number, "nan")) {
    number = strtod(possible_number, &unparsed);
    if(unparsed != possible_number && *unparsed == '\0')
      return mk_number(lr->ic, number);
  }

  /* Return as is. */
  return token;
}

/* To read an array, we read a '}' terminated list and convert it
   into an array. */
static sexpr *logoreadarray(logoreader *lr) {
  sexpr *item_list = logoreadlist(lr, lr->ic->n_rbrace);
  int origin = 1;

  /* Look at the next token for @<origin> */
  sexpr *token = gettoken(lr);
  if(token->u.name.length >= 2 &&
     token->u.name.head[token->u.name.start] == '@') {
      origin = atoi(get_cstring(lr->ic, butfirst(lr->ic, token)));
  } else {
      putback_token(lr, token);
  }
  return list_to_array(lr->ic, item_list, origin);
}

/* Read a list terminated by ending_token.  To read a list,
   ending_token will be ']' (AKA ic->n_rbracket).
   To read an array, ending_token will be '}' (AKA ic->n_rbrace).
 */
static sexpr *logoreadlist(logoreader *lr, sexpr *ending_token) {
  sexpr *first;
  sexpr *token = gettoken(lr);

  /* Skip newlines. */
  while(token != NULL && name_eq(token, lr->ic->n_newline)) {
      lr->ic->maybe_prompt(lr, "~ ");
      token = gettoken(lr);
  }

  if(token == NULL) return lr->ic->eof;

  /* If we're at the end, return the empty list. */
  if(name_eq(token, ending_token) && !lr->token_escaped)
    return lr->ic->g_nil;

  /* Otherwise, read an object, read the rest of the list, and
     cons the object onto the beginning of the list.
     The object must be protected from garbage collection that
     can occur while reading the rest of the list. */
  putback_token(lr, token);
  first = logoreadobj(lr); /* Must force evaluation order */
  protect_ptr(lr->ic->g, (void **) &first);
  sexpr *ret = cons(lr->ic, first, logoreadlist(lr, ending_token));
  unprotect_ptr(lr->ic->g);
  return ret;
}

/* Used by READLIST, the primitive that reads a newline terminated list
   of objects. */
static sexpr *readlisthelper(logoreader *lr, int parencount) {
  sexpr *first;
  sexpr *token = gettoken(lr);

  /* Skip newlines if we have outstanding parenthesis. */
  while(token != NULL && name_eq(token, lr->ic->n_newline) &&
        parencount > 0) {
      lr->ic->maybe_prompt(lr, "~ ");
      token = gettoken(lr);
  }
  
  if(token == NULL) return lr->ic->g_nil;

  /* Found a newline without outstanding parenthesis.  We're done. */
  if(name_eq(token, lr->ic->n_newline) && parencount <= 0)
      return lr->ic->g_nil;
  else if(name_eq(token, lr->ic->n_lparen))
      parencount++;
  else if(name_eq(token, lr->ic->n_rparen) && parencount > 0)
      parencount--;

  /* Otherwise, read an object, read a list, and cons the object
     onto the list. Must protect the object from garbage collection
     that may occur during the recursive call to readlisthelper(). */
  putback_token(lr, token);
  first = logoreadobj(lr); /* Must force evaluation order */
  protect_ptr(lr->ic->g, (void **) &first);
  sexpr *ret = cons(lr->ic, first, readlisthelper(lr, parencount));
  unprotect_ptr(lr->ic->g);
  return ret;
}


/* This implements the READLIST primitive.  It reads a newline terminated
   list of Logo objects. */
sexpr *readlist(logoreader *lr) {
  sexpr *token;
  IC *ic = lr->ic;

  ic->linemode(lr);

  token = gettoken(lr);
  if(token == NULL) return lr->ic->n_empty;

  putback_token(lr, token);
  return readlisthelper(lr, 0);
}

/* Read in a LINE of a procedure.  Create a line that contains
   the procedure name, a word containing the raw line, and a parsed
   version of the line. */
sexpr *readline(logoreader *lr, sexpr *procedure, sexpr *prompt) {
  sexpr *list, *line = lr->ic->eof;

  /* Tell the char reading function to log the characters read. */
  lr->logging_raw_line = 1;
  clear_byte_buffer(lr->raw_line);

  /* Read a list, the parsed version of the line.
     The list must be protected from possible garbage collection
     during the call to word_from_byte_buffer() below.
     Readlist returns the empty word (not list) on EOF. */
  lr->ic->maybe_prompt(lr, get_cstring(lr->ic, prompt));
  list = readlist(lr);
  protect_ptr(lr->ic->g, (void **) &list);
  if(name_eq(list, lr->ic->n_empty)) goto exit;

  /* Create the LINE.  The word from the byte_bytter is the
     raw line. */
  line = mk_line(lr->ic, word_from_byte_buffer(lr->ic, lr->raw_line),
                         list,
                         procedure);

  exit:
  /* Cleanup and return the line. */
  unprotect_ptr(lr->ic->g);
  lr->logging_raw_line = 0;
  clear_byte_buffer(lr->raw_line);
  return line;
}


/* Read a line and return it as a word.
   Returns the empty list on EOF. */
sexpr *readword(logoreader *lr) {
    IC *ic = lr->ic;
    byte_buffer *bb = mk_byte_buffer(lr->ic);
    int ch;

    ic->linemode(lr);

    ch = lr->char_reader(lr);

    for(;;) {
        if(ch == EOF) {
            /* Return the empty list (not a word) on EOF. */
            return ic->g_nil;
        } else if(ch == '\\') {
            /* Process backslash escapes.  
               The character following a backslash is kept as is. */
            ch = lr->char_reader(lr);
            if(ch == EOF)
                return ic->g_nil;
            add_to_byte_buffer(ic, bb, ch);

            /* If the character after the backslash is a newline, we keep
               it in the word, but print a prompt if reading from a 
               terminal. */
            if(ch == '\n')
                lr->ic->maybe_prompt(lr, "\\ ");
            ch = lr->char_reader(lr);
        } else if(ch == '|') {
            /* Keep characters between |'s */
            /* Also keep the beginning and ending |'s */
            add_to_byte_buffer(ic, bb, ch);
            ch = lr->char_reader(lr);
            for(;;) {
                /* Keep newlines between |'s */
                if(ch == '\n')
                    lr->ic->maybe_prompt(lr, "| ");
                
                if(ch == '\\') {
                    /* Keep any character after a backslash.
                       Prompt if the character is a newline. 
                       Do not keep the backslash. */
                    ch = lr->char_reader(lr);
                    if(ch == '\n')
                        lr->ic->maybe_prompt(lr, "\\ ");

                    if(ch == EOF) {
                        return ic->g_nil;
                    } else {
                        add_to_byte_buffer(ic, bb, ch);
                        ch = lr->char_reader(lr);
                    }
                } else if(ch == '|') {
                    /* We're done with the | enclosed string.
                       Include the trailing | and break out of
                       the loop. */
                    add_to_byte_buffer(ic, bb, ch);
                    ch = lr->char_reader(lr);
                    break;
                } else {
                    add_to_byte_buffer(ic, bb, ch);
                    ch = lr->char_reader(lr);
                }
            }
        } else if(ch == '~') {
            /* ~'s continue lines, and are kept in the word. */
            add_to_byte_buffer(ic, bb, ch); /* Keep the ~ */
            ch = lr->char_reader(lr);
            if(ch == '\n') {
                /* If it's followed by a newline, print a prompt. */
                lr->ic->maybe_prompt(lr, "~ ");
                add_to_byte_buffer(ic, bb, ch);
                ch = lr->char_reader(lr);
            }
        } else if(ch == '\n') {
            /* We found the end of the line.  Create the word
               and return it. */
            return word_from_byte_buffer(ic, bb);
        } else {
            /* Default case, add, read, and try again. */
            add_to_byte_buffer(ic, bb, ch);
            ch = lr->char_reader(lr);
        }
    }
}

/* Read a raw line.  No escape processing is performed.
   We just read until we get a newline. */
sexpr *readrawline(logoreader *lr) {
    IC *ic = lr->ic;
    byte_buffer *bb = mk_byte_buffer(lr->ic);
    int ch;

    ic->linemode(lr);

    ch = lr->char_reader(lr);

    for(;;) {
        if(ch == EOF) {
            /* On EOF return the empty list. */
            return ic->g_nil;
        } else if(ch == '\n') {
            /* Done on newline. */
            return word_from_byte_buffer(ic, bb);
        } else {
            /* All other characters we keep. */
            add_to_byte_buffer(ic, bb, ch);
            ch = lr->char_reader(lr);
        }
    }
}

/* READCHAR - read one character.
   Use cbreak mode if reading from a terminal. */
sexpr *readchar(logoreader *lr) {
    IC *ic = lr->ic;
    int ch;
    char cch;

    ic->charmode_blocking(lr);

    ch = lr->char_reader(lr);

    if(ch == EOF)
        return ic->g_nil;

    cch = ch;
    return intern_len(ic, NULL, &cch, 1);
}

/* READCHARS reads a given number of bytes.
   Uses cbreak mode if we are reading from a terminal. */
sexpr *readchars(logoreader *lr, int count) {
    IC *ic = lr->ic;
    int ch, i;
    byte_buffer *bb = mk_byte_buffer(ic);
    sexpr *ret;

    ic->charmode_blocking(lr);

    for(i = 0; i < count; i++) {
        ch = lr->char_reader(lr);
        if(ch == EOF) {
            ret = ic->g_nil;
            goto exit;
        }
        add_to_byte_buffer(ic, bb, ch);
    }

    ret = word_from_byte_buffer(ic, bb);

    exit:
    return ret;
}

/* Is there a keystroke waiting for us?
   Use cbreak nonblocking mode if we are reading from a terminal. */
sexpr *keyp(logoreader *lr) {
    int ch;
    sexpr *ret;

    if(lr->char_la_valid)
        return lr->ic->n_true;

    lr->ic->charmode_nonblocking(lr);

    fflush(stdout);

    ch = lr->char_reader(lr);
    if(ch == EOF) {
        ret = lr->ic->n_false;
    } else {
        put_back_char(lr, ch);
        ret = lr->ic->n_true;
    }

    /* Switch back to blocking mode.
       Stay in cbreak mode.  If we switch back to cooked
       mode the keystroke will be flushed. */
    lr->ic->charmode_blocking(lr);

    return ret;
}

/* The only garbage collected pointers in a logoreader are the
   byte_buffer's and the source string.
 */
static void mark_logoreader(GC *g, void *o, object_marker om, weak_pointer_registerer wpr) {
    logoreader *r = (logoreader *) o;
    om(g, (void **) &r->bb);
    om(g, (void **) &r->raw_line);
    om(g, (void **) &r->source_string);
}

/* Create a logoreader. */
logoreader *mk_logoreader(IC *ic) {
  logoreader *r = (logoreader *)ic_xmalloc(ic, sizeof(logoreader), mark_logoreader);
  r->ic = ic;
  r->bb = mk_byte_buffer(ic);       /* For building tokens. */
  r->raw_line = mk_byte_buffer(ic); /* For logging the raw line during a
                                       READLINE. */
  r->source_string = NULL;
  r->logging_raw_line = 0;
  r->token_la_valid = 0;
  r->last_break = BROKE_ON_SPACE;
  return r;
}
