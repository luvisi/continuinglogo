
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
#include <ctype.h>
#include <string.h>
#include "pcgc.h"
#include "list_memory.h"
#include "reader.h"

void read_from_user_terminal(IC *ic) {
    read_from_file(ic, stdin);
}

/* This function is stored into r->char_reader when we are reading from
   a file. */
static int fetch_char_from_file(reader *r) {
    if(r->char_la_valid) {
        r->char_la_valid = 0;
        return r->char_la;
    } else {
        return getc(r->ifp);
    }
}

/* Set things up so that future calls to r->char_reader(r) will read from
   the file pointer fp. */
void read_from_file(IC *ic, FILE *fp) {
    if(fp == NULL) {
        ic->read_from_user(ic);
        return;
    }
    reader *r = ic->r;
    r->ifp = fp;
    r->char_reader = fetch_char_from_file;
    r->char_la_valid = 0;
}


/* This function is stored into r->char_reader when we are reading from
   a string. */
static int fetch_char_from_string(reader *r) {
    if(r->char_la_valid) {
        r->char_la_valid = 0;
        return r->char_la;
    } else if(r->source_string[r->source_position] == '\0') {
        return EOF;
    } else {
        return r->source_string[r->source_position++];
    }
}

/* Set things up so that future calls to r->char_reader(r) will read from
   the string source. */
void read_from_string(IC *ic, char *source) {
    reader *r = ic->r;
    r->source_string = source;
    r->source_position = 0;
    r->char_reader = fetch_char_from_string;
    r->char_la_valid = 0;
}

/* Put back a character so the next call to r->char_reader(r) will
   return it again. */
static void put_back_char(reader *r, char ch) {
    r->char_la = ch;
    r->char_la_valid = 1;
}


/* Add a character to the token we are building up in buf.
   Length check must leave room for the null that will be
   added at the end. */
static void add_to_buf(reader *r, char ch) {
    if(r->bufused < TOKEN_MAXLEN - 1)
        r->buf[r->bufused++] = ch; 
}

/* Put back a token so the next call to gettoken(r) will
   return it again.
   The last token returned is still in the buffer, so we just
   mark it as a lookahead token.  */
static void putback_token(reader *r, char *token) {
    r->token_la_valid = 1;
}


/* This is the lexer.  Build a token up in buf, null terminate it, and
   return a pointer to buf.  Clients must copy this out of buf before
   making any more calls to gettoken. */
static char *gettoken(reader *r) {
  int ch;

  /* If we have a valid lookahead, just return the buffer again. */
  if(r->token_la_valid) { r->token_la_valid = 0; return r->buf; }

  /* Empty the buffer */
  r->bufused = 0;


  /* Skip whitespace and comments.
     Comments begin with a ; and continue to the end of
     the line. */
  do {
    if((ch = r->char_reader(r)) == EOF)
        return NULL;
    if(ch == ';') {
      do {
        ch = r->char_reader(r);
      } while(ch != '\n' && ch != EOF);
      if(ch == EOF) return NULL;
    }
  } while(isspace(ch));
  add_to_buf(r, ch);

  /* If the token begins with one of these characters, then we know
     we are about to return. */
  if(strchr("()\'`,;", ch)) {
    if(ch == ',') {
      /* Treat ,@ as a single token.  It will be converted into
         unquote_splicing. */
      ch = r->char_reader(r);
      if(ch == '@')
        add_to_buf(r, ch);
      else
        put_back_char(r, ch);
    }
    r->buf[r->bufused] = '\0';
    return r->buf;
  }


  /* Keep reading characters until we find a whitespace or a character
     that begins a new token. */
  for(;;) {
    if((ch = r->char_reader(r)) == EOF)
        return NULL;
    if(strchr("()\'", ch) || isspace(ch)) {
      /* We have found a whitespace, a parenthesis, or a single
         quote.  Put it back, null terminate the buffer, and return. */
      put_back_char(r, ch);
      r->buf[r->bufused] = '\0';
      return r->buf;
    }
    add_to_buf(r, ch);
  }
}

/* Forward declaration */
static sexpr *readlist(IC *ic);

/* Read a lisp object.  This is the main interface to the reader. */
sexpr *readobj(IC *ic) {
  reader *r = ic->r;
  char *token;

  /* These are used by strtod().  Number is the return value, and
     unparsed is set to the portion of the string not parsed.
     If it is not pointing at the terminating '\0' when we are done, then
     we failed to get a number. */
  double number;
  char *unparsed;

  token = gettoken(r);
  if(token == NULL) return ic->eof;
  if(!strcmp(token, "(")) return readlist(ic);
  if(!strcmp(token, "\'") ||
     !strcmp(token, "`") ||
     !strcmp(token, ",") ||
     !strcmp(token, ",@")) {
      /* We are going to read the following object, and then wrap it in
         a call to something.  Figure out what that something is. */
      sexpr *quoter = NULL;
      protect_ptr(ic->g, (void **)&quoter);
      if(!strcmp(token, "\'"))
          STORE(ic->g, NULL, quoter, ic->n_quote);
      else if(!strcmp(token, "`"))
          STORE(ic->g, NULL, quoter, ic->n_quasiquote);
      else if(!strcmp(token, ","))
          STORE(ic->g, NULL, quoter, ic->n_unquote);
      else if(!strcmp(token, ",@"))
          STORE(ic->g, NULL, quoter, ic->n_unquote_splicing);
      else {
          fprintf(stderr,
                  "Fatal error in lisp reader - this should never happen!\n");
          longjmp(ic->quit, 1);
      }
    
    sexpr *obj = readobj(ic);
    protect_ptr(ic->g, (void **)&obj);
    sexpr *ret = listl(ic, quoter, obj, NULL);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
  }


  /* Check to see if it's a valid number. */
  if(strcasecmp(token, "inf") &&
     strcasecmp(token, "infinity") &&
     strcasecmp(token, "nan")) {
    number = strtod(token, &unparsed);
    if(unparsed != token && *unparsed == '\0')
      return mk_number(ic, number);
  }

  return intern(ic, token);
}

/* Read a list.  The opening '(' has already been read when this is called. */
static sexpr *readlist(IC *ic) {
  char *token = gettoken(ic->r);
  if(token == NULL) return ic->eof;

  if(!strcmp(token, ")")) return ic->g_nil;
  if(!strcmp(token, ".")) {
    sexpr *tmp = readobj(ic);
    token = gettoken(ic->r);
    if(token == NULL) return ic->eof;
    if(strcmp(token, ")")) return ic->g_nil;
    return tmp;
  }
  putback_token(ic->r, token);
  sexpr *first = readobj(ic); /* Must force evaluation order */
  protect_ptr(ic->g, (void **) &first);
  sexpr *rest = readlist(ic);
  protect_ptr(ic->g, (void **) &rest);
  sexpr *ret = cons(ic, first, rest);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  return ret;
}

/* Readers have no garbage collected members to mark. */
static void mark_reader(GC *g, void *o, object_marker om, weak_pointer_registerer wpr) {}

reader *mk_reader(IC *ic) {
  reader *r = (reader *)ic_xmalloc(ic, sizeof(reader), mark_reader);
  r->token_la_valid = 0;
  return r;
}
