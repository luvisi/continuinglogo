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

#ifndef STRUCTURES_H
#define STRUCTURES_H
#include "gc.h"
#include "interpreter.h"
#include "byte_buffer.h"
typedef struct sexpr *(*efun)(IC *interp,
                              struct sexpr *args);
enum sexpr_type { CONS, EMPTY_LIST, SUBR, FSUBR, PROC, FUNARG, MACRO, CONTINUATION, NAME, NUMBER, LINE, OUTPUT_ERROR_INFO, FILEP, BYTE_BUFFERP, UNBOUND, NO_VALUE, ARRAY };
struct sexpr { 
  enum sexpr_type t;
  union { 
    struct { 
      struct sexpr * car;
      struct sexpr * cdr;
      struct sexpr * proc_cache;
      struct sexpr * tree_cache;
    } cons;
    struct { 
    } empty_list;
    struct { 
      struct sexpr * name;
      efun func;
    } subr;
    struct { 
      struct sexpr * name;
      efun func;
    } fsubr;
    struct { 
      struct sexpr * proc;
      int minargs;
      int defargs;
      int maxargs;
    } proc;
    struct { 
      struct sexpr * lfun;
      struct frame * frame;
    } funarg;
    struct { 
      struct sexpr * macro_type;
      struct sexpr * expander;
    } macro;
    struct { 
      struct continuation * parent;
    } continuation;
    struct { 
      struct symbol * symbol;
      char * head;
      char * name;
      unsigned int length;
    } name;
    struct { 
      double value;
    } number;
    struct { 
      struct sexpr * raw_line;
      struct sexpr * parsed_line;
      struct sexpr * procedure;
    } line;
    struct { 
      struct sexpr * wanting;
      struct sexpr * to_blame;
      struct sexpr * line;
    } output_error_info;
    struct { 
      FILE * file;
    } filep;
    struct { 
      byte_buffer * byte_buffer;
    } byte_bufferp;
    struct { 
    } unbound;
    struct { 
    } no_value;
    struct { 
      struct sexpr ** members;
      unsigned int length;
      unsigned int origin;
    } array;
  } u;
};

void mark_cons(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_empty_list(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_subr(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_fsubr(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_proc(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_funarg(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_macro(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_continuation(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_name(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_number(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_line(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_output_error_info(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_filep(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_byte_bufferp(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_unbound(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_no_value(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_array(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);

struct sexpr *mk_cons(IC *ic, struct sexpr * car, struct sexpr * cdr, struct sexpr * proc_cache, struct sexpr * tree_cache);
struct sexpr *unsafe_mk_cons(IC *ic, struct sexpr * car, struct sexpr * cdr, struct sexpr * proc_cache, struct sexpr * tree_cache);
struct sexpr *mk_empty_list(IC *ic);
struct sexpr *unsafe_mk_empty_list(IC *ic);
struct sexpr *mk_subr(IC *ic, struct sexpr * name, efun func);
struct sexpr *unsafe_mk_subr(IC *ic, struct sexpr * name, efun func);
struct sexpr *mk_fsubr(IC *ic, struct sexpr * name, efun func);
struct sexpr *unsafe_mk_fsubr(IC *ic, struct sexpr * name, efun func);
struct sexpr *mk_proc(IC *ic, struct sexpr * proc, int minargs, int defargs, int maxargs);
struct sexpr *unsafe_mk_proc(IC *ic, struct sexpr * proc, int minargs, int defargs, int maxargs);
struct sexpr *mk_funarg(IC *ic, struct sexpr * lfun, struct frame * frame);
struct sexpr *unsafe_mk_funarg(IC *ic, struct sexpr * lfun, struct frame * frame);
struct sexpr *mk_macro(IC *ic, struct sexpr * macro_type, struct sexpr * expander);
struct sexpr *unsafe_mk_macro(IC *ic, struct sexpr * macro_type, struct sexpr * expander);
struct sexpr *mk_continuation(IC *ic, struct continuation * parent);
struct sexpr *unsafe_mk_continuation(IC *ic, struct continuation * parent);
struct sexpr *mk_name(IC *ic, struct symbol * symbol, char * head, char * name, unsigned int length);
struct sexpr *unsafe_mk_name(IC *ic, struct symbol * symbol, char * head, char * name, unsigned int length);
struct sexpr *mk_number(IC *ic, double value);
struct sexpr *unsafe_mk_number(IC *ic, double value);
struct sexpr *mk_line(IC *ic, struct sexpr * raw_line, struct sexpr * parsed_line, struct sexpr * procedure);
struct sexpr *unsafe_mk_line(IC *ic, struct sexpr * raw_line, struct sexpr * parsed_line, struct sexpr * procedure);
struct sexpr *mk_output_error_info(IC *ic, struct sexpr * wanting, struct sexpr * to_blame, struct sexpr * line);
struct sexpr *unsafe_mk_output_error_info(IC *ic, struct sexpr * wanting, struct sexpr * to_blame, struct sexpr * line);
struct sexpr *mk_filep(IC *ic, FILE * file);
struct sexpr *unsafe_mk_filep(IC *ic, FILE * file);
struct sexpr *mk_byte_bufferp(IC *ic, byte_buffer * byte_buffer);
struct sexpr *unsafe_mk_byte_bufferp(IC *ic, byte_buffer * byte_buffer);
struct sexpr *mk_unbound(IC *ic);
struct sexpr *unsafe_mk_unbound(IC *ic);
struct sexpr *mk_no_value(IC *ic);
struct sexpr *unsafe_mk_no_value(IC *ic);
struct sexpr *mk_array(IC *ic, struct sexpr ** members, unsigned int length, unsigned int origin);
struct sexpr *unsafe_mk_array(IC *ic, struct sexpr ** members, unsigned int length, unsigned int origin);

enum continuation_type { IF_C, BEGIN_C, OPER_C, APPLY_C, RETURN_C, EVAL_C, BEGINRESULT_C, OPTIONALS_C, DONT_SAY_C, DIDNT_OUTPUT_C, TRACE_C };
struct continuation { 
  enum continuation_type t;
  struct continuation * parent;
  struct frame * frame;
  struct sexpr * expr;
  struct sexpr * line;
  int allowed_results;
  struct sexpr * output_error_info;
  int tail;
  union { 
    struct { 
      struct sexpr * then_expr;
      struct sexpr * else_expr;
    } if_c;
    struct { 
      struct sexpr * exprs;
    } begin_c;
    struct { 
      struct sexpr * params;
    } oper_c;
    struct { 
      struct sexpr * oper;
      struct sexpr * exprs;
      struct sexpr * values;
    } apply_c;
    struct { 
    } return_c;
    struct { 
    } eval_c;
    struct { 
    } beginresult_c;
    struct { 
      struct sexpr * oper;
      struct sexpr * formals;
    } optionals_c;
    struct { 
    } dont_say_c;
    struct { 
    } didnt_output_c;
    struct { 
      struct sexpr * procedure;
    } trace_c;
  } u;
};

void mark_if_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_begin_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_oper_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_apply_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_return_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_eval_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_beginresult_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_optionals_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_dont_say_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_didnt_output_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);
void mark_trace_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);

struct continuation *mk_if_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * then_expr, struct sexpr * else_expr);
struct continuation *unsafe_mk_if_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * then_expr, struct sexpr * else_expr);
struct continuation *mk_begin_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * exprs);
struct continuation *unsafe_mk_begin_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * exprs);
struct continuation *mk_oper_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * params);
struct continuation *unsafe_mk_oper_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * params);
struct continuation *mk_apply_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * exprs, struct sexpr * values);
struct continuation *unsafe_mk_apply_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * exprs, struct sexpr * values);
struct continuation *mk_return_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *unsafe_mk_return_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *mk_eval_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *unsafe_mk_eval_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *mk_beginresult_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *unsafe_mk_beginresult_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *mk_optionals_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * formals);
struct continuation *unsafe_mk_optionals_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * formals);
struct continuation *mk_dont_say_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *unsafe_mk_dont_say_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *mk_didnt_output_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *unsafe_mk_didnt_output_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail);
struct continuation *mk_trace_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * procedure);
struct continuation *unsafe_mk_trace_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * procedure);

#endif
