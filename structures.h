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
#include "pcgc.h"
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
      const char * head;
      unsigned int start;
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
      byte_buffer * bb;
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

static void mark_cons(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.cons.car);
  om(g, (void **) &s->u.cons.cdr);
  om(g, (void **) &s->u.cons.proc_cache);
  om(g, (void **) &s->u.cons.tree_cache);
}

static void mark_empty_list(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
}

static void mark_subr(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.subr.name);
}

static void mark_fsubr(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.fsubr.name);
}

static void mark_proc(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.proc.proc);
}

static void mark_funarg(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.funarg.lfun);
  om(g, (void **) &s->u.funarg.frame);
}

static void mark_macro(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.macro.macro_type);
  om(g, (void **) &s->u.macro.expander);
}

static void mark_continuation(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.continuation.parent);
}

static void mark_name(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.name.symbol);
  om(g, (void **) &s->u.name.head);
}

static void mark_number(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
}

static void mark_line(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.line.raw_line);
  om(g, (void **) &s->u.line.parsed_line);
  om(g, (void **) &s->u.line.procedure);
}

static void mark_output_error_info(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.output_error_info.wanting);
  om(g, (void **) &s->u.output_error_info.to_blame);
  om(g, (void **) &s->u.output_error_info.line);
}

static void mark_filep(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
}

static void mark_byte_bufferp(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.byte_bufferp.bb);
}

static void mark_unbound(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
}

static void mark_no_value(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
}

static void mark_array(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  om(g, (void **) &s->u.array.members);
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_cons(IC *ic, struct sexpr * car, struct sexpr * cdr, struct sexpr * proc_cache, struct sexpr * tree_cache) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &car);
  protect_ptr(ic->g, (void **) &cdr);
  protect_ptr(ic->g, (void **) &proc_cache);
  protect_ptr(ic->g, (void **) &tree_cache);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_cons);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = CONS;
  ret->u.cons.car = car;
  ret->u.cons.cdr = cdr;
  ret->u.cons.proc_cache = proc_cache;
  ret->u.cons.tree_cache = tree_cache;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_cons(IC *ic, struct sexpr * car, struct sexpr * cdr, struct sexpr * proc_cache, struct sexpr * tree_cache) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &car);
  protect_ptr(ic->g, (void **) &cdr);
  protect_ptr(ic->g, (void **) &proc_cache);
  protect_ptr(ic->g, (void **) &tree_cache);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_cons);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = CONS;
  ret->u.cons.car = car;
  ret->u.cons.cdr = cdr;
  ret->u.cons.proc_cache = proc_cache;
  ret->u.cons.tree_cache = tree_cache;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_empty_list(IC *ic) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_empty_list);
  ret->t = EMPTY_LIST;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_empty_list(IC *ic) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_empty_list);
  ret->t = EMPTY_LIST;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_subr(IC *ic, struct sexpr * name, efun func) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &name);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_subr);
  unprotect_ptr(ic->g);
  ret->t = SUBR;
  ret->u.subr.name = name;
  ret->u.subr.func = func;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_subr(IC *ic, struct sexpr * name, efun func) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &name);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_subr);
  unprotect_ptr(ic->g);
  ret->t = SUBR;
  ret->u.subr.name = name;
  ret->u.subr.func = func;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_fsubr(IC *ic, struct sexpr * name, efun func) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &name);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_fsubr);
  unprotect_ptr(ic->g);
  ret->t = FSUBR;
  ret->u.fsubr.name = name;
  ret->u.fsubr.func = func;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_fsubr(IC *ic, struct sexpr * name, efun func) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &name);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_fsubr);
  unprotect_ptr(ic->g);
  ret->t = FSUBR;
  ret->u.fsubr.name = name;
  ret->u.fsubr.func = func;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_proc(IC *ic, struct sexpr * proc, int minargs, int defargs, int maxargs) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &proc);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_proc);
  unprotect_ptr(ic->g);
  ret->t = PROC;
  ret->u.proc.proc = proc;
  ret->u.proc.minargs = minargs;
  ret->u.proc.defargs = defargs;
  ret->u.proc.maxargs = maxargs;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_proc(IC *ic, struct sexpr * proc, int minargs, int defargs, int maxargs) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &proc);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_proc);
  unprotect_ptr(ic->g);
  ret->t = PROC;
  ret->u.proc.proc = proc;
  ret->u.proc.minargs = minargs;
  ret->u.proc.defargs = defargs;
  ret->u.proc.maxargs = maxargs;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_funarg(IC *ic, struct sexpr * lfun, struct frame * frame) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &lfun);
  protect_ptr(ic->g, (void **) &frame);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_funarg);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = FUNARG;
  ret->u.funarg.lfun = lfun;
  ret->u.funarg.frame = frame;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_funarg(IC *ic, struct sexpr * lfun, struct frame * frame) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &lfun);
  protect_ptr(ic->g, (void **) &frame);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_funarg);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = FUNARG;
  ret->u.funarg.lfun = lfun;
  ret->u.funarg.frame = frame;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_macro(IC *ic, struct sexpr * macro_type, struct sexpr * expander) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &macro_type);
  protect_ptr(ic->g, (void **) &expander);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_macro);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = MACRO;
  ret->u.macro.macro_type = macro_type;
  ret->u.macro.expander = expander;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_macro(IC *ic, struct sexpr * macro_type, struct sexpr * expander) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &macro_type);
  protect_ptr(ic->g, (void **) &expander);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_macro);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = MACRO;
  ret->u.macro.macro_type = macro_type;
  ret->u.macro.expander = expander;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_continuation(IC *ic, struct continuation * parent) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &parent);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_continuation);
  unprotect_ptr(ic->g);
  ret->t = CONTINUATION;
  ret->u.continuation.parent = parent;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_continuation(IC *ic, struct continuation * parent) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &parent);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_continuation);
  unprotect_ptr(ic->g);
  ret->t = CONTINUATION;
  ret->u.continuation.parent = parent;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_name(IC *ic, struct symbol * symbol, const char * head, unsigned int start, unsigned int length) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &symbol);
  protect_ptr(ic->g, (void **) &head);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_name);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = NAME;
  ret->u.name.symbol = symbol;
  ret->u.name.head = head;
  ret->u.name.start = start;
  ret->u.name.length = length;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_name(IC *ic, struct symbol * symbol, const char * head, unsigned int start, unsigned int length) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &symbol);
  protect_ptr(ic->g, (void **) &head);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_name);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = NAME;
  ret->u.name.symbol = symbol;
  ret->u.name.head = head;
  ret->u.name.start = start;
  ret->u.name.length = length;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_number(IC *ic, double value) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_number);
  ret->t = NUMBER;
  ret->u.number.value = value;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_number(IC *ic, double value) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_number);
  ret->t = NUMBER;
  ret->u.number.value = value;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_line(IC *ic, struct sexpr * raw_line, struct sexpr * parsed_line, struct sexpr * procedure) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &raw_line);
  protect_ptr(ic->g, (void **) &parsed_line);
  protect_ptr(ic->g, (void **) &procedure);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_line);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = LINE;
  ret->u.line.raw_line = raw_line;
  ret->u.line.parsed_line = parsed_line;
  ret->u.line.procedure = procedure;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_line(IC *ic, struct sexpr * raw_line, struct sexpr * parsed_line, struct sexpr * procedure) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &raw_line);
  protect_ptr(ic->g, (void **) &parsed_line);
  protect_ptr(ic->g, (void **) &procedure);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_line);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = LINE;
  ret->u.line.raw_line = raw_line;
  ret->u.line.parsed_line = parsed_line;
  ret->u.line.procedure = procedure;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_output_error_info(IC *ic, struct sexpr * wanting, struct sexpr * to_blame, struct sexpr * line) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &wanting);
  protect_ptr(ic->g, (void **) &to_blame);
  protect_ptr(ic->g, (void **) &line);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_output_error_info);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = OUTPUT_ERROR_INFO;
  ret->u.output_error_info.wanting = wanting;
  ret->u.output_error_info.to_blame = to_blame;
  ret->u.output_error_info.line = line;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_output_error_info(IC *ic, struct sexpr * wanting, struct sexpr * to_blame, struct sexpr * line) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &wanting);
  protect_ptr(ic->g, (void **) &to_blame);
  protect_ptr(ic->g, (void **) &line);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_output_error_info);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = OUTPUT_ERROR_INFO;
  ret->u.output_error_info.wanting = wanting;
  ret->u.output_error_info.to_blame = to_blame;
  ret->u.output_error_info.line = line;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_filep(IC *ic, FILE * file) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_filep);
  ret->t = FILEP;
  ret->u.filep.file = file;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_filep(IC *ic, FILE * file) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_filep);
  ret->t = FILEP;
  ret->u.filep.file = file;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_byte_bufferp(IC *ic, byte_buffer * bb) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &bb);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_byte_bufferp);
  unprotect_ptr(ic->g);
  ret->t = BYTE_BUFFERP;
  ret->u.byte_bufferp.bb = bb;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_byte_bufferp(IC *ic, byte_buffer * bb) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &bb);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_byte_bufferp);
  unprotect_ptr(ic->g);
  ret->t = BYTE_BUFFERP;
  ret->u.byte_bufferp.bb = bb;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_unbound(IC *ic) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_unbound);
  ret->t = UNBOUND;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_unbound(IC *ic) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_unbound);
  ret->t = UNBOUND;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_no_value(IC *ic) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_no_value);
  ret->t = NO_VALUE;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_no_value(IC *ic) {
  struct sexpr *ret;
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_no_value);
  ret->t = NO_VALUE;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *mk_array(IC *ic, struct sexpr ** members, unsigned int length, unsigned int origin) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &members);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_array);
  unprotect_ptr(ic->g);
  ret->t = ARRAY;
  ret->u.array.members = members;
  ret->u.array.length = length;
  ret->u.array.origin = origin;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct sexpr *unsafe_mk_array(IC *ic, struct sexpr ** members, unsigned int length, unsigned int origin) {
  struct sexpr *ret;
  protect_ptr(ic->g, (void **) &members);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_array);
  unprotect_ptr(ic->g);
  ret->t = ARRAY;
  ret->u.array.members = members;
  ret->u.array.length = length;
  ret->u.array.origin = origin;
  return ret;
}

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
      struct sexpr * proc;
      struct sexpr * funarg;
      struct sexpr * macro;
      struct sexpr * real_oper;
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

static void mark_if_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
  om(g, (void **) &s->u.if_c.then_expr);
  om(g, (void **) &s->u.if_c.else_expr);
}

static void mark_begin_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
  om(g, (void **) &s->u.begin_c.exprs);
}

static void mark_oper_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
  om(g, (void **) &s->u.oper_c.params);
}

static void mark_apply_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
  om(g, (void **) &s->u.apply_c.oper);
  om(g, (void **) &s->u.apply_c.exprs);
  om(g, (void **) &s->u.apply_c.values);
  om(g, (void **) &s->u.apply_c.proc);
  om(g, (void **) &s->u.apply_c.funarg);
  om(g, (void **) &s->u.apply_c.macro);
  om(g, (void **) &s->u.apply_c.real_oper);
}

static void mark_return_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
}

static void mark_eval_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
}

static void mark_beginresult_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
}

static void mark_optionals_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
  om(g, (void **) &s->u.optionals_c.oper);
  om(g, (void **) &s->u.optionals_c.formals);
}

static void mark_dont_say_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
}

static void mark_didnt_output_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
}

static void mark_trace_c(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct continuation *s = (struct continuation *) c;
  om(g, (void **) &s->parent);
  om(g, (void **) &s->frame);
  om(g, (void **) &s->expr);
  om(g, (void **) &s->line);
  om(g, (void **) &s->output_error_info);
  om(g, (void **) &s->u.trace_c.procedure);
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_if_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * then_expr, struct sexpr * else_expr) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &then_expr);
  protect_ptr(ic->g, (void **) &else_expr);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_if_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = IF_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.if_c.then_expr = then_expr;
  ret->u.if_c.else_expr = else_expr;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_if_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * then_expr, struct sexpr * else_expr) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &then_expr);
  protect_ptr(ic->g, (void **) &else_expr);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_if_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = IF_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.if_c.then_expr = then_expr;
  ret->u.if_c.else_expr = else_expr;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_begin_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * exprs) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &exprs);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_begin_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = BEGIN_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.begin_c.exprs = exprs;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_begin_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * exprs) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &exprs);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_begin_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = BEGIN_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.begin_c.exprs = exprs;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_oper_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * params) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &params);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_oper_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = OPER_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.oper_c.params = params;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_oper_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * params) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &params);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_oper_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = OPER_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.oper_c.params = params;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_apply_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * exprs, struct sexpr * values, struct sexpr * proc, struct sexpr * funarg, struct sexpr * macro, struct sexpr * real_oper) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &oper);
  protect_ptr(ic->g, (void **) &exprs);
  protect_ptr(ic->g, (void **) &values);
  protect_ptr(ic->g, (void **) &proc);
  protect_ptr(ic->g, (void **) &funarg);
  protect_ptr(ic->g, (void **) &macro);
  protect_ptr(ic->g, (void **) &real_oper);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_apply_c);
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
  unprotect_ptr(ic->g);
  ret->t = APPLY_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.apply_c.oper = oper;
  ret->u.apply_c.exprs = exprs;
  ret->u.apply_c.values = values;
  ret->u.apply_c.proc = proc;
  ret->u.apply_c.funarg = funarg;
  ret->u.apply_c.macro = macro;
  ret->u.apply_c.real_oper = real_oper;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_apply_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * exprs, struct sexpr * values, struct sexpr * proc, struct sexpr * funarg, struct sexpr * macro, struct sexpr * real_oper) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &oper);
  protect_ptr(ic->g, (void **) &exprs);
  protect_ptr(ic->g, (void **) &values);
  protect_ptr(ic->g, (void **) &proc);
  protect_ptr(ic->g, (void **) &funarg);
  protect_ptr(ic->g, (void **) &macro);
  protect_ptr(ic->g, (void **) &real_oper);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_apply_c);
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
  unprotect_ptr(ic->g);
  ret->t = APPLY_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.apply_c.oper = oper;
  ret->u.apply_c.exprs = exprs;
  ret->u.apply_c.values = values;
  ret->u.apply_c.proc = proc;
  ret->u.apply_c.funarg = funarg;
  ret->u.apply_c.macro = macro;
  ret->u.apply_c.real_oper = real_oper;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_return_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_return_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = RETURN_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_return_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_return_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = RETURN_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_eval_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_eval_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = EVAL_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_eval_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_eval_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = EVAL_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_beginresult_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_beginresult_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = BEGINRESULT_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_beginresult_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_beginresult_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = BEGINRESULT_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_optionals_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * formals) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &oper);
  protect_ptr(ic->g, (void **) &formals);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_optionals_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = OPTIONALS_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.optionals_c.oper = oper;
  ret->u.optionals_c.formals = formals;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_optionals_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * oper, struct sexpr * formals) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &oper);
  protect_ptr(ic->g, (void **) &formals);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_optionals_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = OPTIONALS_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.optionals_c.oper = oper;
  ret->u.optionals_c.formals = formals;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_dont_say_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_dont_say_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = DONT_SAY_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_dont_say_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_dont_say_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = DONT_SAY_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_didnt_output_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_didnt_output_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = DIDNT_OUTPUT_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_didnt_output_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_didnt_output_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = DIDNT_OUTPUT_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *mk_trace_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * procedure) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &procedure);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_trace_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = TRACE_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.trace_c.procedure = procedure;
  return ret;
}

static inline 
#ifdef __GNUC__
#ifdef FORCE_INLINES
__attribute__ ((__always_inline__))
#endif
#endif
struct continuation *unsafe_mk_trace_c(IC *ic, struct continuation * parent, struct frame * frame, struct sexpr * expr, struct sexpr * line, int allowed_results, struct sexpr * output_error_info, int tail, struct sexpr * procedure) {
  struct continuation *ret;
  protect_ptr(ic->g, (void **) &parent);
  protect_ptr(ic->g, (void **) &frame);
  protect_ptr(ic->g, (void **) &expr);
  protect_ptr(ic->g, (void **) &line);
  protect_ptr(ic->g, (void **) &output_error_info);
  protect_ptr(ic->g, (void **) &procedure);
  ret = (struct continuation *)ic_xmalloc(ic, sizeof(struct continuation), mark_trace_c);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->t = TRACE_C;
  ret->parent = parent;
  ret->frame = frame;
  ret->expr = expr;
  ret->line = line;
  ret->allowed_results = allowed_results;
  ret->output_error_info = output_error_info;
  ret->tail = tail;
  ret->u.trace_c.procedure = procedure;
  return ret;
}

#endif
