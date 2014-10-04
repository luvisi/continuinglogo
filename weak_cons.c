
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

#include "weak_cons.h"
#include "interpreter.h"


/* Creates a weak_cons cell.

   The car of a weak_cons cell will act like a weak pointer if it
   points to a name that has no value, procedure, or property list.

   It is used in the global name list, and allows the garbage collector
   to collect names that nave no value, procedure, or property list and
   are not referenced from anywhere other than the name list.
 */
void mark_weak_cons(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct sexpr *s = (struct sexpr *) c;
  struct interpreter *ic = (struct interpreter *) g->roots;

  if(s->u.cons.car != NULL) {
    if(s->u.cons.car->t != NAME) {
        /* If the car doesn't point to a name, just mark it. */
        om(g, s->u.cons.car);
    } else if(s->u.cons.car->u.name.symbol->value != ic->g_unbound  ||
              s->u.cons.car->u.name.symbol->function != ic->g_unbound  ||
              !is_nil(ic, s->u.cons.car->u.name.symbol->properties)) {
        /* If the name contains a value, a function, or a property list
           then we treat the car as a strong pointer and mark it. */
        om(g, s->u.cons.car);
    } else {
        /* Otherwise, we treat the car as a weak pointer.
           If this name is referenced from elsewhere in the program,
           like from a print statement, it will still get marked and will
           not be collected.  However, if this is the only place from which
           it is referenced, it will be collected and the car of this
           weak_cons will be reset to ic->g_unbound. */
        wpr(g, (void **)&s->u.cons.car);
    }
  }
  om(g, s->u.cons.cdr);
}

/* This is just like mk_cons except that it uses mark_weak_cons instead of
   mark_cons.  In fact, the resulting cell is even tagged CONS so that it
   will look normal to the rest of the program.
 */
struct sexpr *mk_weak_cons(IC *ic, struct sexpr * car, struct sexpr * cdr) {
  struct sexpr *ret;
  protect(ic->g, car);
  protect(ic->g, cdr);
  ret = (struct sexpr *)ic_xmalloc(ic, sizeof(struct sexpr), mark_weak_cons);
  unprotect(ic->g);
  unprotect(ic->g);
  ret->t = CONS;
  ret->u.cons.car = car;
  ret->u.cons.cdr = cdr;
  return ret;
}

