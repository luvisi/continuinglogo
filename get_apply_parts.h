
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

#ifndef GET_APPLY_PARTS_H
#define GET_APPLY_PARTS_H

/* Given an operator, e, tries to figure out:
      The PROC, if any, which contains the minimum, default, and maximum args
      for the operator.

      The FUNARG, if any, which contains the frame in which to execute the
      operator.

      The MACRO, if any, which is a marker that this operator is a macro.

      The actual operator which will be applied to the arguments.

      print_errors can be set to 1 to enable printing an error if we don't
      find anything.  This is set to 0 in situations where it isn't
      necessarily an error to find nothing, such as in PROCEDUREP.
 */
inline static int get_apply_parts(IC *ic,
                    sexpr *e,
                    sexpr **procp,
                    sexpr **funargp,
                    sexpr **macrop,
                    sexpr **operp,
                    int print_errors) {
    protect_ptr(ic->g, (void **) &e);

    sexpr *original_e = e;
    protect_ptr(ic->g, (void **) &original_e);

    STORE(ic->g, NULL, *procp, ic->g_nil);
    STORE(ic->g, NULL, *funargp, ic->g_nil);
    STORE(ic->g, NULL, *macrop, ic->g_nil);
    STORE(ic->g, NULL, *operp, ic->g_nil);

    int ret = 0;

    for(;;) {
        if(e->t == NAME) {
            if(e->u.name.symbol->function == ic->g_unbound) {
                if(print_errors) {
                    eprintf(ic, "I don't know how to ");
                    eprint_sexpr(ic, e);
                }
                ret = 0;
                goto end;
            } else {
                STORE(ic->g, NULL, e, e->u.name.symbol->function);
            }
        } else if(e->t == PROC) {
            STORE(ic->g, NULL, *procp, e);
            STORE(ic->g, NULL, e, e->u.proc.proc);
        } else if(e->t == FUNARG) {
            STORE(ic->g, NULL, *funargp, e);
            STORE(ic->g, NULL, e, e->u.funarg.lfun);
        } else if(e->t == MACRO) {
            STORE(ic->g, NULL, *macrop, e);
            STORE(ic->g, NULL, e, e->u.macro.expander);
        } else if(e->t == CONS &&
                  car(e) == ic->n_lambda &&
                  !is_nil(ic, cdr(e)) &&
                  (is_nil(ic, car(cdr(e))) ||
                   car(cdr(e))->t == CONS)) {
            STORE(ic->g, NULL, *operp, e);
            ret = 1;
            goto end;
        } else if(e->t == SUBR || e->t == FSUBR || e->t == CONTINUATION) {
            STORE(ic->g, NULL, *operp, e);
            ret = 1;
            goto end;
        } else {
            if(print_errors) {
                eprintf(ic, "I don't know how to ");
                eprint_sexpr(ic, original_e);
            }
            ret = 0;
            goto end;
        }
    }

    end:
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    return ret;
}

#endif
