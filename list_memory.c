
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

#include "list_memory.h"
#include "weak_cons.h"
#include "pcgc.h"
#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Wrapper for allocate() that aborts execution if we are truly out of
   memory.
 */
void *ic_xmalloc(IC *ic, size_t size, pointer_iterator marker) {
    void *ret = gc_allocate(ic->g, size, marker);
    if(ret == NULL) {
        fprintf(stderr, "Out of memory!\n");
        longjmp(ic->quit, 1);
    }
    return ret;
}

/* Do nothing.  Strings do not have any pointers to garbage collected
   objects.
 */
void mark_cstring(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {}

/* Creates garbage collected copy of namestr if namehead is NULL and
   creates an unbound and uninterned name using mk_symbol and mk_name.
 */
sexpr *new_name(IC *ic, const char *namehead, const char *namestr, unsigned int len) {
    const char *new_head, *new_name;
    struct symbol *symbol;
    sexpr *name;

    if(namehead == NULL) {
        char *tmp = (char *)ic_xmalloc(ic, len, mark_cstring);
        strncpy(tmp, namestr, len);
        new_head = new_name = tmp;
    } else {
        new_head = namehead;
        new_name = namestr;
    }


    protect_ptr(ic->g, (void **) &new_head);
    symbol = mk_symbol(ic,
                       ic->g_unbound,  /* Unbound value */
                       ic->g_unbound,  /* Unbound function */
                       ic->g_nil,      /* Empty function_source */
                       ic->g_nil,      /* Empty plist */
                       ic->g_nil,      /* Empty canonical name */
                       ic->g_nil,      /* Empty names list */
                       0);             /* No flags set */
    name = mk_name(ic, symbol, new_head, new_name-new_head, len);
    STORE(ic->g, symbol, symbol->canonical_name, name);
    STORE(ic->g, symbol, symbol->names, cons(ic, name, ic->g_nil));
    unprotect_ptr(ic->g);

    return name;
}

int min(int a, int b) { return a < b ? a : b; }

/* Finds or creates the appropriate name given a canonical name.
   If a name must be created, adds it to the namelist for the symbol. */
sexpr *find_or_create_name(IC *ic,
                           sexpr *cname,
                           const char *namestr,
                           unsigned int len) {
    char *new_name;
    sexpr *names;
    sexpr *name;
    struct symbol *symbol;

    names = cname->u.name.symbol->names;
    while(!is_nil(ic, names)) {
        /* Comparison IS case sensitive here. */
        if(car(names)->u.name.length == len &&
           !strncmp(car(names)->u.name.head+car(names)->u.name.start, namestr, len)) {
            return car(names);
        }
        names = cdr(names);
    }

    new_name = (char *)ic_xmalloc(ic, len, mark_cstring);

    strncpy(new_name, namestr, len);
    symbol = cname->u.name.symbol;
    name = mk_name(ic, symbol, new_name, 0, len);
    STORE(ic->g, symbol, symbol->names,
          cons(ic, name, symbol->names));

    return name;
}

/* Dan Bernstein hash function */
static unsigned long djb2_hash(const char *str, int count) {
    unsigned long hash = 5381;
    int i;

    for(i = 0; i < count; i++)
        hash = ((hash << 5) + hash) + (unsigned char)tolower(str[i]); /* hash * 33 + c */

    return hash;
}

/* Intern a string.  Try to find a matching symbol with case insensitive
   comparison.
   If we find one, call find_or_create_name above to either get or
   create a name with the appropriate case.
   If we do not find one, create a new name and add it to the symbol
   list.
 */
  
sexpr *intern_len(IC *ic, const char *namehead, const char *namestr, unsigned int len) {
    unsigned long hash = djb2_hash(namestr, len) % NAME_TABLE_HASH_BUCKETS;
    sexpr *names = ic->name_table[hash];
    sexpr *name;
    sexpr *ret;

    protect_ptr(ic->g, (void **) &namehead);

    while(!is_nil(ic, names)) {
        if(car(names)->t == NAME &&
           car(names)->u.name.length == len &&
           !strncasecmp(namestr, car(names)->u.name.head+car(names)->u.name.start, len)) {
            ret = find_or_create_name(ic, car(names), namestr, len);
            unprotect_ptr(ic->g);
            return ret;
        }
        names = cdr(names);
    }

    name = new_name(ic, namehead, namestr, len);

    /* This strange way of making a pair is necessary because 
       ic->name_table can be changed by the garbage collector
       callback.  This way, we make no memory allocations between
       reading and writing ic->name_table. */
      
    sexpr *pair = mk_weak_cons(ic, name, ic->g_nil);
    STORE(ic->g, pair, cdr(pair), ic->name_table[hash]);
    STORE(ic->g, NULL, ic->name_table[hash], pair);

    ret = name;
    unprotect_ptr(ic->g);
    return ret;
}

/* Wrapper function for calling intern_len when we have a null terminated
   C string. */
sexpr *intern(IC *ic, const char *namestr) {
    int len = strlen(namestr);

    return intern_len(ic, NULL, namestr, len);
}


/* Frames and Bindings

   This whole section is based on the approach described in
   "Shallow Binding in LISP 1.5" by Henry G. Baker, Jr.
   https://dspace.mit.edu/bitstream/handle/1721.1/41975/AI_WP_138.pdf
 */


/* mk_bindings builds up its return value destructively.
   ret is the value that will be returned, and tail is always pointing
   to the last pointer in the list.  It points at ret to begin with,
   but points inside of the last cons cell once some bindings have been
   created.

   *paramsp points to the formal arguments to be used in creating
   the bindings.  Each formal may be:
       <word>          normal parameter
       [<word> <expr>] optional parameter - If no value is provided, then
                       <expr> will be evaluated and will provide the value.
       [<word>]        rest parameter - All remaining arguments will be turned
                       into a list and assigned to <word>

   *paramsp is advanced through the formal parameters as bindings are made.
   This is to inform eval() what optional or rest arguments may still need
   to be handled.

   A set of bindings is a list of cons cells where the car of the cell is
   a word and the cdr if the cell is a value.
 */
sexpr *mk_bindings(IC *ic, sexpr **paramsp, sexpr *args, sexpr *ending) {
    sexpr *ret = ic->g_nil;  /* ret will be returned */
    sexpr **tail = &ret;     /* tail will destructively modify either ret
                                or the last cons cell in the list built
                                up so far. */
    sexpr *linking_object = NULL;

    protect_ptr(ic->g, (void **)&ret);

    while((*paramsp)->t == CONS &&
          (is_nil(ic, args) || args->t == CONS)) {
        sexpr *name;

        if(car(*paramsp)->t == CONS) {
            /* We have [<word>] or [<word> <expr>] */
            name = car(car(*paramsp));
        } else {
            /* We have <word> */
            name = car(*paramsp);
        }

        if(car(*paramsp)->t == CONS &&
           is_nil(ic, cdr(car(*paramsp)))) {
            /* We have a rest parameter.  Bind name to the list of the
               rest of the arguments.
               Destructively modify the tail of the return value, and
               update tail to point to the cdr of the new last cons
               cell in the list. */
            sexpr *binding = unsafe_cons(ic, name, args);
            protect_ptr(ic->g, (void **) &binding);
            STORE(ic->g,
                  linking_object,
                  *tail,
                  unsafe_cons(ic, binding, ic->g_nil));
            unprotect_ptr(ic->g);
            linking_object = *tail;
            tail = &(cdr(*tail));
            STORE(ic->g, NULL, *paramsp, cdr(*paramsp));
            /* Set args to nil to terminate argument processing.
               We've bound everything to a rest argument, so there's
               nothing else to do. */
            args = ic->g_nil;
        } else if(args->t == CONS) {
            /* We have a name, and a value to bind to it, so let's do it.
               Same destructive modification of tail, and updating of
               tail to point to the cdr of the new last cons cell in the
               list. */
            sexpr *binding = unsafe_cons(ic, name,  car(args));
            protect_ptr(ic->g, (void **) &binding);
            STORE(ic->g,
                  linking_object,
                  *tail,
                  unsafe_cons(ic, binding, ic->g_nil));
            unprotect_ptr(ic->g);
            linking_object = *tail;
            tail = &(cdr(*tail));
            STORE(ic->g, NULL, *paramsp, cdr(*paramsp));
            args = cdr(args);
        } else {
            break;
        }
    }

    /* Tack on the ending. */
    STORE(ic->g, linking_object, *tail, ending);

    /* This undoes the protect_ptr above. */
    unprotect_ptr(ic->g);
    return ret;
}


/* Standard mark_* and mk_*.  See gc.h for details. */
void mark_frame(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
    frame *f = (frame *) c;
    om(g, (void **) &f->parent);
    om(g, (void **) &f->to_active);
    om(g, (void **) &f->bindings);
    om(g, (void **) &f->procedure);
    om(g, (void **) &f->output_error_info);
    om(g, (void **) &f->continuation);
    om(g, (void **) &f->parent_continuation);
}

frame *mk_frame(IC *ic, frame *parent,
                        sexpr *bindings,
                        sexpr *procedure,
                        sexpr *output_error_info,
                        struct continuation *continuation,
                        int allowed_results,
                        struct continuation *parent_continuation,
                        int parent_allowed_results) {
    protect_ptr(ic->g, (void **) &parent);
    protect_ptr(ic->g, (void **) &bindings);
    protect_ptr(ic->g, (void **) &procedure);
    protect_ptr(ic->g, (void **) &output_error_info);
    protect_ptr(ic->g, (void **) &continuation);
    protect_ptr(ic->g, (void **) &parent_continuation);
    frame *f = (frame *)ic_xmalloc(ic, sizeof(frame), mark_frame);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    f->parent = parent;
    f->to_active = parent;
    f->bindings = bindings;
    f->procedure = procedure;
    f->output_error_info = output_error_info;
    f->continuation = continuation;
    f->allowed_results = allowed_results;
    f->parent_continuation = parent_continuation;
    f->parent_allowed_results = parent_allowed_results;
    return f;
}

/* Swaps the values stored in a frame with the values stored in the
   symbols referenced in the frame.  This same function is used both
   for entering a frame and for leaving it.  It is its own inverse.
 */
void do_swap(IC *ic, frame *f) {
    sexpr *bindings = f->bindings;

    while(!is_nil(ic, bindings)) {
        sexpr *binding = car(bindings);

        sexpr *name = car(binding);
        sexpr *tmp  = cdr(binding);
        STORE(ic->g, binding,
              cdr(binding), name->u.name.symbol->value);
        STORE(ic->g, name->u.name.symbol,
              name->u.name.symbol->value, tmp);
        bindings = cdr(bindings);
    }
}

/* Is the name s in the list l?
   This is tricky because l came from an argument list, so
   the car may be a cons cell if it is an optional or rest argument.
 */
int name_member(IC *ic, sexpr *s, sexpr *l) {
    for(; !is_nil(ic, l) && l->t == CONS; l = cdr(l)) {
        sexpr *elem;
        if(car(l)->t == NAME)
            elem = car(l);
        else if(car(l)->t == CONS)
            elem = car(car(l));
        else
            elem = ic->g_unbound;

        if(name_eq(elem, s))
            return 1;
    }
    return 0;
}

/* remove_bindings() is used by extend during tail calls to generate
   a new list of bindings that does not contain the ones that are about
   to be shadowed.

   It returns a copy of bindings with every name in names removed.

   Attempts to avoid consing by returning the "bindings" argument if
   the first item is not being removed and if the recursive call did
   not remove anything.
 */
sexpr *remove_bindings(IC *ic, sexpr *names, sexpr *bindings) {
    sexpr *subcall_result;

    if(is_nil(ic, bindings))
        return ic->g_nil;
    else if(name_member(ic, car(car(bindings)), names))
        return remove_bindings(ic, names, cdr(bindings));

    subcall_result = remove_bindings(ic, names, cdr(bindings));

    if(subcall_result == cdr(bindings))
        return bindings;
    else
        return cons(ic, car(bindings), subcall_result);
}

/* Extend creates a copy of frame f extended with bindings from formalsp
   and values.  This frame will have the same parent, and the same
   bindings (the very same objects, mutating one will mutate the other)
   except for any that are shadowed by new bindings from formalsp
   and values.
 */
frame *extend(IC *ic, frame *f, sexpr **formalsp, sexpr *values,
              sexpr *procedure, sexpr *output_error_info,
              struct continuation *continuation,
              int allowed_results,
              struct continuation *parent_continuation,
              int parent_allowed_results) {
    sexpr *filtered_bindings = remove_bindings(ic, *formalsp, f->bindings);
    /* This protect_ptr() is necessary because mk_bindings() does not
        protect its arguments. */
    protect_ptr(ic->g, (void **)&filtered_bindings);
    frame *ret = mk_frame(ic,
                          f->parent,
                          mk_bindings(ic,
                                      formalsp,
                                      values,
                                      filtered_bindings),
                          procedure,
                          output_error_info,
                          continuation,
                          allowed_results,
                          parent_continuation,
                          parent_allowed_results);
    unprotect_ptr(ic->g);
    return ret;
}

/* add_local adds a local, unbound binding to the frame f by destructively
   modifying the frame.  All other frame members stay the same. */
void add_local(IC *ic, frame *f, sexpr *name) {

    /* If name is already a local variable in the frame, do nothing. */
    if(name_member(ic, name, f->bindings))
        return;
    
    /* Must reroot to the frame before manipulating it. */
    reroot(ic, f);

    /* Store the current value of name. */
    STORE(ic->g,
          f,
          f->bindings,
          cons(ic, cons(ic, name,
                            name->u.name.symbol->value),
                   f->bindings));
                   
    /* Set the value to unbound. */
    STORE(ic->g,
          name->u.name.symbol,
          name->u.name.symbol->value,
          ic->g_unbound);
}

/* reroot changes the current active frame to frame f.

   It does this by reversing all of the to_active pointers, and then
   walking from the current root to the new root.

   First, bindings are swapped during a trip up the tree to the common
   ancestor.

   The common ancestor bindings are not swapped (both the old and new
   frames are under it, so we wish to remain within it).

   Then, bindings are swapped during the trip down the tree to the new
   root.
 */
void reroot(IC *ic, frame *f) {
    frame *newf = NULL;
    frame *old = f;
    frame *tmp;

    /* First walk, adjust to_active pointers to point to self.
       Frames are taken off of old, adjusted to point to new, and
       stuck at the head of new. */
    while(old != NULL) {
        /* Pop off of old */
        tmp = old;
        old = old->to_active;

        /* Push onto newf */
        tmp->to_active = newf;
        newf = tmp;
    }

    /* Second walk, unwind all frames up to common ancestor.
       We are at the destination if to_active == NULL.
       We are at the common ancestor if to_active != parent. */
    while(newf->to_active != NULL && newf->to_active == newf->parent) {
        do_swap(ic, newf);
        newf = newf->to_active;
    }

    /* Move down to branch to wind.
       The common ancestor does not get a do_swap() because its
       bindings are common to both the old and the new frames.
       If to_active is NULL, then we're already at our new frame
       and we're done, so the loop below will do nothing. */
    newf = newf->to_active;

    /* Wind all frames down to self/new root. */
    while(newf != NULL) {
        do_swap(ic, newf);
        newf = newf->to_active;
    }
}



/* This is called after a garbage collection.
   Because the name list is made with weak_cons cells, any word
   that is not referenced from elsewhere in the program, and has
   no value, procedure, or property list, will be freed during garbage
   collection.

   The car of the weak_cons will be reset to ic->g_nil.

   The cleanup process consists of removing any cons cells whose
   car has been reset to ic->g_nil by the garbage collector
   from the name list.
 */
void cleanup_name_table(GC *g) {
  struct interpreter *ic = (struct interpreter *) g->roots;
  sexpr *current;
  int i;

  for(i = 0; i < NAME_TABLE_HASH_BUCKETS; i++) {
      /* For each hash bucket, we special case the deletion of
         items from the beginning of the list.
         These stores are into the root set, while stores
         later on below are into cons cells. */
      while(!is_nil(ic, ic->name_table[i]) && 
            car(ic->name_table[i]) == ic->g_nil) {
          STORE(g, NULL, ic->name_table[i], cdr(ic->name_table[i]));
      }

      current = ic->name_table[i];
      /* At this point, current is either nil or it is a pointer to
         a cons cell with a non-nil car. */
      while(!is_nil(ic, current)) {
        /* current is a pointer to a cons cell with a non-nil car.
           Unlink any successors with a nil car and then move on. */
        sexpr *next = cdr(current);
        if(!is_nil(ic, next) && car(next) == ic->g_nil) {
            while(!is_nil(ic, next) && car(next) == ic->g_nil)
                next = cdr(next);
            /* next is nil or has a non-nil car. */
            STORE(g, current, cdr(current), next);
            /* cdr(current) is nil or has a non-nil car. */
        }
        current = cdr(current);
        /* current is nil or has a non-nil car. */
      }
      /* current is nil and the name table entry now contains no
         cons cells with a nil car */
  }
}


/* The characters in a name are stored in pointer+length format.
   get_cstring() creates a garbage collected C string from the name
   that is null terminated.
   It is used to create the input to strtod() in to_number().
 */
char *get_cstring(IC *ic, sexpr *e) {
    char *ret;
    protect_ptr(ic->g, (void **) &e);
    if(e->t == NAME) {
        char *cp = (char *)ic_xmalloc(ic, e->u.name.length+1, mark_cstring);
        strncpy(cp, e->u.name.head+e->u.name.start, e->u.name.length);
        cp[e->u.name.length] = '\0';
        ret = cp;
    } else {
        ret = NULL;
    }
    unprotect_ptr(ic->g);
    return ret;
}

/* Convert the argument into a name. */
sexpr *to_name(IC *ic, sexpr *e) {
    int len;
    char *buf;

    switch(e->t) {
        case NAME:
            return e;
        case NUMBER:
            len = snprintf(NULL, 0, "%g", e->u.number.value);
            buf = (char *)ic_xmalloc(ic, len+1, mark_cstring);
            len = snprintf(buf, len+1, "%g", e->u.number.value);
            return intern_len(ic, buf, buf, len);
        default:
            bad_argument(ic, e);
    }
}

/* Convert the argument into a number. */
sexpr *to_number(IC *ic, sexpr *e) {
    char *s, *unparsed;
    double d;

    switch(e->t) {
        case NAME:
            s = get_cstring(ic, e);
            if(strcasecmp(s, "inf") &&
               strcasecmp(s, "infinity") &&
               strcasecmp(s, "nan")) {
              d = strtod(s, &unparsed);
              if(unparsed != s && *unparsed == '\0')
                  return mk_number(ic, d);
            }
            bad_argument(ic, e);
        case NUMBER:
            return e;
        default:
            bad_argument(ic, e);
    }
}

/* Used by the propertly list handling functions below.
   Returns the pointer to the pointer to the portion of the property
   list for "name" starting with "prop".  Returns nil if prop is not on
   the property list.

   linker is an optional void ** points to the beginning of the object
   that the return value points into.  Only needed if the object will
   be modified, like in remprop().
 */

static sexpr **findprop(IC *ic, sexpr *name, sexpr *prop, void **linker) {
    void *linking_object = to_name(ic, name)->u.name.symbol;
    sexpr **pl = &((symbol *)linking_object)->properties;

    while(!is_nil(ic, *pl)) {
        if(car(*pl) == prop ||
           (name_eq(ic->n_caseignoredp->u.name.symbol->value, ic->n_true) &&
            name_eq(car(*pl), prop))) {
            if(linker != NULL)
                *linker = linking_object;
            return pl;
        }
        /* Property list items are in pairs, so we skip over two
           members here.  *pl is the first cons of the pair.
           cdr(*pl) is the second cons of the pair, and contains
           the pointer to the next pair (or nil).
           This pointer is located at &(cdr(cdr(*pl))), the 
           address of the cdr pointer within cdr(*pl). */
        linking_object = cdr(*pl);
        pl = &(cdr(cdr(*pl)));
    }

    if(linker != NULL)
        *linker = linking_object;
    return pl;
}

void pprop(IC *ic, sexpr *name, sexpr *prop, sexpr *value) {
    sexpr **pl_tail;

    /* Make sure that name and prop are NAME's or can be
       turned into NAME's (numbers). */
    protect_ptr(ic->g, (void **) &name);
    protect_ptr(ic->g, (void **) &prop);
    name = to_name(ic, name);
    prop = to_name(ic, prop);

    pl_tail = findprop(ic, name, prop, NULL);
    if(is_nil(ic, *pl_tail)) {
        /* prop is not on the plist for name.  Add it at the
           beginning. */
        STORE(ic->g, name->u.name.symbol,
              name->u.name.symbol->properties,
              cons(ic, prop, cons(ic, value, name->u.name.symbol->properties)));
    } else {
        /* prop is on the plist for name.  Destructively modify
           the cons cell after the current one to point to value. */
        STORE(ic->g, cdr(*pl_tail), car(cdr(*pl_tail)), value);
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

sexpr *gprop(IC *ic, sexpr *name, sexpr *prop) {
    sexpr **pl_tail;
    /* Make sure that name and prop are NAME's or can be
       turned into NAME's (numbers). */
    protect_ptr(ic->g, (void **) &name);
    protect_ptr(ic->g, (void **) &prop);
    name = to_name(ic, name);
    prop = to_name(ic, prop);

    pl_tail = findprop(ic, name, prop, NULL);
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
    if(is_nil(ic, *pl_tail))
        return ic->g_nil;
    else
        return car(cdr(*pl_tail));
}

void remprop(IC *ic, sexpr *name, sexpr *prop) {
    void *linker;
    sexpr **pl_tail;
    /* Make sure that name and prop are NAME's or can be
       turned into NAME's (numbers). */
    protect_ptr(ic->g, (void **) &name);
    protect_ptr(ic->g, (void **) &prop);
    name = to_name(ic, name);
    prop = to_name(ic, prop);

    pl_tail = findprop(ic, name, prop, &linker);
    if(is_nil(ic, *pl_tail)) {
        eprint_sexpr(ic, name);
        eprintf(ic, " has no property named ");
        eprint_sexpr(ic, prop);
        throw_error(ic, ic->continuation->line);
    } else {
        /* Need two cdr's because we are removing two items from the
           property list. */
        STORE(ic->g, linker, *pl_tail, cdr(cdr(*pl_tail)));
    }
    unprotect_ptr(ic->g);
    unprotect_ptr(ic->g);
}

/* Helper function for returning a copy of the plist.
   Because we return a copy here, it is safe to mutate
   above because the only way the plist is ever accessed is through
   the pointer inside the symbol.
 */
static sexpr *plist_helper(IC *ic, sexpr *l) {
    if(is_nil(ic, l))
        return ic->g_nil;
    else
        return cons(ic, car(l), plist_helper(ic, cdr(l)));
}

sexpr *plist(IC *ic, sexpr *name) {
    sexpr *ret = NULL;
    protect_ptr(ic->g, (void **) &name);
    name = to_name(ic, name);

    ret = plist_helper(ic, name->u.name.symbol->properties);
    unprotect_ptr(ic->g);
    return ret;
}




/* Procedure given to the garbage collector to mark "struct symbol" objects */
void mark_symbol(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
  struct symbol *s = (struct symbol *) c;
  om(g, (void **) &s->value);
  om(g, (void **) &s->function);
  om(g, (void **) &s->function_source);
  om(g, (void **) &s->properties);
  om(g, (void **) &s->canonical_name);
  om(g, (void **) &s->names);
}



/* Create a struct symbol object. */
struct symbol *mk_symbol(IC *ic,
                        struct sexpr *value,
                        struct sexpr *function,
                        struct sexpr *function_source,
                        struct sexpr *properties,
                        struct sexpr *canonical_name,
                        struct sexpr *names,
                        int flags) {
  struct symbol *ret;
  protect_ptr(ic->g, (void **) &value);
  protect_ptr(ic->g, (void **) &function);
  protect_ptr(ic->g, (void **) &properties);
  protect_ptr(ic->g, (void **) &canonical_name);
  protect_ptr(ic->g, (void **) &names);
  ret = (struct symbol *)ic_xmalloc(ic, sizeof(struct symbol), mark_symbol);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  unprotect_ptr(ic->g);
  ret->value = value;
  ret->function = function;
  ret->function_source = function_source;
  ret->properties = properties;
  ret->canonical_name = canonical_name;
  ret->names = names;
  ret->flags = flags;
  return ret;
}

/* Marking and creating ARRAY objects.
   Arrays can have an arbitrary number of children.
   We make no effort to do this incrementally.  Just mark them
   all at once.

   Arrays are allocated with one more pointer than is needed to hold
   the items.  The extra pointer contains NULL so the marking
   function knows when to stop marking children.
 */
static void mark_array_members(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {
    sexpr **members = (sexpr **) c;

    for( ; *members; members++)
        om(g, (void **) members);
}

/* Create an ARRAY object. */
sexpr *array(IC *ic, int length, int origin) {
    int i;
    sexpr **members;

    members = (sexpr **) ic_xmalloc(ic, (length+1)*sizeof(sexpr *), mark_array_members);

    for(i = 0; i < length; i++)
        members[i] = ic->g_nil;
    members[length] = NULL;

    return mk_array(ic, members, length, origin);
}


/* list_length() supports list_to_array() */
static int list_length(IC *ic, sexpr *l) {
    int i = 0;
    for( ; !is_nil(ic, l); l = cdr(l))
        i++;
    return i;
}

/* Convert a list to an array.  Used by LISTTOARRAY and by the reader
   to process { ... } syntax. */
sexpr *list_to_array(IC *ic, sexpr *l, int origin) {
    int i;
    int length = list_length(ic, l);
    sexpr *a = array(ic, length, origin);

    for(i = 0; !is_nil(ic, l); i++, l = cdr(l))
        STORE(ic->g, a->u.array.members,
                     a->u.array.members[i],
                     car(l));

    return a;
}

/* Is e a number or a name that can be converted into a number? */
int numberp(IC *ic, sexpr *e) {
    char *str, *unparsed;

    switch(e->t) {
        case NAME:
            str = get_cstring(ic, e);
            if(strcasecmp(str, "inf") &&
               strcasecmp(str, "infinity") &&
               strcasecmp(str, "nan")) {
              strtod(str, &unparsed);
              if(unparsed != str && *unparsed == '\0')
                  return 1;
            }
            return 0;
        case NUMBER:
            return 1;
        default:
            return 0;
    }
}

