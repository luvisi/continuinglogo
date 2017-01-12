
/*
    Portable C Garbage Collector
    
    Copyright (C) 2014 Andru Luvisi

    Licensed under the Apache License, Version 2.0.
    For details see COPYING.Garbage.Collector.txt or
    http://www.apache.org/licenses/LICENSE-2.0
 */

/* The Portable C Garbage Collector by Andru Luvisi, 2014

   GENERAL REMARKS

   This garbage collector is designed to know nothing about the program
   using it or the objects being allocated.  Pointers within an object
   are marked by calling a callback function that was provided when the
   object was allocated.

   This collector is:
     Mark/Sweep - It works by marking all reachable objects and then freeing
                  all unmarked objects.
     Non-moving - Objects are never moved by the collector.
     Tracing    - It does not use reference counting.  It will collect loops.
     Precise    - It is not conservative.  It will free all unreachable
                  objects.
     Portable   - It is written in mostly standard C.
     Generic    - It knows nothing about the program that is using it or the
                  data structures being allocated.
     Incremental (optional) - It can have limited pause times by performing
                              small amounts of garbage collection every time
                              an object is allocated.

   TYPES OF FUNCTION POINTERS

   There are three special types of functions used in callbacks.

   A pointer_iterator is passed to the garbage collector when the
   garbage collection context is created, and whenever memory is
   allocated.  It is called during the tracing phase of the collector
   with four arguments:
       The garbage collection context
       The object being traced
       An object_marker, a function to use to mark child objects
       A weak_pointer_registerer, a function used to register weak
       pointers for the collector to clear at the end of the tracing
       phase if the target has not been marked and will be collected.

   CREATING A GARBAGE COLLECTION CONTEXT

   Typical usage involves creating a root set and a procedure for marking
   the members of the root set, followed by the creation of the actual
   garbage collection context.  For example, the ContinuingLogo interpreter
   uses an interpreter context, ic, as the root set.  It contains a
   function for marking the root set (it looks slightly different from
   this in the code due to the use of macros, but here's the idea).

     void mark_interpreter(GC *g, void *c,
                           object_marker om, weak_pointer_registerer wpr) {
         IC *ic = (IC *) c;

         om(ic->g, &ic->field1);
         om(ic->g, &ic->field1);
         ...
     }

   Then a garbage collection context is created.  create_ms_gc takes,
   as arguments, the function for marking the root set, a pointer to
   the root set, the length of the root set, and a delay.  The delay
   is a parameter for the interpreter when in stop the world mode.
   It is how many allocations to perform before doing a stop the world
   garbage collection.  It is not relevant in incremental mode.

   This creates a mark sweep garbage collector.

     GC *g = create_ms_gc(mark_interpreter, ic, sizeof(*ic), gc_delay);
     if(g == NULL) {
         fprintf(stderr, "Out of memory!\n");
         exit(EXIT_FAILURE);
     }

   The newly created garbage collection context can be used to allocate
   memory, but will not start collecting garbage until it is enabled.
   This gives the client a change to get the root set into a consistent
   state before garbage collection begins.

   ALLOCATING OBJECTS

   To allocate an object, the allocate() function must be passed the
   garbage collection context, the number of bytes to allocate, and an
   object_marker for marking the object's children during tracing.  For
   example, to allocate a simplified cons node (the ones in ContinuingLogo
   are far more complex than this), one might create the following:

     void mark_cons(GC *g, void *c,
                    object_marker om, weak_pointer_registerer wpr) {
         struct cons *cn = (struct cons *) c;

         om(g, &cn->car);
         om(g, &cn->cdr);
     }

   Then, to do the actual allocation:

     struct cons *cn = allocate(g, sizeof(struct cons), mark_cons);

   It is often helpful to create a function for creating each type of
   object.  For example:

     struct cons* cons(GC *g, struct cons *car, struct cons *cdr) {
         protect_ptr(g, (void **) &car);  <----  See PROTECTION below
         protect_ptr(g, (void **) &cdr);
         struct cons *cn = allocate(g, sizeof(struct cons), mark_cons);
         unprotect_ptr(g);
         unprotect_ptr(g);
         cn->car = car;
         cn->cdr = cdr;
         return cn;
     }

   To create a cons node where the car pointer is a weak pointer, you
   create a different marking function and pass it to allocate:

     void mark_weak_cons(GC *g, void *c,
                         object_marker om, weak_pointer_registerer wpr) {
         struct cons *cn = (struct cons *) c;

         wpr(g, (void **) &cn->car);
         om(g, (void **) &cn->cdr);
     }

   When a pointer_iterator is called, every pointer within the object
   must be either marked or registered as a weak pointer.  Weak pointers
   will be cleared to a default value at the end of the tracing phase
   if the objects to which they point were not marked through some other
   path.  Whether to treat a pointer as a weak pointer can vary depending
   on the contents of the object being iterated, the object being pointed
   to, or any other parameter.

   ENABLING THE GARBAGE COLLECTOR

   Once the initial set of objects has been allocated and stored into the
   root set, garbage collection can be enabled with:

     enable_gc(g);

   SETTING CONFIGURATION PARAMETERS

   In incremental mode, the number of nodes marked or swept during
   every allocation can be set with:

     set_gc_work_per_alloc(g, work_per_allocation);


   The value to which weak pointers are reset when their target is
   collected can be set with:

     set_gc_default_value(g, default_value);

   The garbage collector can call a callback after collection is completed.
   This can be used by the client to clean up objects whose weak pointers
   have been cleared:

     set_gc_post_collection_callback(g, cleanup_name_table);

   PROTECTION

   Pointers (generally arguments to functions or local variables within
   functions) can be temporarily protected from garbage collection by:

     protect_ptr(g, &object_pointer);

   object_pointer must point to a valid object or to NULL when it is
   protected.

   The most recently protected pointer is unprotected by calling:

     unprotect_ptr(g);

   In ContinuingLogo, I have adopted the following conventions:
     * mk_* functions protect their arguments before calling the
       allocator.

     * Other functions assume that arguments passed to them are
       protected.

     * unsafe_mk_* functions DO NOT protect their arguments before
       calling the allocator.  It is the client's responsibility to ensure
       that arguments passed to unsafe_mk_* functions are protected.
       These exist primarily for performance reasons.  mk_* calls that
       occur very frequently can sometimes be replaced with unsafe_mk_*
       calls, but by default, mk_* functions are used for allocation.
       I have only replaced mk_* calls with unsafe_mk_* calls after
       profiling with gprof and gcov and careful examination of the code.

     * All functions assume that values returned from other functions
       are not protected.

   In C all arguments to a function are evaluated before the function
   is called.  Therefore, if two arguments to a function both involve
   allocating memory, then one of them must be protected.

   For example, this has a garbage collection error:
       *** WRONG! DO NOT USE! ***
       struct cons *c = cons(g, cons(g, a, b), cons(g, c, d));
       *** WRONG! DO NOT USE! ***

   If cons(g, a, b) is called first, then the cell allocated could
   potentially be freed during the call to cons(g, c, d).  This is true
   even though the outer cons will protect its arguments, because both
   arguments will be evaluated before the outer call is made.  A similar
   problem exists if cons(g, c, d) is evaluated first.

   To fix this, we force the evaluation order and protect one of the
   arguments.

   struct cons *ab = cons(g, a, b);
   protect_ptr(g, (void **) &ab)
   struct cons *c = cons(g, ab, cons(g, c, d));
   unprotect_ptr(g);

   INCREMENTAL MODE

   The garbage collector can operate in incremental mode.  In incremental
   mode it does some marking or sweeping every time an object is allocated.
   It is switched into incremental mode by the call:

     set_gc_mode(ic->g, INCREMENTAL);

   THE STORE() WRITE BARRIER

   When operating in incremental mode, it is critical that the client not
   modify any pointers to allocated objects without notifying the garbage
   collector, or else the garbage collector may free memory that is still
   reachable.

   To avoid this happening, every time a pointer to an allocated object
   is stored into another allocated object (except at initial creation),
   into the root set, or into a protected pointer, the macro STORE()
   must be used.  STORE takes four arguments:
       The garbage collection context.
       The object which is being modified if an object is being modified
         or NULL if:
             The root set is being modified.
             A protected function argument or local variable is being modified.
         IF AN OBJECT IS BEING MODIFIED, THEN THAT OBJECT MUST BE
         PASSED!!!
       The full expression for the location being modified.
       The value being stored.

   For example, to modify one of the simplified cons cells from above,
   one might say:

     STORE(g, c, c->cdr, cons(g, foo, nil));

   To modify part of the root set, one might say:

     STORE(g, NULL, ic->field1, value1);
   
   FINALIZATION

   To free the garbage collector context and all allocated objects, call

     gc_free(g);

 */

#ifndef PCGC_H
#define PCGC_H

#include <stdio.h>
#include <stdlib.h>

/* The size of each block containing weak pointers. */
#define WEAK_POINTER_BLOCK_SIZE 1024

/* The maximum number of pointers to objects that can
   be protected at once. */
#define PROTECT_MAX 4096


/* Colors from the tri-color abstraction.
   WHITE means unmarked.
   GRAY means marked, but children have not yet been marked.
   BLACK means marked, and children have also been marked.
 */
#define MARK_WHITE 0
#define MARK_GRAY 1
#define MARK_BLACK 2

/* EXPERIMENTAL!  For copying collector. */
/* We overload the "mark" property in copying mode to indicate whether
   a node is active or is redirecting to the new location in the new
   semispace. */
#define MARK_ACTIVE 0
#define MARK_REDIRECT 1

/* The two supported modes for garbage collection.
   STOP_THE_WORLD means a complete garbage collection happens all
                  at once from time to time.
   INCREMENTAL    means that some work (marking or sweeping) occurs
                  during every allocation.
 */
enum gc_mode { STOP_THE_WORLD, INCREMENTAL };

/* state only matters when in incremental mode.
   GC_MARKING  means we are marking.
   GC_SWEEPING means we are sweeping.
 */
enum gc_state { GC_MARKING, GC_SWEEPING };

/* A linked list of weak_pointer_block's is used to keep track of
   the weak pointers that are registered during the mark phase.
   At the end of the mark phase, any weak pointers that point to unmarked
   objects will be cleared to a default value before the unmarked
   objects are freed in the sweep phase.
 */
struct weak_pointer_block {
    void **pointers[WEAK_POINTER_BLOCK_SIZE];
    int count;
    struct weak_pointer_block *next;
};

/* A function of type pointer_iterator is passed to allocate every
   time an object is allocated.  It is called on that object during
   the mark phase.  Every pointer to a child object must either be
   marked with the object_marker or registered as a weak pointer with
   the weak_pointer_registerer. */
typedef void (*object_marker)(struct gc_context *g, void **p);
typedef void (*weak_pointer_registerer)(struct gc_context *g, void **p);
typedef void (*pointer_iterator)(struct gc_context *,
                                 void *,
                                 object_marker,
                                 weak_pointer_registerer);

/* The Garbage Collector Context */
struct gc_context {
    struct node *nodes; /* Linked list of all currently allocated objects */
    int node_count;     /* The number of nodes currently allocated. */
    struct node *mark_stack; /* Contains all GRAY objects, objects that are
                                marked but whose children have not yet been
                                marked. */

    /* The callback for marking the root set */
    void (*mark_roots)(struct gc_context *,
                       void *,
                       void (*)(struct gc_context *, void **),
                       void (*)(struct gc_context *, void **));
    void *roots; /* Pointer to the root set */
    unsigned int allocations_per_collection; /* How often to perform a complete
                                                garbage collection when
                                                in STOP_THE_WORLD mode. */
    unsigned int allocations_left; /* How many more allocations to perform
                                      before performing a complete garbage
                                      collection when in STOP_THE_WORLD mode.
                                    */

    unsigned int garbage_collection_enabled; /* Flag to indicate whether
                                                garbage collection is enabled.
                                                Initially this is disabled to
                                                give the client a chance to get
                                                the root set into a consistent
                                                state. */

    struct weak_pointer_block *weak_pointers; /* Head of the linked list
                                                 of weak pointer blocks. */

    void *default_value; /* Value to which weak pointers revert when target
                            is garbage collected. */

    /* Called after garbage collection.  Gives the client a chance to
       clean up any objects whose weak pointers got cleared. */
    void (*post_collection_callback)(struct gc_context *);

    enum gc_mode mode;   /* Either STOP_THE_WORLD or INCREMENTAL */
    enum gc_state state; /* Used during incremental collection.
                            Either GC_MARKING or GC_SWEEPING. */
    
    struct node **sweep_pointer; /* Used during sweep stage of collection. */

    int work_per_alloc; /* Amount of incremental work to perform per
                           allocation when mode is INCREMENTAL. */


    /* Stack of temporarily protected object pointers. */
    int protect_ptr_count;
    void **protect_ptr_stack[PROTECT_MAX];

    /* START EXPERIMENTAL! For copying collector. */
    /* State data used by the copying collector */

    /* How large we want semispaces to be.  At the beginning of
       collection, we will grow the semispace to which we are copying
       to this size if it is smaller. */
    unsigned int desired_semispace_size; 

    /* Minimum fraction of the semispace that we want reclaimed
       during a collection.  If less than this is reclaimed, we
       increase desired_semispace_size. */
    double minimum_reclaimed;

    /* The semispace in which we are currently allocating nodes */
    char *active_semispace;
    unsigned int active_semispace_size, active_semispace_used;

    /* The inactive semispace */
    char *inactive_semispace;
    unsigned int inactive_semispace_size, inactive_semispace_used;
    
    /* END EXPERIMENTAL COPYING STUFF */

    /* Called at the start and end of threads other than the main
       thread. */
    void (*thread_start)();
    void (*thread_end)();

    /* Called by macros described in comments at the top of this file. */
    /* These are called through function pointers to allow multiple garbage
       collectors to coexist. */
    void (*enable_gc)(struct gc_context *g);
    void (*disable_gc)(struct gc_context *g);
    void (*free_gc)(struct gc_context *g);
    void *(*gc_allocate)(struct gc_context *g, size_t length, pointer_iterator iterator);
    void (*collect_garbage)(struct gc_context *g);
    void (*set_gc_default_value)(struct gc_context *g, void *d);
    void (*set_gc_post_collection_callback)(struct gc_context *g, void (*)(struct gc_context *));
    void (*set_gc_mode)(struct gc_context *g, enum gc_mode mode);
    void (*set_gc_work_per_alloc)(struct gc_context *g, int count);
    void (*store)(struct gc_context *g, void *obj, void **field, void *pointer);
    void (*protect_ptr)(struct gc_context *g, void **o);
    void (*unprotect_ptr)(struct gc_context *g);
};

typedef struct gc_context GC;

/* Every allocated object gets a struct node tacked onto the head
   of it for use by the garbage collector. */
struct node {
    /* For iterating over the pointers in this node */
    pointer_iterator iterator; 

    struct node *next;      /* The linked list of all nodes, for sweeping */
    struct node *stack;     /* The linked list stack for marking          */
    unsigned char mark;     /* MARK_WHITE, MARK_GRAY, or MARK_BLACK       */

    /* To be used for generational collection.  Not yet implemented. */
#if 0
    struct node *oldyoung;  /* The linked list of nodes containing pointers to
                               other nodes in a younger generation. */
    unsigned char generation;       /* This node's current generation. */
    unsigned char collections_left; /* The number of collections left before
                                       this node will be promoted to the next
                                       generation. */
#endif

    /* Used by the copying collector to advance from the beginning of
       one node to the beginning of the next while copying.
       Includes the size of the "struct node" header. */
    size_t size;
};


/* See the comments at the top of this file for an explanation of how
   to create a garbage collector using these functions. */

/* Crete a mark/sweep garbage collector. */
GC *create_ms_gc(pointer_iterator rm,
                 void *roots,
                 unsigned int root_size,
                 unsigned int allocations_per_collection);

/* Crete a conservative garbage collector. */
GC *create_conservative_gc(pointer_iterator rm,
                           void *roots,
                           unsigned int root_size,
                           unsigned int allocations_per_collection);

/* Crete a copying garbage collector. */
GC *create_copying_gc(pointer_iterator rm,
                      void *roots,
                      unsigned int root_size,
                      unsigned int allocations_per_collection);

/* This type, gc_creator, can be used for passing one of the above
   garbage collector creation functions as an argument or storing them
   in a data structure. */
typedef GC *(*gc_creator)(pointer_iterator, void *, unsigned int, unsigned int);

/* Macros to call functions through funcion pointers in a gc_context. */
#define enable_gc(g) (g->enable_gc(g))
#define disable_gc(g) (g->disable_gc(g))
#define free_gc(g) (g->free_gc(g))
#define gc_allocate(g, length, iterator) (g->gc_allocate((g), (length), (iterator)))
#define collect_garbage(g) (g->collect_garbage(g))
#define set_gc_default_value(g, d) (g->set_gc_default_value((g), (d)))
#define set_gc_post_collection_callback(g, cb) (g->set_gc_post_collection_callback((g), (cb)))
#define set_gc_mode(g, mode) (g->set_gc_mode((g), (mode)))
#define set_gc_work_per_alloc(g, count) (g->set_gc_work_per_alloc((g), (count)))

/* Macro for storing a new pointer into an object.  Not to be used when
   initializing the field to begin with during creation, only when
   modifying.
 */

/*
#define STORE(gc, obj, field, pointer) \
    (gc->store((gc), (obj), (void **)&(field), (pointer)))

#define protect_ptr(g, o) (g->protect_ptr((g), (o)))
#define unprotect_ptr(g) (g->unprotect_ptr((g)))
*/

#define STORE(gc, obj, field, pointer) \
    (*(void **)(&field) = (void *)(pointer))
#define protect_ptr(g, o) ((g)->protect_ptr_stack[(g)->protect_ptr_count++] = (o))
#define unprotect_ptr(g) ((g)->protect_ptr_count--)


#endif
