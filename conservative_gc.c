
/*
    Portable C Garbage Collector
    
    Copyright (C) 2014 Andru Luvisi

    Licensed under the Apache License, Version 2.0.
    For details see COPYING.Garbage.Collector.txt or
    http://www.apache.org/licenses/LICENSE-2.0
 */

/* EXPERIMENTAL! */
/* This is an interface to the Bohm conservative collector.
   It is mostly a proof of concept for supporting multiple collectors.
   In particular, this collector will never collect any strings in
   ContinuingLogo due to the lack of support for weak pointers. */

#include "pcgc.h"

#include <pthread.h>
#define GC_THREADS
#include <gc.h>

/* No support or need for a conservative collector to wait to begin
   collecting. */
static void conservative_enable_gc(GC *g) { }
static void conservative_disable_gc(GC *g) { }

/* No weak pointers, so no way to set a default value. */
static void conservative_set_gc_default_value(GC *g, void *d) { }

/* Nothing will be reset to a default value, so no point calling a
   post collection callback to handle cleared pointers. */
static void conservative_set_gc_post_collection_callback(GC *g, void (*callback)(GC *))
{ }

/* Not meaningful. */
static void conservative_set_gc_work_per_alloc(GC *g, int count) { }

/* Only one mode. */
static void conservative_set_gc_mode(GC *g, enum gc_mode mode) { }

static void conservative_free_gc(GC *g) { }

/* Straight pass through. */
static void *conservative_allocate(GC *g, size_t length, pointer_iterator iterator) {
    return GC_MALLOC(length);
}

/* No direct control. */
static void conservative_collect_garbage(GC *g) { }

static void conservative_store(GC *g, void *obj, void **field, void *pointer) {
    *field = pointer;
}

/* All pointers are protected automatically.  Nothing to do here. */
static void conservative_protect_ptr(GC *g, void **o) { }
static void conservative_unprotect_ptr(GC *g) { }

/* At the start of a thread other than the main thread, we need to
   register the new thread with the collector. */
static void conservative_thread_start() {
    struct GC_stack_base sb;

    if(GC_get_stack_base(&sb) != GC_SUCCESS) {
        fprintf(stderr, "Failure getting stack base for interpreter thread.\n");
        exit(EXIT_FAILURE);
    }

    if(GC_register_my_thread(&sb) != GC_SUCCESS) {
        fprintf(stderr, "Failure registering interpreter thread.\n");
        exit(EXIT_FAILURE);
    }
}

/* At the end of a thread other than the main thread, we need to unregister
   the thread with the garbage collector. */
static void conservative_thread_end() {
    GC_unregister_my_thread();
}

GC *create_conservative_gc(pointer_iterator rm,
                 void *roots,
                 unsigned int root_size,
                 unsigned int allocations_per_collection) {

    GC_INIT();

    /* We need to tell the collector about the root set because it was
       not allocated by the garbage collector. */
    GC_add_roots(roots, ((char *)roots)+root_size);

    /* Inform the collector that we will be registering additional threads. */
    GC_allow_register_threads();

    GC *g = (GC *) GC_MALLOC(sizeof(GC));
    if(g == NULL) return NULL;

    g->protect_ptr_count = 0;

	g->thread_start = conservative_thread_start;
	g->thread_end = conservative_thread_end;

    g->enable_gc = conservative_enable_gc;
    g->disable_gc = conservative_disable_gc;
    g->free_gc = conservative_free_gc;
    g->gc_allocate = conservative_allocate;
    g->collect_garbage = conservative_collect_garbage;
    g->set_gc_default_value = conservative_set_gc_default_value;
    g->set_gc_post_collection_callback = conservative_set_gc_post_collection_callback;
    g->set_gc_mode = conservative_set_gc_mode;
    g->set_gc_work_per_alloc = conservative_set_gc_work_per_alloc;
    g->store = conservative_store;
    g->protect_ptr = conservative_protect_ptr;
    g->unprotect_ptr = conservative_unprotect_ptr;

    return g;
}

