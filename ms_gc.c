
/*
    Portable C Garbage Collector
    
    Copyright (C) 2014 Andru Luvisi

    Licensed under the Apache License, Version 2.0.
    For details see COPYING.Garbage.Collector.txt or
    http://www.apache.org/licenses/LICENSE-2.0
 */

#include "pcgc.h"
#include <stdio.h>

/* I recommend reading pcgc.h before reading this file.  The comments
   in this code assume the reader is familiar with the concepts explained
   in pcgc.h. - Andru Luvisi */

/* Weak pointers can be forgotten or cleared.

   Forgotten means we forget they exist, empty the stored list of weak
   pointers.  This happens before the mark phase.  The list is
   then filled in during the mark phase.

   Cleared means they are modified to point to the default value set
   with set_gc_default_value().  This happens after the mark phase,
   when we know both of the following:
        * Where all of the weak pointers are.
        * Whether or not every object they point to was marked or not.
 */

/* In incremental mode, there are two states, GC_MARKING and GC_SWEEPING.

   The roots must be marked and any children placed on the mark stack, and
   the weak pointers must be forgotten, before incremental work can begin
   in the GC_MARKING state.  Marking is over when the mark stack is empty.

   The weak pointers must be cleared and g->sweep_pointer must be set
   before incremental work can begin in the GC_SWEEPING state.  Sweeping
   is over when g->sweep_pointer is NULL.

   Because these preconditions need to be established, the code to setup
   for sweeping occurs in the handler for marking, and the code to setup
   for marking occurs in the handler for sweeping.
 */
   
   


/* Forward declarations. */
static void forget_weak_pointers(GC *g);
static void collect_some_garbage(GC *g);
static void collect_increment(GC *g);
static void mark_roots(GC *g);
static void register_weak_pointer(GC *g, void **p);
static void mark_object(GC *g, void **p);

static void ms_enable_gc(GC *g) {
    g->garbage_collection_enabled = 1;
}

static void ms_disable_gc(GC *g) {
    g->garbage_collection_enabled = 0;
}

static void ms_set_gc_default_value(GC *g, void *d) {
    g->default_value = d;
}

static void ms_set_gc_post_collection_callback(GC *g, void (*callback)(GC *)) {
    g->post_collection_callback = callback;
}

static void ms_set_gc_work_per_alloc(GC *g, int count) {
    g->work_per_alloc = count;
}

static void ms_set_gc_mode(GC *g, enum gc_mode mode) {
    switch(mode) {
        case STOP_THE_WORLD:
            fprintf(stderr, "Ending incremental garbage collection not supported.\n");
            exit(EXIT_FAILURE);
            break;
        case INCREMENTAL:
            g->mode = INCREMENTAL;
            g->state = GC_MARKING;
            forget_weak_pointers(g);
            mark_roots(g);
            break;
    }
}

static void ms_free_gc(GC *g) {
    struct node *n = g->nodes;
    struct weak_pointer_block *wpb = g->weak_pointers;

    while(n != NULL) {
        struct node *tmp = n->next;
        free(n);
        n = tmp;
    }

    while(wpb != NULL) {
        struct weak_pointer_block *tmp = wpb->next;
        free(wpb);
        wpb = tmp;
    }
    
    free(g);
}

static void *ms_allocate(GC *g, size_t length, pointer_iterator iterator) {
    struct node *n;

    collect_some_garbage(g);

    n = (node *) malloc(length + sizeof(struct node));
    if(n == NULL) return NULL;
    n->iterator = iterator;
    n->next = g->nodes;
    g->nodes = n;
    g->node_count++;
    n->stack = NULL;
    switch(g->mode) {
        case STOP_THE_WORLD:
            /* In STOP_THE_WORLD mode, the garbage collector is never
               in progress during allocation, so all nodes are allocated
               WHITE, meaning unmarked. */
            n->mark = MARK_WHITE;
            break;
        case INCREMENTAL:
            switch(g->state) {
                case GC_MARKING:
                    /* During the marking phase, new nodes are allocated
                       WHITE in the hopes that nodes who are not long lived
                       may die before the end of this collection.  If a
                       pointer to the node is STORE'd into a BLACK node,
                       then store() will mark it.  If it is STORE'd into
                       a GRAY node, then it will get marked in the normal
                       course of processing the mark stack. */
                    n->mark = MARK_WHITE;
                    break;
                case GC_SWEEPING:
                    /* If we are in the sweep phase, the node must be WHITE
                       if the sweeper will not visit it again, and must
                       be BLACK if the sweeper will visit it.  Because new
                       nodes are placed at the head of the nodes list, the
                       only way the sweeper may visit the new node is if
                       it hasn't processed any nodes yet, in which case
                       the sweep_pointer still points to g->nodes in the 
                       garbage collection context, and not into any allocated
                       nodes.
                       Therefore, if the sweep_pointer equals &(g->nodes)
                       then we need to allocate BLACK so the sweeper will
                       keep the node, and otherwise we need to allocate WHITE
                       so the node will start out unmarked during the next
                       mark phase. */
                    if(g->sweep_pointer == &(g->nodes)) {
                        n->mark = MARK_BLACK;
                    } else {
                        n->mark = MARK_WHITE;
                    }
                    break;
            }
            break;
    }
    return (void *)(n + 1);
}

/* Mark the root set, all protected objects, and all objects pointed
   to by protected pointers. */
static void mark_roots(GC *g) {
    int i;

    g->mark_roots(g, g->roots, mark_object, register_weak_pointer);

    for(i = 0; i < g->protect_ptr_count; i++)
        mark_object(g, g->protect_ptr_stack[i]);
}

/* Called at the end of sweeping/beginning of marking.
   This does not modify the pointers.  It causes the garbage collector
   to forget any weak pointers that it knows about so the marking
   phase can populate them anew. */
static void forget_weak_pointers(GC *g) {
    struct weak_pointer_block *wpb = g->weak_pointers;
    while(wpb != NULL) {
        wpb->count = 0;
        wpb = wpb->next;
    }
}

/* Called during the mark phase.  The mark stack must be non-empty
   before calling mark_one().  It pops one object off the mark stack,
   marks it BLACK, and calls its callback to mark its children.

   Called from mark() during stop the world collection and from 
   collect_increment() during incremental collection.
 */
static void mark_one(GC *g) {
    struct node *n = g->mark_stack;
    g->mark_stack = n->stack;
    n->mark = MARK_BLACK;
    if(n->iterator != NULL)
        n->iterator(g, n+1, mark_object, register_weak_pointer);
}

/* mark() performs the entire mark phase during stop the world collection. */
static void mark(GC *g) {
    while(g->mark_stack != NULL)
        mark_one(g);
}

/* clear_weak_pointers() is called at the end of the mark phase.
   Any weak pointers that point to unmarked objects (WHITE objects that will
   be collected during the sweep phase) are modified to point to the
   default value set by set_gc_default_value().
 */
static void clear_weak_pointers(GC *g) {
    struct weak_pointer_block *wpb = g->weak_pointers;
    int i;

    while(wpb != NULL) {
        for(i = 0; i < wpb->count; i++) {
            void **p = wpb->pointers[i];
            if(*p != NULL) {
                struct node *n = ((struct node *) *p) - 1;
                if(n->mark == MARK_WHITE) {
                    *p = g->default_value;
                }
            }
        }
        wpb->count = 0;
        wpb = wpb->next;
    }
}

/* Process one node in the node list during the sweep phase.
   Called from sweep() in stop the world mode.
   Called from collect_increment() in incremental mode.
   WHITE nodes are freed.
   BLACK nodes are marked WHITE and kept.
   GRAY nodes should not exist during the sweep phase.
 */
static void sweep_one(GC *g) {
    struct node *n;

    switch((*(g->sweep_pointer))->mark) {
        case MARK_WHITE:
            n = (*(g->sweep_pointer))->next;
            free(*(g->sweep_pointer));
            *(g->sweep_pointer) = n;
            g->node_count--;
            break;
        case MARK_BLACK:
            (*(g->sweep_pointer))->mark = MARK_WHITE;
            g->sweep_pointer = &((*(g->sweep_pointer))->next);
            break;
        case MARK_GRAY:
            fprintf(stderr, "Gray node found during gc sweep!\n");
            exit(EXIT_FAILURE);
            break;
        default:
            fprintf(stderr, "Invalid mark value found!\n");
            exit(EXIT_FAILURE);
            break;
    }
}

/* Called in stop the world mode.  Performs the entire sweep phase. */
static void sweep(GC *g) {
    g->sweep_pointer = &(g->nodes);
    while(*(g->sweep_pointer) != NULL) {
        sweep_one(g);
    }
}


/* Perform a complete garbage collection.  Called automatically in
   stop the world mode.  Can be called manually by the client
   in either mode.
   
   If collect_garbage() is called in incremental mode,
   it finishes the current collection and runs another complete collection,
   to make sure that a complete collection has occured during the call.

   In particular, the user should be guaranteed that anything unreachable
   has been collected when this procedure finishes.  However, during an
   incremental collection, a node could be marked, and then become 
   unreachable before the collection ends.  If we just ran to the end
   of the current collection, then that node would not be reclaimed.
   This is why we do the extra work of finishing the current collection
   and performing a whole new collection if the client explicitly asks
   for a collection.
 */
static void ms_collect_garbage(GC *g) {
    switch(g->mode) {
        case STOP_THE_WORLD:
            forget_weak_pointers(g);
            mark_roots(g);
            mark(g);
            clear_weak_pointers(g);
            sweep(g);
            if(g->post_collection_callback != NULL)
                g->post_collection_callback(g);
            break;
        case INCREMENTAL:
            /* Finish the current pass.
               If we are sweeping then the first loop is a NOOP. */
            while(g->state == GC_MARKING)
                collect_increment(g);
            while(g->state == GC_SWEEPING)
                collect_increment(g);

            /* Perform a whole new pass */
            while(g->state == GC_MARKING)
                collect_increment(g);
            while(g->state == GC_SWEEPING)
                collect_increment(g);
            break;
    }
}

/* Incremental collector. */
/* Must only be called if g->mode == INCREMENTAL */
static void collect_increment(GC *g) {
    switch(g->state) {
        case GC_MARKING:
            if(g->mark_stack == NULL) {
                /* If the mark stack is empty, then there are no more
                   GRAY nodes left and we are done marking.
                   Clear the weak pointers, setup for sweeping, and
                   switch states. */
                clear_weak_pointers(g);
                g->sweep_pointer = &(g->nodes);
                g->state = GC_SWEEPING;
                /* printf("Switching to sweeping\n"); */
            } else {
                /* If the mark stack is not empty, then we have something
                   GRAY that we can mark as BLACK.  If it has any WHITE
                   children, then they will be marked GRAY and pushed
                   onto the mark stack. */
                mark_one(g);
            }
            break;
        case GC_SWEEPING:
            if(*(g->sweep_pointer) == NULL) {
                /* If the pointer pointed to by g->sweep_pointer is NULL,
                   then we have processed the entire linked list of all
                   nodes, and we are done sweeping.
                   Call the post_collection_callback to let the client
                   do any bookkeeping, such as handling weak pointers
                   that were cleared, and switch states to MARKING. */
                if(g->post_collection_callback != NULL)
                    g->post_collection_callback(g);
                forget_weak_pointers(g);
                g->state = GC_MARKING;
                /* printf("Switching to marking\n"); */
                mark_roots(g);
            } else {
                /* We are not done with the linked list of all nodes,
                   so we can handle the next one. */
                sweep_one(g);
            }
            break;
    }
}


/* Called from allocate().  This is the main entry point for automatic
   garbage collection.
   In stop the world mode, it counts down to the next complete collection,
   and runs it if it is time.
   In incremental mode, it calls collect_increment() a few times to
   get some work done.
 */
static void collect_some_garbage(GC *g) {
    int i;

    if(!g->garbage_collection_enabled)
        return;

    switch(g->mode) {
        case STOP_THE_WORLD:
            if(g->allocations_left > 0)
                g->allocations_left--;

            if(g->allocations_left == 0) {
                collect_garbage(g);
                g->allocations_left = g->allocations_per_collection;
            }
            break;

        case INCREMENTAL:
            for(i = 0; i < g->work_per_alloc; i++)
                collect_increment(g);
            break;
            
        default:
            fprintf(stderr, "Unsupported GC mode: %d\n", g->mode);
            exit(EXIT_FAILURE);
    } 
}


/* mark_object marks an object during the mark phase.

   It is called directly from some other procedures in this file,
   and is passed to the client's node iterator procedure as a callback.

   We do nothing with NULL pointers.
   We do nothing with objects marked BLACK or GRAY.  They are
   already marked.  GRAY objects are on the mark stack and will be
   processed in due time by mark_one().
   WHITE objects get marked GRAY and placed on the mark stack for further
   processing.
 */
static void mark_object(GC *g, void **p) {
    void *obj = *p;
    if(obj == NULL) return;
    struct node *n = ((struct node *) obj) - 1;
    if(n->mark == MARK_WHITE) {
        n->mark = MARK_GRAY;
        n->stack = g->mark_stack;
        g->mark_stack = n;
    }
}

/* This adds a weak pointer to the list of weak pointers to process
   at the end of the mark phase.
   It is only called by the client as a callback. */
static void register_weak_pointer(GC *g, void **p) {
    struct weak_pointer_block *wpb = g->weak_pointers;

    /* Skip over any full weak pointer blocks */
    while(wpb->count >= WEAK_POINTER_BLOCK_SIZE &&
          wpb->next != NULL)
        wpb = wpb->next;

    /* Allocate a new weak pointer block if necessary */
    if(wpb->count >= WEAK_POINTER_BLOCK_SIZE &&
       wpb->next == NULL) {
        wpb->next = (struct weak_pointer_block *)
                         malloc(sizeof(struct weak_pointer_block));
        if(g->weak_pointers == NULL) {
            fprintf(stderr, "Out of memory!\n");
            exit(EXIT_FAILURE);
        }
        wpb = wpb->next;
        wpb->next = NULL;
        wpb->count = 0;
    }

    /* At this point, wpb is guaranteed to point to a weak pointer block
       that contains space for a new weak pointer. */
    wpb->pointers[wpb->count++] = p;
}

/* The function called by the STORE macro.

   The invariant that must be maintained is that during the marking phase,
   there must never be a pointer from a BLACK node to a WHITE node.
   When the assignment would cause this to happen, the node being stored
   is marked.

   No invariant needs to be maintained during the sweep phase.  All nodes
   will be white at the end for the beginning of the next mark phase.

   If obj == NULL then we are storing into the root set or a protected
   pointer, and we need to mark the object immediately.

   if pointer == NULL then we can't break the invariant so there's nothing
   else to do.
 */
static void ms_store(GC *g, void *obj, void **field, void *pointer) {
    *field = pointer;

    if(pointer == NULL) return;

    switch(g->mode) {
        case INCREMENTAL:
            switch(g->state) {
                case GC_MARKING:
                    if(obj == NULL ||
                       (((struct node *)obj - 1)->mark == MARK_BLACK &&
                        ((struct node *)pointer - 1)->mark == MARK_WHITE)) {
                        mark_object(g, (void **) &pointer);
                    }
                    break;
                case GC_SWEEPING:
                    break;
            }
            break;

        case STOP_THE_WORLD:
            break;

        default:
            fprintf(stderr, "Unsupported GC mode: %d\n", g->mode);
            exit(EXIT_FAILURE);
    }
}

static void ms_protect_ptr(GC *g, void **o) {
    void mark_object(GC *g, void **p);

    if(g->protect_ptr_count == PROTECT_MAX) {
        fprintf(stderr, "Exceeded maximum pointer protection count\n");
        exit(EXIT_FAILURE);
    }

    /* If we are marking in incremental mode, then protecting a
       pointer is equivalent to temporarily adding it to the root
       set, so anything to which it points must be marked immediately. */
    if(g->mode == INCREMENTAL && g->state == GC_MARKING)
        mark_object(g, o);

    g->protect_ptr_stack[g->protect_ptr_count++] = o;
}

static void ms_unprotect_ptr(GC *g) {
    g->protect_ptr_count--;
}

static void do_nothing() {}


GC *create_ms_gc(pointer_iterator rm,
                 void *roots,
                 unsigned int root_size,
                 unsigned int allocations_per_collection) {
    GC *g = (GC *) malloc(sizeof(GC));
    if(g == NULL) return NULL;

    g->weak_pointers = (struct weak_pointer_block *) 
                         malloc(sizeof(struct weak_pointer_block));
    if(g->weak_pointers == NULL) {
        free(g);
        return NULL;
    }

    g->nodes = NULL;
    g->node_count = 0;
    g->mark_stack = NULL;
    g->mark_roots = rm;
    g->roots = roots;
    g->allocations_per_collection = allocations_per_collection;
    g->allocations_left = allocations_per_collection;
    g->weak_pointers->count = 0;
    g->weak_pointers->next = NULL;
    g->garbage_collection_enabled = 0;
    g->default_value = NULL;
    g->post_collection_callback = NULL;
    g->mode = STOP_THE_WORLD;
    g->work_per_alloc = 6;
    g->protect_ptr_count = 0;

    g->thread_start = do_nothing;
    g->thread_end = do_nothing;

    g->enable_gc = ms_enable_gc;
    g->disable_gc = ms_disable_gc;
    g->free_gc = ms_free_gc;
    g->gc_allocate = ms_allocate;
    g->collect_garbage = ms_collect_garbage;
    g->set_gc_default_value = ms_set_gc_default_value;
    g->set_gc_post_collection_callback = ms_set_gc_post_collection_callback;
    g->set_gc_mode = ms_set_gc_mode;
    g->set_gc_work_per_alloc = ms_set_gc_work_per_alloc;
    g->store = ms_store;
    g->protect_ptr = ms_protect_ptr;
    g->unprotect_ptr = ms_unprotect_ptr;

    return g;
}

