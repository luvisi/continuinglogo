
/*
    Portable C Garbage Collector
    
    Copyright (C) 2014 Andru Luvisi

    Licensed under the Apache License, Version 2.0.
    For details see COPYING.Garbage.Collector.txt or
    http://www.apache.org/licenses/LICENSE-2.0
 */

#include "gc.h"
#include "list_memory.h"
#include <stdio.h>

/* I recommend reading gc.h before reading this file.  The comments
   in this code assume the reader is familiar with the concepts explained
   in gc.h. - Andru Luvisi */

/* Weak pointers can be forgotten or cleared.

   Forgotten means we forget they exist, empty the stored list of weak
   pointers.  This happens before the sweep phase.

   Cleared means they are modified to point to the default value set
   with set_gc_default_value().  This happens after the sweep phase.
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
void forget_weak_pointers(GC *g);
void register_weak_pointer(GC *g, void **p);
void collect_some_garbage(GC *g);
void collect_increment(GC *g);
void mark_roots(GC *g);

GC *create_gc(pointer_iterator rm,
              void *roots,
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
    g->protect_count = 0;
    g->protect_ptr_count = 0;
    return g;
}

void enable_gc(GC *g) {
    g->garbage_collection_enabled = 1;
}

void disable_gc(GC *g) {
    g->garbage_collection_enabled = 0;
}

void set_gc_default_value(GC *g, void *d) {
    g->default_value = d;
}

void set_gc_post_collection_callback(GC *g, void (*callback)(GC *)) {
    g->post_collection_callback = callback;
}

void set_gc_work_per_alloc(GC *g, int count) {
    g->work_per_alloc = count;
}

void set_gc_mode(GC *g, enum gc_mode mode) {
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

void free_gc(GC *g) {
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

void *allocate(GC *g, size_t length, pointer_iterator iterator) {
    struct node *n;

    collect_some_garbage(g);

    n = malloc(length + sizeof(struct node));
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
void mark_roots(GC *g) {
    int i;

    g->mark_roots(g, g->roots, mark_object, register_weak_pointer);

    for(i = 0; i < g->protect_count; i++)
        mark_object(g, g->protect_stack[i]);

    for(i = 0; i < g->protect_ptr_count; i++)
        mark_object(g, *(g->protect_ptr_stack[i]));
}

/* Called at the end of sweeping/beginning of marking.
   This does not modify the pointers.  It causes the garbage collector
   to forget any weak pointers that it knows about. */
void forget_weak_pointers(GC *g) {
    struct weak_pointer_block *wpb = g->weak_pointers;
    while(wpb != NULL) {
        wpb->count = 0;
        wpb = wpb->next;
    }
}

/* This adds a weak pointer to the list of weak pointers to process
   at the end of the mark phase. */
void register_weak_pointer(GC *g, void **p) {
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

    wpb->pointers[wpb->count++] = p;
}

/* Called during the mark phase.  The mark stack must be non-empty
   before calling mark_one().  It pops one object off the mark stack,
   marks it BLACK, and calls its callback to mark its children.

   Called from mark() during stop the world collection and from 
   collect_increment() during incremental collection.
 */
void mark_one(GC *g) {
    struct node *n = g->mark_stack;
    g->mark_stack = n->stack;
    n->mark = MARK_BLACK;
    n->iterator(g, n+1, mark_object, register_weak_pointer);
}

/* mark() performs the entire mark phase during stop the world collection. */
void mark(GC *g) {
    while(g->mark_stack != NULL)
        mark_one(g);
}

/* clear_weak_pointers() is called at the end of the mark phase.
   Any weak pointers that point to unmarked objects (WHITE objects that will
   be collected during the sweep phase) are modified to point to the
   default value set by set_gc_default_value().
 */
void clear_weak_pointers(GC *g) {
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
   Called from collect_increment in incremental mode.
   WHITE nodes are freed.
   BLACK nodes are marked WHITE and kept.
   GRAY nodes should not exist during the sweep phase.
 */
void sweep_one(GC *g) {
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
void sweep(GC *g) {
    g->sweep_pointer = &(g->nodes);
    while(*(g->sweep_pointer) != NULL) {
        sweep_one(g);
    }
}


/* Perform a complete garbage collection.  Called automatically in
   stop the world mode.  Can be called manually by the client
   in either mode.  If collect_garbage() is called in incremental mode,
   it finishes the current collection and runs another complete collection,
   to make sure that a complete collection has occured at the time of the
   call.
 */
void collect_garbage(GC *g) {
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
            while(g->state == GC_MARKING)
                collect_increment(g);
            while(g->state == GC_SWEEPING)
                collect_increment(g);
            while(g->state == GC_MARKING)
                collect_increment(g);
            while(g->state == GC_SWEEPING)
                collect_increment(g);
            break;
    }
}

/* Incremental collector. */
/* Must only be called if g->mode == INCREMENTAL */
void collect_increment(GC *g) {
    switch(g->state) {
        case GC_MARKING:
            if(g->mark_stack == NULL) {
                clear_weak_pointers(g);
                g->sweep_pointer = &(g->nodes);
                g->state = GC_SWEEPING;
            } else {
                mark_one(g);
            }
            break;
        case GC_SWEEPING:
            if(*(g->sweep_pointer) == NULL) {
                if(g->post_collection_callback != NULL)
                    g->post_collection_callback(g);
                forget_weak_pointers(g);
                g->state = GC_MARKING;
                mark_roots(g);
            } else {
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
void collect_some_garbage(GC *g) {
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


/* The function called by the STORE macro.

   The invariant that must be maintained is that during the marking phase,
   there must never be a pointer from a BLACK node to a WHITE node.
   When the assignment would cause this to happen, the node being stored
   is marked.

   No invariant needs to be maintained during the sweep phase.  All nodes
   will be white at the end for the beginning of the next mark phase.
 */
/*
void store(GC *g, void *obj, void **field, void *pointer) {
    if(pointer == NULL) return;

    switch(g->mode) {
        case INCREMENTAL:
            switch(g->state) {
                case GC_MARKING:
                    if(obj == NULL ||
                       (((struct node *)obj - 1)->mark == MARK_BLACK &&
                        ((struct node *)pointer - 1)->mark == MARK_WHITE)) {
                        mark_object(g, pointer);
                    }
                    break;
                case GC_SWEEPING:
                    break;
            }
            *field = pointer;
            break;

        case STOP_THE_WORLD:
            *field = pointer;
            break;

        default:
            fprintf(stderr, "Unsupported GC mode: %d\n", g->mode);
            exit(EXIT_FAILURE);
    }
}
*/


/* See PROTECTION in gc.h for a description of how these functions are
   used by the client program.

   See mark_roots() above to see how the protection stacks are used
   by the garbage collector.
 */
void *protect(GC *g, void *o) {
    if(g->protect_count == PROTECT_MAX) {
        fprintf(stderr, "Exceeded maximum protection count\n");
        exit(EXIT_FAILURE);
    }

    STORE(g, NULL, g->protect_stack[g->protect_count++], o);
    return o;
}

void unprotect(GC *g) {
    g->protect_count--;
}

void *protect_ptr(GC *g, void **o) {
    if(g->protect_ptr_count == PROTECT_MAX) {
        fprintf(stderr, "Exceeded maximum pointer protection count\n");
        exit(EXIT_FAILURE);
    }

    g->protect_ptr_stack[g->protect_ptr_count++] = o;
    return o;
}

void unprotect_ptr(GC *g) {
    g->protect_ptr_count--;
}

