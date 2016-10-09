
/*
    Portable C Garbage Collector
    
    Copyright (C) 2014 Andru Luvisi

    Licensed under the Apache License, Version 2.0.
    For details see COPYING.Garbage.Collector.txt or
    http://www.apache.org/licenses/LICENSE-2.0
 */

#include "pcgc.h"
#include "list_memory.h"
#include <stdio.h>

/* I recommend reading pcgc.h before reading this file.  The comments
   in this code assume the reader is familiar with the concepts explained
   in pcgc.h. - Andru Luvisi */

/* Weak pointers can be cleared if the object they point to is dead.

   Cleared means they are modified to point to the default value set
   with set_gc_default_value().  This happens after the mark phase.
 */

/* Forward declarations. */
static void copy_roots(GC *g);
static void register_weak_pointer(GC *g, void **p);
static void copy_object(GC *g, void **p);
static void copying_collect_garbage(GC *g);


/* A copying collector can't really be disabled.  Once a semispace is full,
   collection is mandatory.  We just have to hope the client can get
   things into a consistent starting state before the first collection. */
static void copying_enable_gc(GC *g) { }
static void copying_disable_gc(GC *g) { }

/* Value to which weak pointers get reset if they point to objects
   that will be reclaimed. */
static void copying_set_gc_default_value(GC *g, void *d) {
    g->default_value = d;
}

/* Set client function to call after collection. */
static void copying_set_gc_post_collection_callback(GC *g, void (*callback)(GC *)) {
    g->post_collection_callback = callback;
}

/* Collection is not incremental, so this is meaningess. */
static void copying_set_gc_work_per_alloc(GC *g, int count) { }

/* There is only one mode, stop and copy. */
static void copying_set_gc_mode(GC *g, enum gc_mode mode) { }

static void copying_free_gc(GC *g) {
    struct weak_pointer_block *wpb = g->weak_pointers;

    while(wpb != NULL) {
        struct weak_pointer_block *tmp = wpb->next;
        free(wpb);
        wpb = tmp;
    }
    
    free(g->active_semispace);
    free(g->inactive_semispace);
    free(g);
}

static void *copying_allocate(GC *g, size_t length, pointer_iterator iterator) {
    struct node *n;
    length += sizeof(node);

    //printf("Allocating node, %u/%u\n", g->active_semispace_used, g->active_semispace_size);

    /* We try twice before giving up since we may end up growing
       during the second try. */
    if(g->active_semispace_used + length > g->active_semispace_size)
        copying_collect_garbage(g);
    if(g->active_semispace_used + length > g->active_semispace_size)
        copying_collect_garbage(g);
    if(g->active_semispace_used + length > g->active_semispace_size) {
        fprintf(stderr, "Out of memory in copying_allocate!\n");
        exit(EXIT_FAILURE);
    }

    n = (node *) (g->active_semispace + g->active_semispace_used);
    g->active_semispace_used += length;

    n->size = length;
    n->iterator = iterator;
    n->mark = MARK_ACTIVE;

    return (void *)(n + 1);
}

/* Copy the root set, all protected objects, and all objects pointed
   to by protected pointers.
   Copy the default_value (and adjust the pointer) so that it will
   have a meaningful value when we clear weak pointers at the end. */
static void copy_roots(GC *g) {
    int i;

    copy_object(g, &g->default_value);
    g->mark_roots(g, g->roots, copy_object, register_weak_pointer);

    for(i = 0; i < g->protect_ptr_count; i++)
        copy_object(g, g->protect_ptr_stack[i]);
}

/* clear_weak_pointers() is called at the end of copying.
   Any weak pointers that point to copied objects will be
   updated.
   All others will be cleared to the default value set by
   set_gc_default_value().
 */
static void clear_weak_pointers(GC *g) {
    struct weak_pointer_block *wpb = g->weak_pointers;
    int i;

    while(wpb != NULL) {
        for(i = 0; i < wpb->count; i++) {
            void **p = wpb->pointers[i];
            if(*p != NULL) {
                struct node *n = ((struct node *) *p) - 1;
                if(n->mark == MARK_ACTIVE) {
                    *p = g->default_value;
                } else if(n->mark == MARK_REDIRECT) {
                    *p = n->next+1;
                } else {
                    fprintf(stderr, "Bad value of n->mark in clear_weak_pointers: %d\n", n->mark);
                    exit(EXIT_FAILURE);
                }
            }
        }
        wpb->count = 0;
        wpb = wpb->next;
    }
}

static void copying_collect_garbage(GC *g) {
    char *temp_semispace;
    node *n;
    unsigned int temp_size, temp_used;

    fprintf(stderr, "collecting garbage\n");

    /* To collect garbage, we swap the active and inactive semispaces
       and copy everything to the active semispace. */


    /* Swap semispaces */
    temp_semispace = g->active_semispace;
    g->active_semispace = g->inactive_semispace;
    g->inactive_semispace = temp_semispace;

    temp_size = g->active_semispace_size;
    g->active_semispace_size = g->inactive_semispace_size;
    g->inactive_semispace_size = temp_size;

    temp_used = g->active_semispace_used;
    g->active_semispace_used = g->inactive_semispace_used;
    g->inactive_semispace_used = temp_used;

    /* Clear active semispace.  I know there's some redundancy here.
       I'm deliberately trying not to be clever to avoid mistakes. */
    g->active_semispace_used = 0;

    /* Grow active semispace if needed. */
    if(g->active_semispace_size < g->desired_semispace_size) {
        free(g->active_semispace);
        g->active_semispace = (char *)malloc(g->desired_semispace_size);
        if(g->active_semispace == NULL) {
            fprintf(stderr, "Out of memory growing semispace!\n");
            exit(EXIT_FAILURE);
        }
        g->active_semispace_size = g->desired_semispace_size;
    }


    /* Start by adjusting the root pointers and copying the objects to
       which they point */
    copy_roots(g);

    /* n will sweep through the active semispace, fixing pointers and
       copying things to which they point until there are no unhandled
       objects in the active semispace */
    n = (node *)g->active_semispace;
    while((char *)n < g->active_semispace + g->active_semispace_used) {
        n->iterator(g, n+1, copy_object, register_weak_pointer);
        n = (node *)(((char *)n) + n->size);
    }

    if((((double)g->active_semispace_size - (double)g->active_semispace_used)/
       (double)g->active_semispace_size) < g->minimum_reclaimed)
        g->desired_semispace_size *= 2;

    clear_weak_pointers(g);
    if(g->post_collection_callback != NULL)
        g->post_collection_callback(g);
    fprintf(stderr, "collected garbage\n");
}

/* copy_object copies an object from the inactive semispace to the
   active semispace. */
static void copy_object(GC *g, void **p) {
    void *obj = *p;
    struct node *n;
    if(obj == NULL) return;
    n = ((node *)obj) - 1;
    if((char *)n < g->inactive_semispace ||
       ((char *)n) + n->size >
       g->inactive_semispace + g->inactive_semispace_used) {
        fprintf(stderr, "Invalid pointer found during copying collection\n");
        exit(EXIT_FAILURE);
    }

    if(n->mark == MARK_ACTIVE) {
        /* The object has not been copied.  Copy it and turn it into a
           redirect. */
        char *newaddr = g->active_semispace + g->active_semispace_used;
        g->active_semispace_used += n->size;
        memcpy(newaddr, n, n->size);
        n->mark = MARK_REDIRECT;
        n->next = (node *)newaddr;
        *p = (void *) (n->next+1);
    } else if(n->mark == MARK_REDIRECT) {
        /* The object pointed to has already been copied, leaving
           a redirection behind.  Adjust the pointer to follow the redirect. */
        *p = (void *) (n->next+1);
    } else {
        fprintf(stderr, "Bad value of n->mark in copy_object(): %d\n", n->mark);
        exit(EXIT_FAILURE);
    }

}

/* This adds a weak pointer to the list of weak pointers to process
   at the end of the mark phase. */
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

/* The function called by the STORE macro. */
static void copying_store(GC *g, void *obj, void **field, void *pointer) {
    *field = pointer;
}

static void *copying_protect_ptr(GC *g, void **o) {
    if(g->protect_ptr_count == PROTECT_MAX) {
        fprintf(stderr, "Exceeded maximum pointer protection count\n");
        exit(EXIT_FAILURE);
    }

    g->protect_ptr_stack[g->protect_ptr_count++] = o;
    return o;
}

static void copying_unprotect_ptr(GC *g) {
    g->protect_ptr_count--;
}

static void do_nothing() {}


GC *create_copying_gc(pointer_iterator rm,
                      void *roots,
                      unsigned int root_size,
                      unsigned int allocations_per_collection) {
    GC *g = (GC *) malloc(sizeof(GC));
    if(g == NULL) return NULL;

    g->weak_pointers = (struct weak_pointer_block *) 
                         malloc(sizeof(struct weak_pointer_block));
    if(g->weak_pointers == NULL)
        goto weak_pointers_failed;

    g->weak_pointers->count = 0;
    g->weak_pointers->next = NULL;

    g->node_count = 0;
    g->mark_roots = rm;
    g->roots = roots;
    g->default_value = NULL;
    g->post_collection_callback = NULL;

    g->protect_ptr_count = 0;

    /* Semispace management */

    g->minimum_reclaimed = 0.75;
    g->desired_semispace_size = 
    g->active_semispace_size =
    g->inactive_semispace_size = 16*1024*1024; /* 16 MB */

    g->active_semispace_used =
    g->inactive_semispace_used = 0;

    g->active_semispace = (char *)malloc(g->active_semispace_size);
    if(g->active_semispace == NULL)
        goto active_semispace_failed;

    g->inactive_semispace = (char *)malloc(g->inactive_semispace_size);
    if(g->inactive_semispace == NULL)
        goto inactive_semispace_failed;
    /* End semispace management */

    g->thread_start = do_nothing;
    g->thread_end = do_nothing;

    g->enable_gc = copying_enable_gc;
    g->disable_gc = copying_disable_gc;
    g->free_gc = copying_free_gc;
    g->gc_allocate = copying_allocate;
    g->collect_garbage = copying_collect_garbage;
    g->set_gc_default_value = copying_set_gc_default_value;
    g->set_gc_post_collection_callback = copying_set_gc_post_collection_callback;
    g->set_gc_mode = copying_set_gc_mode;
    g->set_gc_work_per_alloc = copying_set_gc_work_per_alloc;
    g->store = copying_store;
    g->protect_ptr = copying_protect_ptr;
    g->unprotect_ptr = copying_unprotect_ptr;

    printf("Made gc\n");
    return g;

    inactive_semispace_failed:
        free(g->active_semispace);
    active_semispace_failed:
        free(g->weak_pointers);
    weak_pointers_failed:
        free(g);
        return NULL;
}

