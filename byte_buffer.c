
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

#include <string.h>

#include "gc.h"
#include "list_memory.h"
#include "byte_buffer.h"

/* Function for marking the buffer during garbage collection. */
void mark_byte_buffer(GC *g, void *c,
                      object_marker om, weak_pointer_registerer wpr) {
    byte_buffer *b = (byte_buffer *) c;
    om(g, b->buffer);
}


/* Creates a byte_buffer. */
byte_buffer *mk_byte_buffer(IC *ic) {
    byte_buffer *b = ic_xmalloc(ic, sizeof(byte_buffer), mark_byte_buffer);
    b->buffer = ic_xmalloc(ic, BYTE_BUFFER_INCREMENT, mark_cstring);
    b->size = BYTE_BUFFER_INCREMENT;
    b->used = 0;
    return b;
}

/* Adds ch to b, growing the buffer if needed.
   Because the old buffer is garbage collected, we can just forget
   about it once we have copied the contents into the new buffer. */
void add_to_byte_buffer(IC *ic, byte_buffer *b, char ch) {
    if(b->used >= b->size) {
        int new_size = b->size + BYTE_BUFFER_INCREMENT;
        char *tmp = ic_xmalloc(ic, new_size, mark_cstring);
        strncpy(tmp, b->buffer, b->used);
        b->buffer = tmp;
        b->size = new_size;
    }
    b->buffer[b->used++] = ch;
}

/* Convert the contents of the byte_buffer into a word. */
sexpr *word_from_byte_buffer(IC *ic, byte_buffer *b) {
    return intern_len(ic, NULL, b->buffer, b->used);
}

/* Clear the contents of the byte_buffer. */
void clear_byte_buffer(byte_buffer *b) {
    b->used = 0;
}

