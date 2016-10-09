
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

/* A byte_buffer is a buffer that can have bytes appended to it
   and will expand as needed.

   The contents of a byte_buffer can be converted to a word.
   New memory will be allocated, the new word will not share any
   memory with the byte_buffer.

   A byte buffer can be cleared and reused.
 */

#ifndef BYTE_BUFFER_H
#define BYTE_BUFFER_H

#include "pcgc.h"
#include "list_memory.h"

#define BYTE_BUFFER_INCREMENT 2048

typedef struct byte_buffer {
    char *buffer;
    int size;
    int used;
} byte_buffer;

/* Create a byte_buffer */
byte_buffer *mk_byte_buffer(IC *ic);

/* Adds ch to b, growing the buffer in b if necessary. */
void add_to_byte_buffer(IC *ic, byte_buffer *b, char ch);

/* Creates a word containing the contents of byte_buffer b. */
struct sexpr *word_from_byte_buffer(IC *ic, byte_buffer *b);

/* Empties byte_buffer b. */
void clear_byte_buffer(byte_buffer *b);

#endif
