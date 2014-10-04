
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

#ifndef TTYMODES_H
#define TTYMODES_H
#include <termios.h>
#include <unistd.h>

/* Initialize the static tty structures. */
int tty_init(int fd);

/* Set the terminal to cbreak or cooked mode.
   Do nothing if the mode is not changing.
   Flush input if the mode is changing.
 */
int tty_cbreak(int fd);
int tty_cooked(int fd);

/* Set the terminal to nonblocking or blocking mode. */
int tty_nonblocking(int fd);
int tty_blocking(int fd);


#endif
