
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

#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include "ttymodes.h"

/* termios structures.  Must be initialized by tty_init() while the
   terminal is in cooked mode. */
static struct termios cooked_termios, cbreak_termios;

/* Track the current state so we don't flush input if we are not
   changing state. */
static enum { STATE_CBREAK, STATE_COOKED } tty_state;

/* Initialize the structures and state above. */
int tty_init(int fd) {
    if(!isatty(fd))
        return 0;

    if (tcgetattr(fd, &cooked_termios) < 0)
        return(-1);

    cbreak_termios = cooked_termios;

    /* echo off, canonical mode off */
    cbreak_termios.c_lflag &= ~(ECHO | ICANON);

    cbreak_termios.c_cc[VMIN] = 1;	/* 1 byte at a time, no timer */
    cbreak_termios.c_cc[VTIME] = 0;

    tty_state = STATE_COOKED;

    return 0;
}

/* Change to cbreak mode if input is a terminal and we are not currently
   in cbreak mode. */
int tty_cbreak(int fd) {
    if(!isatty(fd) ||
       tty_state == STATE_CBREAK)
        return 0;

    if (tcsetattr(fd, TCSAFLUSH, &cbreak_termios) < 0)
        return(-1);

    tty_state = STATE_CBREAK;
    return(0);
}

/* Change to cooked mode if input is a terminal and we are not currently
   in cooked mode. */
int tty_cooked(int fd) {
    if(!isatty(fd) ||
       tty_state == STATE_COOKED)
        return 0;

    if (tcsetattr(fd, TCSAFLUSH, &cooked_termios) < 0)
        return(-1);

    tty_state = STATE_COOKED;
    return(0);
}

/* Used to set and clear flags.
   Currently only used by tty_nonblocking and tty_blocking.
 */
static int tty_flags(int fd, int mask, int set) {
    int flags;

    if((flags = fcntl(fd, F_GETFL, 0)) < 0)
        return -1;
    flags = (flags & mask) | set;
    if(fcntl(fd, F_SETFL, flags) < 0)
        return -1;
    return 0;
}

/* Set nonblocking mode. */
int tty_nonblocking(int fd) { return tty_flags(fd, ~0, O_NONBLOCK); }

/* Set blocking mode. */
int tty_blocking(int fd)    { return tty_flags(fd, ~O_NONBLOCK, 0); }
