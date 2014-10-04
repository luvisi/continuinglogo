
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

#ifndef GLOBAL_ENVIRONMENT_H
#define GLOBAL_ENVIRONMENT_H

#include "interpreter.h"
#include "gc.h"
#include "list_memory.h"

void initialize_global_environment(IC *ic);
sexpr *first(IC *ic, sexpr *e);
sexpr *butfirst(IC *ic, sexpr *e);


#endif
