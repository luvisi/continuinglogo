#!/usr/bin/perl

#    ContinuingLogo Logo Interpreter 
#    
#    Copyright (C) 2014 Andru Luvisi
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


# This perl script generates the following two files:
#   structures.h
#       Declarations for two of the main data structures used in the
#       interpreter.  They are both tagged unions.
#           struct sexpr
#           struct continuation
#       Declarations for the mark_* and mk_* functions used to create
#       each type of sexpr and each type of continuation.
#      
#   structures.c
#       Definitions for the mark_* and mk_* functions used to create
#       each type of sexpr and each type of continuation.
#
#
# I recommend reading just the definitions below for $sexpr_type and
# $continuation_type, and then looking over structures.h and structures.c
# to get a feel for what this script does.
#
# A data type is represented by:
#     [ type, name, mark, ...]
#     "..." is subparts for unions and structs.
#     Each subpart is represented as above.
#
# The mark field indicates whether the field is to be marked during
# garbage collection.  Most pointer fields are always marked.
# "int" fields are not marked.


#use Data::Dumper;
use strict;
use warnings;


sub part_decl { $_[0]->[0] }  # Fetch the type declaration of a part
sub part_name { $_[0]->[1] }  # Fetch the name of a part
sub is_marked { $_[0]->[2] }  # Fetch the flag for whether to mark the part

# Fetch the list of subparts from a part
sub subparts  { @{$_[0]}[3..$#{$_[0]}] } 


# sexprs, or Symbolic Expressions, are the objects upon which Logo programs
# operate.  Numbers, words, lists, etc. are all sexprs, and are all
# represented using this structure which is a tagged union.
my $sexpr_type =
[ 'struct sexpr', '', 1,
  # The only member contained by every type of sexpr is the type tag
  # itself, 't'
  [ 'enum sexpr_type', 't', 0 ],

  # The union 'u' contains all of the actual data in an sexpr.
  [ 'union', 'u', 1, 
    # CONS cells have a car and a cdr, just like in Lisp.
    [ 'struct', 'cons', 1,
      [ 'struct sexpr *', 'car', 1 ],
      [ 'struct sexpr *', 'cdr', 1 ]],

    # The EMPTY_LIST contains no data.
    [ 'struct', 'empty_list', 0 ],

    # SUBR's are primitives implemented in C.
    # They have a name (for printing) and a pointer to the C function
    # implementing the SUBR.
    [ 'struct', 'subr', 1,
      [ 'struct sexpr *', 'name', 1 ],
      [ 'efun', 'func', 1 ]],

    # FSUBR's are like SUBR's, but their arguments are not evaluated.
    # They are called with the raw unevaluated list structures of
    # their arguments.  Currently the only FSUBR is QUOTE.
    # FSUBR's cannot evaluate their arguments in ContinuingLogo because
    # eval() cannot be called recursively in order to keep continuations
    # working.
    [ 'struct', 'fsubr', 1,
      [ 'struct sexpr *', 'name', 1 ],
      [ 'efun', 'func', 1 ]],

    # PROC's wrap primitive procedures and user defined procedures with
    # a record indicating the minimum, default, and maximum number of
    # arguments the procedure accepts.  This information is used by
    # treeify().
    [ 'struct', 'proc', 1,
      [ 'struct sexpr *', 'proc', 1 ],
      [ 'int', 'minargs', 0 ],
      [ 'int', 'defargs', 0 ],
      [ 'int', 'maxargs', 0 ]],

    # FUNARG's contain the environment frame in which to evaluate a
    # procedure.  They are produced by FUNCTION.
    [ 'struct', 'funarg', 1,
      [ 'struct sexpr *', 'lfun', 1 ],
      [ 'struct frame *', 'frame', 1 ]],

    # MACRO's are used for syntax extension.  'macro_type' may point to
    # either:
    #    ic->n_lisp_macro for Lisp style macros, where arguments are not
    #                     evaluated.
    #    ic->n_logo_macro for Logo style macros, where arguments are
    #                     evaluated.
    # In either case, the output of the macro is run in the caller's
    # context.
    [ 'struct', 'macro', 1,
      [ 'struct sexpr *', 'macro_type', 1 ],
      [ 'struct sexpr *', 'expander', 1 ]],

    # This is the programmer visible representation of a continuation.
    [ 'struct', 'continuation', 1,
      [ 'struct continuation *', 'parent', 1 ]],

    # This is the equivalent of a Logo word or a Lisp symbol, sort of.
    # In Logo, variable references are case insensitive, but words
    # must be printed with the correct case.  Therefore, most information
    # about a word is stored in a 'struct symbol' (defined in list_memory.h),
    # but the actual printable representation is stored in a NAME sexpr.
    # The 'head' points to the beginning of a string allocated by the
    # garbage collector.  We need it in order to properly mark the string.
    # The 'name' points to the first character in the printable representation
    # of this word, and  'length' is the number of characters.
    # We mark 'head' and WE DO NOT mark 'name' during garbage collection.
    # Operations like BUTFIRST can create NAME's whose 'name' points into
    # the middle of another NAME's 'name'.
    [ 'struct', 'name', 1,
      [ 'struct symbol *', 'symbol', 1],
      [ 'char *', 'head', 1 ],
      [ 'char *', 'name', 0 ],
      [ 'unsigned int', 'length', 0]],

    # All numbers are represented as doubles.
    [ 'struct', 'number', 0,
      [ 'double', 'value', 0 ]],

    # The representation of a line read from the terminal or a file.
    # 'raw_line' prints to a word containing the unparsed contents of
    # the line.
    # 'parsed_line' points to a list containing the parsed but not treeified
    # contents of the line.
    # 'procedure' points to a word containing the name of the procedure
    # of which this line is a part.
    [ 'struct', 'line', 1,
      [ 'struct sexpr *', 'raw_line', 1 ],
      [ 'struct sexpr *', 'parsed_line', 1 ],
      [ 'struct sexpr *', 'procedure', 1 ]],

    # Not user visible.  This is used to keep track of information we may
    # need for error messages, information that might otherwise be
    # lost by tail call optimization.
    # 'wanting' is the procedure for which we are currently assembling
    # arguments, if any.
    # 'to_blame' is the procedure we need to blame if there is a failure
    # to output a value, or if a value is output when none is expected.
    # 'line' is the line in which the error is to be reported.
    [ 'struct', 'output_error_info', 1,
      [ 'struct sexpr *', 'wanting', 1 ],
      [ 'struct sexpr *', 'to_blame', 1 ],
      [ 'struct sexpr *', 'line', 1 ]],

    # Not user visible.  Wrapper for file objects.
    [ 'struct', 'filep', 0,
      [ 'FILE *', 'file', 0 ]],

    # Not user visible.  Used when output is being collected into a word.
    [ 'struct', 'byte_bufferp', 1,
      [ 'byte_buffer *', 'byte_buffer', 1 ]],

    # The special value returned by procedures that don't return anything.
    [ 'struct', 'unbound', 0 ],

    # A hack.  See interpreter.c for more details about NO_VALUE.
    [ 'struct', 'no_value', 0 ],

    # ARRAY's.
    # 'length' is the number of items in the array.
    #
    # 'members' is an array of pointers to the members of the array.
    # It contains 'length'+1 pointers, the last of which contains NULL.
    # This allows us to give the garbage collector a function for the
    # C array that marks everything until it finds a NULL.
    #
    # 'origin' is the index of the first element.  Normally it is 1 in
    # Logo, but it can be set to 0 to make some algorithms easier to write.
    [ 'struct', 'array', 1,
      [ 'struct sexpr **', 'members', 1 ],
      [ 'unsigned int', 'length', 0 ],
      [ 'unsigned int', 'origin', 0 ],],
  ],
];

# The evaluator in interpreter.c uses explicit heap allocated continuation
# objects of type 'struct continuation'.  These are tagged unions, which
# each type of continuation representing a different way of processing
# a value that has been returned from a sub-computation.
my $continuation_type =
  [ 'struct continuation', '', 1,
    # The type tag.
    [ 'enum continuation_type', 't', 0 ],

    # The parent continuation.
    [ 'struct continuation *', 'parent', 1],

    # The stack frame to restore upon application of this continuation.
    [ 'struct frame *', 'frame', 1],

    # The expression that was being evaluated, and whose result is to
    # be returned to this continuation.  Mostly for debugging.
    [ 'struct sexpr *', 'expr', 1],

    # The current line to restore upon application of this continuation.
    # Mostly for error messages.
    [ 'struct sexpr *', 'line', 1],

    # The currently allowed results to restore upon application of this
    # continuation.  For detecting "You don't say what to do with ..."
    # errors and "X didn't output to Y" errors.
    [ 'int', 'allowed_results', 0],

    # Details for use in generating "don't say" and "didn't output" errors.
    [ 'struct sexpr *', 'output_error_info', 1],

    # Whether or not we are currently in a tail context.  To be restored
    # upon application of this continuation.
    [ 'int', 'tail', 0],

    # The union containing type specific information.
    [ 'union', 'u', 1,
      # An IF_C is applied to the result of evaluating a condition.
      # The 'then_expr' is to be evaluated if the result is "TRUE
      # and the 'else_expr' is to be evaluated if the result is "FALSE.
      [ 'struct', 'if_c', 1,
        [ 'struct sexpr *', 'then_expr', 1 ],
        [ 'struct sexpr *', 'else_expr', 1 ],],

      # Used for processing (begin ...) expressions.
      [ 'struct', 'begin_c', 1,
        [ 'struct sexpr *', 'exprs', 1 ],],

      # Is applied to the operator of a procedure call.
      [ 'struct', 'oper_c', 1,
        [ 'struct sexpr *', 'params', 1 ],],

      # Is applied to an argument to a procedure call.
      [ 'struct', 'apply_c', 1,
        [ 'struct sexpr *', 'oper', 1 ],
        [ 'struct sexpr *', 'exprs', 1 ],
        [ 'struct sexpr *', 'values', 1 ],],

      # The top level continuation.  Causes eval() to return the value
      # to which the RETURN_C is applied.
      [ 'struct', 'return_c', 1, ],

      # For adding an extra evaluation step when processing (eval <expr>).
      [ 'struct', 'eval_c', 1, ],

      # Used to handle (beginresult ...) which is used to implement
      # RUNRESULT.
      [ 'struct', 'beginresult_c', 1, ],

      # Applied to the value of an expression providing the default value
      # for an optional argument.
      [ 'struct', 'optionals_c', 1,
        [ 'struct sexpr *', 'oper', 1 ],
        [ 'struct sexpr *', 'formals', 1 ],],

      # Used for delaying the printing of error messages in some situations.
      [ 'struct', 'dont_say_c', 1, ],
      [ 'struct', 'didnt_output_c', 1, ],

      # Used to print the return value of a procedure even if the procedure
      # call is removed by tail call optimization.
      [ 'struct', 'trace_c', 1, 
        [ 'struct sexpr *', 'procedure', 1 ],],
    ]
  ];


# You probably want to stop reading here.
# Beyond this point lies madness.



# Create the type declaration for the type.
# Because types can be nested, this is recursive.
# $level is the current recursion level and is used to provide somewhat
# pleasant indentation in the output.
# $type is the type for which we are creating a declaration.
sub make_declaration {
  my($level, $type) = @_;
  my $indent = '  ' x $level;

  # If this type is a struct or a union, and it's not a pointer, then
  # we need to generate a declaration that includes the subtypes within
  # braces.
  if((part_decl($type) =~ /^struct/ || part_decl($type) =~ /^union/) &&
     part_decl($type) !~ /\*/) {
    return $indent . part_decl($type) .  " { \n" .
             join("", (map { make_declaration($level + 1, $_) }
                                    subparts($type))) .
           "$indent}" .  (part_name($type) ? " " . part_name($type) : 
                                             '') . ";\n";
  } else {
    # Otherwise it is a declaration for a simple type, like an int or
    # a pointer, and we just spit out the declaration and the name.
    return $indent . part_decl($type) . ' ' . part_name($type) . ";\n";
  }
}

# Fetch the name of all structs inside of a union.
# This is used to figure out what types of "struct sexpr" there are
# and what types of "struct continuation" there are.  The return
# value is used to generate the list of valid enums, and the list of
# what markers and makers need to be created.
# Perl's automatic list flatening behavior is very handy here.
sub get_type_names {
  my($type) = @_;

  if(part_decl($type) =~ /^struct/ && part_decl($type) !~ /\*/) {
    # recurse blindly into structs
    return +(map { get_type_names($_) } subparts($type));
  } elsif (part_decl($type) =~ /^union/) {
    # Here's the magic.  Only go down one level from a union,
    # and pull the name of each struct inside the union.
    return +(map { part_name($_) } (grep { part_decl($_) =~ /struct/ }
                                   subparts($type)));
  } else {
    # Ignore everything else.
    return;
  }
}

# Generate the list of declarations for every part of $type when we are
# treating a $type as being of type $name.
sub get_part_decls {
  my($name, $type) = @_;

  if(part_decl($type) =~ /^struct/ && part_decl($type) !~ /\*/) {
    # Recurse into structs blindly.
    return +(map { get_part_decls($name, $_) }
                 subparts($type));
  } elsif (part_decl($type) =~ /^union/) {
    # For unions, only recurse into the struct that matches $name.
    return +(map { get_part_decls($name, $_) }
                 (grep { part_name($_) eq $name }
                       subparts($type)));
  } else {
    # Return the declaration for all simple types.
    return part_decl($type);
  }
}

# Join two strings together with a dot if they are both non-empty.
# If one string is empty, then return the other by itself.
sub maybe_dot {
  my($first, $second) = @_;
  if($first && $second) {
    return $first . '.' . $second;
  } elsif($first) {
    return $first;
  } else {
    return $second;
  }
}

# Get a list of fully qualified names, of the sort that might go after
# an arrow operator (->) when an object of type $type has a tag of
# $name.  These are the names that would be actually used to read
# or write members of the structure.  $prefix is used to indicate the
# parent structure name during recursive calls.
sub get_part_names {
  my($name, $prefix, $type) = @_;

  if(part_decl($type) =~ /^struct/ && part_decl($type) !~ /\*/) {
    # If it's a struct, we iterate over all subparts, passing an
    # enhanced prefix with the current type added.
    return +(map { get_part_names($name,
                                   maybe_dot($prefix, part_name($type)),
                                   $_) }
                 subparts($type));
  } elsif (part_decl($type) =~ /^union/) {
    # If it's a union, we get the partnames of all simple types
    # beneath the member named $name.
    return +(map { get_part_names($name,
                                   maybe_dot($prefix, part_name($type)),
                                   $_) }
                 (grep { part_name($_) eq $name }
                       subparts($type)));
  } else {
    # We made it to a simple type.  Here is where recursion terminates.
    # The part_name of the type gets added to the prefix.
    return maybe_dot($prefix, part_name($type));
  }
}

# get_mark_part_names is like get_part_names, but it only generates
# the list of simple types which have been tagged for marking during
# garbage collection.
sub get_mark_part_names {
  my($name, $prefix, $type) = @_;

  if(part_decl($type) =~ /^struct/ && part_decl($type) !~ /\*/) {
    return +(map { get_mark_part_names($name,
                                   maybe_dot($prefix, part_name($type)),
                                   $_) }
                 subparts($type));
  } elsif (part_decl($type) =~ /^union/) {
    return +(map { get_mark_part_names($name,
                                   maybe_dot($prefix, part_name($type)),
                                   $_) }
                 (grep { part_name($_) eq $name }
                       subparts($type)));
  } elsif(is_marked($type)) {
    return maybe_dot($prefix, part_name($type));
  } else {
    return;
  }
}

# This generates the list of short part names when $type is tagged with
# $name, just the very last component in the name of each simple type
# that is in $type.  It is used for generating paramater names for mk_*
# functions.
sub get_short_part_names {
  my($name, $type) = @_;

  if(part_decl($type) =~ /^struct/ && part_decl($type) !~ /\*/) {
    # Recurse blindly into structs.
    return +(map { get_short_part_names($name, $_) }
                 subparts($type));
  } elsif (part_decl($type) =~ /^union/) {
    # For unions, recurse only into the matching struct.
    return +(map { get_short_part_names($name, $_) }
                 (grep { part_name($_) eq $name }
                       subparts($type)));
  } else {
    # Return only the unprefixed name of simple types.
    return part_name($type);
  }
}


# This creates an enum declaration for structures.h
# The allowed values are the upper case versions of the type names.
sub make_enum {
  my($type) = @_;
  my $ret = '';

  my @type_names = get_type_names($type);
  my $enum_type = (grep(/enum/, get_part_decls('', $type)))[0];

  return $enum_type . ' { ' .
                      join(', ', (map { uc($_) } @type_names)) .
                      " };\n";
}

# This generates the declarations or definitions of the functions for
# marking each tagged version of a $type.  $full is 0 if we are
# generating declarations and 1 if we are generating definitions.
sub make_markers {
  my($type, $full) = @_;
  my $ret = '';

  my @type_names = get_type_names($type);
  
  for my $type_name (@type_names) {
    my(@part_decls) = get_part_decls($type_name, $type);
    my(@part_names) = get_part_names($type_name, '', $type);
    my(@mark_part_names) = get_mark_part_names($type_name, '', $type);
    my(@short_part_names) = get_short_part_names($type_name, $type);

    if($full) {
      # We are creating the full definition.
      $ret .= "void mark_${type_name}(GC *g, void *c, object_marker om, weak_pointer_registerer wpr) {\n";
      if(@mark_part_names) {
        # This version has parts that need marking.
        # "s" is a pointer to the main type, to which the (void *) is cast.
        # A call to om() (the object marker) is created for each field
        # that needs to be marked.
        $ret .= '  ' . part_decl($type) .  ' *s = (' .
                       part_decl($type) . " *) c;\n" .
                       join('', (map { "  om(g, s->" . $_ . ");\n" }
                                     @mark_part_names));
      }
      $ret .= "}\n\n";
    } else {
      # We are just creating the declaration.
      $ret .= "void mark_$type_name(GC *g, void *c, object_marker om, weak_pointer_registerer wpr);\n";
    }
  }

  if(!$full) { $ret .= "\n"; }
  return $ret;
}

# This creates the declarations and definitions for the mk_* functions for
# creating different sexpr's and continuations.
# If $full is 0 then we are creating the declarations.
# If $full is 1 then we are creating the definitions.
sub make_makers {
  my($type, $full) = @_;
  my $ret = '';

  my @type_names = get_type_names($type);
  
  for my $type_name (@type_names) {
    my(@part_decls) = get_part_decls($type_name, $type);
    my(@part_names) = get_part_names($type_name, '', $type);
    my(@mark_part_names) = get_mark_part_names($type_name, '', $type);
    my(@short_part_names) = get_short_part_names($type_name, $type);

    if($full) {
      # We are printing the full definition.
      # The mk_* function will take an interpreter context (ic)
      # as an argument, and will take an argument for each simple type
      # contained in $type when tagged as a $type_name.
      my $protect_count = 0;
      $ret .= part_decl($type) . ' *mk_' . $type_name . '(IC *ic';
      for(my $i = 0; $i < @part_decls; $i++) {
        next if $short_part_names[$i] eq 't';
        $ret .= ', ' . $part_decls[$i] . ' ' . $short_part_names[$i];
      }
      $ret .= ") {\n";
      $ret .= '  ' . part_decl($type) . " *ret;\n";

      # All arguments to mk_* functions are protected from the garbage
      # collector during the call to allocate().
      for(my $i = 0; $i < @part_names; $i++) {
        if((grep { $_ eq $part_names[$i] } @mark_part_names)) {
          $ret .= '  protect(ic->g, ' . $short_part_names[$i] . ");\n";
          $protect_count++;
        }
      }

      # The object is allocated.  ic_xmalloc terminates execution if
      # we are out of memory, so there is no need for error checking
      # here.
      $ret .= '  ret = ';
      $ret .= '(' . part_decl($type) . ' *)';
      $ret .= 'ic_xmalloc(ic, sizeof(' . part_decl($type);
      $ret .= '), mark_' . $type_name . ");\n";

      # The arguments are unprotected.
      for(my $i = 0; $i < $protect_count; $i++) {
        $ret .= "  unprotect(ic->g);\n";
      }

      # The parts of the new type are assigned.
      # These assignments are made without STORE() because
      # we know this object has not been traced yet, so it is
      # not possible for us to be in the tracing stage and for
      # ret to be BLACK.
      $ret .= '  ret->t = ' . uc($type_name) . ";\n";
      for(my $i = 0; $i < @part_names; $i++) {
        next if $short_part_names[$i] eq 't';
        $ret .= '  ret->' . $part_names[$i] . ' = ' . 
                            $short_part_names[$i] . ";\n";
      }
      $ret .= "  return ret;\n";
      $ret .= "}\n\n";
    } else {
      # We are just creating the declaration for mk_*
      $ret .= part_decl($type) . ' *mk_' . $type_name . '(IC *ic';
      for(my $i = 0; $i < @part_decls; $i++) {
        next if $short_part_names[$i] eq 't';
        $ret .= ', ' . $part_decls[$i] . ' ' . $short_part_names[$i];
      }
      $ret .= ");\n";
    }
  }

  if(!$full) { $ret .= "\n"; }
  return $ret;
}


# Create structures.h
open(HEADER, '>', 'structures.h') || die("Can't open structures.h");
print HEADER "/*\n";
print HEADER "    ContinuingLogo Logo Interpreter \n";
print HEADER "    \n";
print HEADER "    Copyright (C) 2014 Andru Luvisi\n";
print HEADER "    \n";
print HEADER "    This program is free software: you can redistribute it and/or modify\n";
print HEADER "    it under the terms of the GNU General Public License as published by\n";
print HEADER "    the Free Software Foundation, either version 3 of the License, or\n";
print HEADER "    (at your option) any later version.\n";
print HEADER "    \n";
print HEADER "    This program is distributed in the hope that it will be useful,\n";
print HEADER "    but WITHOUT ANY WARRANTY; without even the implied warranty of\n";
print HEADER "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n";
print HEADER "    GNU General Public License for more details.\n";
print HEADER "    \n";
print HEADER "    You should have received a copy of the GNU General Public License\n";
print HEADER "    along with this program.  If not, see <http://www.gnu.org/licenses/>.\n";
print HEADER " */\n";
print HEADER "\n";

print HEADER "#ifndef STRUCTURES_H\n";
print HEADER "#define STRUCTURES_H\n";
print HEADER "#include \"gc.h\"\n";
print HEADER "#include \"interpreter.h\"\n";
print HEADER "#include \"byte_buffer.h\"\n";
print HEADER "typedef struct sexpr *(*efun)(IC *interp,\n";
print HEADER "                              struct sexpr *args);\n";
for my $type ($sexpr_type, $continuation_type) {
    print HEADER make_enum($type);
    print HEADER make_declaration(0, $type), "\n";
    print HEADER make_markers($type, 0);
    print HEADER make_makers($type, 0);
}
print HEADER "#endif\n";
close(HEADER);


# Create structures.c
open(BODY, '>', 'structures.c') || die("Can't open structures.c");
print BODY "/*\n";
print BODY "    ContinuingLogo Logo Interpreter \n";
print BODY "    \n";
print BODY "    Copyright (C) 2014 Andru Luvisi\n";
print BODY "    \n";
print BODY "    This program is free software: you can redistribute it and/or modify\n";
print BODY "    it under the terms of the GNU General Public License as published by\n";
print BODY "    the Free Software Foundation, either version 3 of the License, or\n";
print BODY "    (at your option) any later version.\n";
print BODY "    \n";
print BODY "    This program is distributed in the hope that it will be useful,\n";
print BODY "    but WITHOUT ANY WARRANTY; without even the implied warranty of\n";
print BODY "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n";
print BODY "    GNU General Public License for more details.\n";
print BODY "    \n";
print BODY "    You should have received a copy of the GNU General Public License\n";
print BODY "    along with this program.  If not, see <http://www.gnu.org/licenses/>.\n";
print BODY " */\n";
print BODY "\n";

print BODY "#include \"list_memory.h\"\n\n";
print BODY "#include \"structures.h\"\n\n";
for my $type ($sexpr_type, $continuation_type) {
    print BODY make_markers($type, 1);
    print BODY make_makers($type, 1);
}
close(BODY);
