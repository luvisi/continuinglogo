#!/usr/bin/perl

use strict;
use warnings;

sub test_position {
  my($new, $rest) = @_;

  for(my $i = 0; $i < @$rest; $i++) {
    if($new == $rest->[$i]) { return; }
    if($new == $rest->[$i]+(@$rest-$i)) { return; }
    if($new == $rest->[$i]-(@$rest-$i)) { return; }
  }
  return 1;
}

sub f_8_queens {
  f_8_queens_helper(8, []);
}

sub f_8_queens_helper {
  my($n, $sofar) = @_;

  if($n == 0) {
      for my $c (@$sofar) {
          print $c, " ";
      }
      print "\n";
      return;
  }

  for(my $i = 0; $i < 8; $i++) {
    if(test_position($i, $sofar)) {
      f_8_queens_helper($n-1, [@$sofar, $i]);
    }
  }
}

f_8_queens();
