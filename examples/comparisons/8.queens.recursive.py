#!/usr/bin/python

def test_position(new, rest):
  l = len(rest)
  for i in range(l):
    if new == rest[i] or new == rest[i]+(l-i) or new == rest[i]-(l-i):
      return False
  return True

def f_8_queens():
  f_8_queens_helper(8, []);

def f_8_queens_helper(n, sofar):
  if n == 0:
    print sofar
    return

  for i in range(8):
    if test_position(i, sofar):
      f_8_queens_helper(n-1, sofar + [i]);

f_8_queens()
