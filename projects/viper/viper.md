---
title: "ViPER Programming Language"
description: "A simple functional programming language with built-in unit testing. Compiler hosted in OCaml targeting x86_64 assembly with a C runtime."
github: "https://github.com/jekhi5/ViPER-compiler"
rank: 2
---

ViPER is a collaboration with [Jacob Kline](https://www.thejacobkline.com).

This language was our capstone project for my [undergrad compilers course at Northeastern](https://course.ccs.neu.edu/cs4410sp25/). It supports a number of features:

- First-class functions
- Python-style tuple unpackings
- Exception handling
- Register allocation
- ~~Garbage collection~~

The name **ViPER** stands for **V**erify**i**ng **P**rograms **E**xecute **R**ight, in reference to the in-language [Pyret-style](https://pyret.org/docs/latest/testing.html) unit tests.

ViPER is ergonomic. Here's a simple program: 

```
def fact(n):
  if n == 0: 1
  else: n * fact(n - 1)

def throw():
  raise(RuntimeException)

def isTrue(b):
  isbool(b) && b

1 + 1

# Factorial checks
check:
  fact(3) spits 6,
  fact(5) bites 121
end

check:
  throw() sheds RuntimeException,
  5 sheds ValueException
end
```