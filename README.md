# structured_binary_paraser (SBP)
A cross platform stuctured binary parser and parsing code generator written in pure c. 

## Installation

to build SBP you need:

1. A C99 compatible compiler
2. CMAKE
3. pkg-config
4. [glib-2.0](https://github.com/GNOME/glib?tab=readme-ov-file)>= 2.64
5. [json-glib-1.0](https://github.com/GNOME/json-glib)
6. [re2c](https://re2c.org/) lexer generator
7. [lemon](https://sqlite.org/src/doc/trunk/doc/lemon.html) Parser Generator

Once you have the dependency tools installed. you can use CMAKE to build and install SBP.

``
  cmake .
  cmake --build .
``

