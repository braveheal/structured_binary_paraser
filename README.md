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

    cmake .  
    cmake --build .

## SBP schema file
to parse a structured binary file, a schema file to define the data structure is needed. The SBP schema is a C-style format file. 
### Basic types       
**integer types**  
- the integer type is parsed into a int type with fixed length of bits and whether signed/unsigned. like this:  
```
    typealias integer {size = 3;}  := Uint3;   
    typealias integer {size = 3; signed = true;}  := Int3;
```
  
**floating_point types**  
**enum types**  
**Formula types**  
**struct types**  
**variant types**  
**array types**
