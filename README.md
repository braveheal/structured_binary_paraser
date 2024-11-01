# structured_binary_parser (SBP)
A cross platform stuctured binary parser and parsing code generator written in pure c. the project was first intend to parse Qualcomm diagnostics log (DLF) file format as well as other structed binary file formats.
- ability to generate high performance c/c# parsing code to parse binary logging
- parse binary file when the schema file changed, no need to re-generte parsing code, which is useful when debugging the schema file.

## Installation

to build SBP you need:

1. A C99 compatible compiler
2. CMAKE
3. pkg-config
4. [glib-2.0](https://github.com/GNOME/glib?tab=readme-ov-file)>= 2.64
   - a [Meson build system](https://mesonbuild.com/) is required to build glib.
6. [json-glib-1.0](https://github.com/GNOME/json-glib)
7. [re2c](https://re2c.org/) lexer generator
8. [lemon](https://sqlite.org/src/doc/trunk/doc/lemon.html) Parser Generator
   - [windows build](https://github.com/deplinenoise/lemon-win32)

Once you have the dependency tools installed. you can use CMAKE to build and install SBP.
```
cmake .  
cmake --build .
```
a sbp.exe(windows) file is ready to use.
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
```
typealias floating_point { size = 32; } := Float32;
```
**enum types**  
- enum types are sub types of unsigned integer.
```
enum : Uint32 {UnknownVersions = Default,Version1 = 1,Version2 = 2,Version3 = 3,Version4 = 4,Version5 = 5} Version;
```
**formula types**  
- formula types don't occupy the bits, they are results computed by some fields.
```
Uint8 BufferSize = ((((((LCG0 + LCG1) + LCG2) + LCG3) + LCG4) + LCG5)+ LCG6) + LCG7;
```
**struct types**  
- struct types are simliar to a c style struct.
```
struct
{
Uint8 SlotNumber;
skip 8;
Uint10 FN;
skip 6;
} SysTime;
```
**variant types**  
- variant types are similar to c style union. except the actual type is determined by the variant parameter which is a enum type.
```
enum : Uint32 {UnknownVersions = Default,Version1 = 1,Version2 = 2,Version3 = 3,Version4 = 4,Version5 = 5} Version;
variant <Version>
{
    struct
    {
    ...
    } UnknownVersions;
    struct
    {
    ...
    } Version1;
    ...
}
```
**array types**
- array type can be fixed length array or variable length
```
Uint8 Buffer[8];
Uint8 Length[((54==MCEType||56==MCEType)||60==MCEType)||62==MCEType];
struct
{
Uint6 PH;
Uint1 V;
Uint1 P;
Uint8 Pcmaxfc[0==V];
Uint8 R[0==V];
} SCell[NumCxSetBits];
```
## Usage
```
sbp.exe [schema_file] [binary_file]
```
will generate a json format parsing result in the console and also the c parsing codes will be in the directory. some sample file can be found in the test folder.

![image](https://github.com/user-attachments/assets/11376830-c58d-438a-a58e-3f10bcea6ab6)


