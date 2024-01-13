# lightpi
Lightweight Pascal Interpreter

## Goals
* code base as small as possible
* easy to understand and expand
* usable in real world projects

## Features
LPI offers dynamic types (implemented as variant) and a subset of the pascal language with no changes to syntax. The following features are implemented:
* support for **string** and **number types**
* +, -, *, /, div, mod **math operators**
* not, and, or **logic operators**
* =, <>, >, <, >=, <= **comparison operators**
* While Do, Repeat Until, For To Do **loops**
* If Then Else **control structure**
* Begin End **command block**

The following internal procedures / functions are currently implemented and can be expanded when needed:
* Round
* WriteLn

## Usage
LPI has been created for education. Most other available pascal interpreters written for education are very simple and don't support enough features of the pascal language. On the other hand there are huge complex projects which work great, with the downside of being more difficult to understand and modify. LPI tries to be inbetween those projects. 
Simple enough to understand, yet providing enough features to get users started to modify and explore. LPI has been implemented as a self contained **TLightPascalInterpreter** class in **ulpi.pas**. The project comes with a set of tests to verify the interpreter is working correctly.

How to load and execute a script:

```pascal
var lpi: TLightPascalInterpreter;
begin
  lpi := TLightPascalInterpreter.Create;
  lpi.Load('for i := 1 to 3 do writeln(i, ''x Hello World!'');');
  lpi.Execute;
  FreeAndNil(lpi);
end;  
```

Script output:

```
1x Hello World!
2x Hello World!
3x Hello World!
```

Each interpreter stage can output debugging information, to better understand the code. Debugging can be enabled like this ***TLightPascalInterpreter.Create(true);*** and will produce the following output for the example script mentioned above:

```
*** Lexer ***
1 Identifier for
1 Identifier i
1 Operator   :=
1 Number     1
1 Identifier to
1 Number     3
1 Identifier do
1 Identifier writeln
1 Single     (
1 Identifier i
1 Single     ,
1 String     x Hello World!
1 Single     )
1 Single     ;

*** Parser ***

# Resulting Tree:
+ begin-end
  + for-to
    | var i, id: 0
    | number 1
    | number 3
    + writeln
      | var i, id: 0
      | string 'x Hello World!'

*** Interpreter ***
1x Hello World!
2x Hello World!
3x Hello World!

# Variable State Dump
i := 3

# Execution halted.
```


The interpreter directly executes the resulting syntax tree to keep the code simple. Please have a look at the included examples for more details and inspiration.
