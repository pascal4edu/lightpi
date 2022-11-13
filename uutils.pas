unit uutils;

{$mode objfpc}{$H+}

interface

function lpi_is_alpha(c: Char): Boolean;
function lpi_is_numeric(c: Char): Boolean;
function lpi_is_operator(c: Char): Boolean;
function lpi_is_math_operator(c: Char): Boolean;
function lpi_is_whitespace(c: Char): Boolean;

function tokentostr(id: Byte): AnsiString;
function operationtostr(op: Byte): AnsiString;

function padstring(s: AnsiString; l: Integer; ch: Char = #32; reverse: Boolean = false): AnsiString;

implementation

uses SysUtils, uconstants;

function lpi_is_alpha(c: Char): Boolean;
begin
  Result := c in ['A' .. 'Z', 'a' .. 'z'];
end;

function lpi_is_numeric(c: Char): Boolean;
begin
  Result := c in ['0' .. '9'];
end;

function lpi_is_operator(c: Char): Boolean;
begin
  Result := c in [':', '=', '<', '>', '+', '-', '*', '/'];
end;

function lpi_is_math_operator(c: Char): Boolean;
begin
  Result := c in ['+', '-', '*', '/'];
end;

function lpi_is_whitespace(c: Char): Boolean;
begin
  Result := c in [#9, #10, #13, #32]; // Tab, Line Feed, Carriage Return, Space
end;

function tokentostr(id: Byte): AnsiString;
begin
  case id of
    CtokenIdentifier: Result := 'Identifier';
    CtokenOperator: Result := 'Operator';
    CtokenString: Result := 'String';
    CtokenNumber: Result := 'Number';
    CtokenComment: Result := 'Comment';
    CtokenSingle: Result := 'Single';
    else Result := '<Unknown Token ID ' + IntToStr(id) + '!>';
  end;
end;

function operationtostr(op: Byte): AnsiString;
begin
  case op of
    CopNOP: Result := 'NOP';
    CopAssign: Result := ':=';
    CopCommandBlock: Result := 'begin-end';
    CopCondition: Result := 'if-then';
    CopLoopWhile: Result := 'while-do';
    CopLoopRepeatUntil: Result := 'repeat-until';
    CopLoopForTo: Result := 'for-to';
    CopLogicTrue: Result := 'true';
    CopLogicFalse: Result := 'false';
    CopLogicOr: Result := 'or';
    CopLogicAnd: Result := 'and';
    CopLogicNot: Result := 'not';
    CopCompareEqual: Result := '=';
    CopCompareUnequal: Result := '<>';
    CopCompareGreater: Result := '>';
    CopCompareSmaller: Result := '<';
    CopCompareGreaterOrEqual: Result := '>=';
    CopCompareSmallerOrEqual: Result := '<=';
    CopMathAdd: Result := '+';
    CopMathSub: Result := '-';
    CopMathMul: Result := '*';
    CopMathDiv: Result := '/';
    CopMathDivInt: Result := 'div';
    CopMathMod: Result := 'mod';
    CopVariable: Result := 'var';
    CopString: Result := 'string';
    CopNumber: Result := 'number';
    CopRound: Result := 'round';
    CopWriteln: Result := 'writeln';
    else Result := '<Unknown Operation ID ' + IntToStr(op) + '!>';
  end;
end;

// String padding...
function padstring(s: AnsiString; l: Integer; ch: Char = #32; reverse: Boolean = false): AnsiString;
begin
  while Length(s) < l do
  if reverse then s := ch + s
             else s := s + ch;

  Result := s;
end;

end.

