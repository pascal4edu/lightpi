unit utypes;

{$mode objfpc}

interface

uses Classes, SysUtils, ulist;

type
  // Lexer Types

  TToken = class(TObject)
    id: Byte;
    s: AnsiString;
    line: Integer;
  end;

  // Parser / Syntax Tree Types

  TASTTreeNode = class(TObject)
    operation: Byte;
    s: AnsiString;   // all strings like identifier, constants etc
    n: Extended;     // numbers (used internally to prevent conversions string->number)
    var_id: Integer; // assigns a unique index to every variable for example a becomes vars[0], b becomes vars[1] etc.
    line: Integer;   // original line in source
    children: TLightList;

    constructor Create(param_operation: Byte; param_s: AnsiString; param_line: Integer);
    function getChild(index: Integer): TASTTreeNode;
    function firstChild: TASTTreeNode;
    function secondChild: TASTTreeNode;
    function thirdChild: TASTTreeNode;
    function fourthChild: TASTTreeNode;
    function lastChild: TASTTreeNode;
    procedure debug(symbol_table: TStringList; messages: TStringList; current_depth: integer = 0);
    procedure clearChildren;
    destructor Destroy; override;
  end;

implementation

uses uconstants, uutils;

constructor TASTTreeNode.Create(param_operation: Byte; param_s: AnsiString; param_line: Integer);
begin
  operation := param_operation;
  s := param_s;

  // automatically populate number field for caching
  if param_operation = CopNumber then
    n := StrToFloatDef(s, 0, FormatSettings)
  else
    n := 0;

  var_id := 0; // Default is function result
  line := param_line;
  children := TLightList.Create;
end;

function TASTTreeNode.getChild(index: Integer): TASTTreeNode;
begin
  Result := TASTTreeNode(children.Items(index));
end;

// for convenience...
function TASTTreeNode.firstChild: TASTTreeNode;
begin
  Result := TASTTreeNode(children.GetFirst);
end;

function TASTTreeNode.secondChild: TASTTreeNode;
begin
  Result := TASTTreeNode(children.GetSecond);
end;

function TASTTreeNode.thirdChild: TASTTreeNode;
begin
  Result := TASTTreeNode(children.GetThird);
end;

function TASTTreeNode.fourthChild: TASTTreeNode;
begin
  Result := TASTTreeNode(children.GetFourth);
end;

function TASTTreeNode.lastChild: TASTTreeNode;
begin
  Result := TASTTreeNode(children.Get);
end;

// "pretty print" the syntax tree for debug output
procedure TASTTreeNode.debug(symbol_table: TStringList; messages: TStringList; current_depth: integer = 0);
var i: Integer;
    temp_s: AnsiString;
begin
  temp_s := '';

  for i := 1 to current_depth do temp_s := temp_s + ('  ');     // indentation

  if children.Count > 0 then
    temp_s := temp_s + ('+ ')
  else
    temp_s := temp_s + ('| ');

  temp_s := temp_s + operationtostr(operation);

  if (operation = CopVariable) then
    temp_s := temp_s + ' ' +  symbol_table.Strings[var_id] + ', id: ' + IntToStr(var_id);

  if s <> '' then
  begin
    if operation = CopString then
      temp_s := temp_s + ' ''' + s + ''''
    else
      temp_s := temp_s + ' ' + s; // number etc
  end;

  messages.Add(temp_s);

  for i := 0 to children.Count - 1 do getChild(i).debug(symbol_table, messages, current_depth + 1);

  dec(current_depth);
end;

procedure TASTTreeNode.clearChildren;
var node: TASTTreeNode;
begin
  while children.Count > 0 do
  begin
    node := TASTTreeNode(children.RemoveFirst);
    FreeAndNil(node);
  end;
end;

// deleting the master node will delete the whole tree
destructor TASTTreeNode.Destroy;
begin
  clearChildren;
  FreeAndNil(children);

  inherited;
end;

begin
  // needed to convert floats correctly
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DecimalSeparator := '.';
end.

