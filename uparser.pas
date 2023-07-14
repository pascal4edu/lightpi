unit uparser;

{$mode objfpc}{$H+}

interface

uses Classes, utypes, ulist, uerror;

type TLPI_Parser = class(TLPI_ErrorMessage)
       private
         function create_node(operation: Byte; s: AnsiString; line: Integer): TASTTreeNode;
         function create_binary_node(operation: Byte; s: AnsiString; line: Integer; child1, child2: TASTTreeNode): TASTTreeNode;
         function expectVariable(node: TASTTreeNode; parent_operation: Byte): Boolean;
         function get_variable_id(s: AnsiString): Integer;

       public
         symbols: TStringList;

         Constructor Create; Override;
         function execute(tokenlist: TLightList): TASTTreeNode; // returns the rootNode of the AST
         Destructor Destroy; Override;
     end;

implementation

uses SysUtils, uconstants, uutils;

Constructor TLPI_Parser.Create;
begin
  inherited;

  symbols := TStringList.Create; // stores original variable names
end;

function TLPI_Parser.create_node(operation: Byte; s: AnsiString; line: Integer): TASTTreeNode;
begin
  Result := TASTTreeNode.Create(operation, s, line);
end;

function TLPI_Parser.create_binary_node(operation: Byte; s: AnsiString; line: Integer; child1, child2: TASTTreeNode): TASTTreeNode;
begin
  Result := TASTTreeNode.Create(operation, s, line);
  if child1 <> nil then Result.children.Add(child1);
  if child2 <> nil then Result.children.Add(child2);
end;

// operation type helper
function TLPI_Parser.expectVariable(node: TASTTreeNode; parent_operation: Byte): Boolean;
begin
  Result := false;
  if node = nil then Exit;

  Result := (node.operation = CopVariable);
  if not Result then LogError('Variable expected, found ' + operationtostr(node.operation) + ' ' + node.s, node.line, parent_operation);
end;

function TLPI_Parser.get_variable_id(s: AnsiString): Integer;
var i, index: Integer;
    found: Boolean;
begin
  s := LowerCase(s);
  index := Cunknown_var_id;
  found := false;

  for i := 0 to symbols.Count - 1 do
  if symbols.Strings[i] = s then
  begin
    found := true;
    index := i;
    break;
  end;

  if not found then
  if symbols.Count < ClpiInterpreterMaxVariables then
    index := symbols.Add(s)
  else
    LogError('Internal Error. Maximum number of allowed variables reached (' + IntToStr(ClpiInterpreterMaxVariables) + ')', Cunknown_line, Cunknown_operation);

  Result := index;
end;


function TLPI_Parser.execute(tokenlist: TLightList): TASTTreeNode;
var rootNode: TASTTreeNode;
    error_token: TToken;
    current_line: Integer; // cache last line if current token is invalid or empty...

// reads a token from the input
function preview_token: TToken;
begin
  Result := nil;

  if tokenlist.Count < 1 then
  begin
    LogError('Unexpected End of Program', current_line, Cunknown_operation);
    Result := error_token;
  end
  else Result := TToken(tokenlist.GetFirst);
end;

// reads a token from the input AND deletes it (pop)
procedure read_token;
var token: TToken;
begin
  if isError then Exit;

  token := preview_token;
  tokenlist.RemoveFirst;
  FreeAndNil(token);
end;

// compares the string with the token, peek() does NOT read the token!
function peek(s: AnsiString): Boolean; inline;
begin
  if tokenlist.Count < 1 then
    Result := false
  else
    Result := (UpperCase(s) = UpperCase(preview_token.s));
end;

// compares the string and reads (accepts) the token on a match
function accept(s: AnsiString): Boolean; inline;
begin
  Result := false;
  if isError then Exit;

  if peek(s) then
  begin
    read_token;
    Result := true;
  end;
end;

// the token is read and the string must match
procedure expect(s: AnsiString); inline;
begin
  if not accept(s) then
    LogError(s + ' expected, but ' + preview_token.s + ' found', current_line, Cunknown_operation);
end;

// the token is read and checked for a matching type
function expect_and_read_number(operation: Byte): Extended;
var token: TToken;
begin
  Result := 0;

  token := preview_token;
  if token.id <> CtokenNumber then
  begin
    LogError('Number expected, found ' + token.s, token.line, operation);
    Exit;
  end;

  Result := StrToFloatDef(token.s, 0, FormatSettings);
  read_token; // also consume the token from the input stream
end;

// returns the name of the identifier
function expect_and_read_identifier(operation: Byte): AnsiString;
var token: TToken;
begin
  Result := '';

  token := preview_token;
  if token.id <> CtokenIdentifier then
  begin
    LogError('Identifier expected, found ' + token.s, token.line, operation);
    Exit;
  end;

  Result := token.s;
  read_token; // also consume the token from the input stream
end;

// needed for primary expressions to read complex, recursive constructs -> go back to the beginning of parsing
function expr: TASTTreeNode; forward;
// needed for primary expressions to read complex, recursive constructs -> go back to mathematical expression parsing
function add_expr: TASTTreeNode; forward;

{
# How does the Parser work?

Every pascal statement (like conditions and loops) consists of expressions. The following code tries to
match the expressions, starting with primary expressions (such as numbers and variables) down to
more complex ones (like math operations, logic).

Every expression procedure first calls the procedure one level above itself and tries to find a match for the following token.
After the procedure returns, the next match is attempted. Anything matching inside the levels above the current procedure
was already parsed and is out of the way in the token list. For example a for statement parses "<for> ... <:=> ... <to> ... <do>"
but it doesn't parse the expressions like variables or math equations. It is calling the according procedures above it instead,
like prim_expr() (for variables) and add_expr() (for math expressions). The whole process starts at the bottom with statement().

# A more detailed breakdown of "for i := 1 to 2+3 do foobar;"

First, <for> is recognized as statement. The statement procedure calls the procedure to parse primary expressions to parse
the variable. The check if it really was a variable is done later, when building the syntax tree. It matches CtokenIdentifier <i>
and the code goes back to the statement. The next token is expected to be <:=>, no exceptions. Luckily it is there and the parser
is fetching the beginning of the loop range, which is the primary expression CtokenNumber <1>.

The code goes back to the statement and expects <to>, which is there. The final expression is the end of the loop range,
which is at first a simple CtokenNumber <2>, but it matches add_expr() on the way back and recognizes the <+>.
add_expr() keeps looking for additional + or - and calls the next higher function to check for expressions
like 4*5 or (4/2) etc. However the next expression is again a simple CtokenNumber <3>.
The code returns and expects <do> as token, which is there.

Finally the next statement is read within the for statement, which could be a begin / end block, a procedure call <foobar> or another loop.
If the statement was read successfully, the code returns to the main loop and tries to read the next statement.
}

// primary expressions
function prim_expr: TASTTreeNode;
var token: TToken;
    temp_s: AnsiString;
    i: Integer;
begin
  Result := nil;

  if isError then Exit;

  token := preview_token;
  current_line := token.line; // cache the current line number for possible error messages after the token was consumed

  if token.id = CtokenNumber then
  begin
    Result := create_node(CopNumber, token.s, current_line); // accept a Number 0123.4
    read_token;
  end
  else
  if token.id = CtokenString then
  begin
    Result := create_node(CopString, token.s, current_line); // accept a String ''
    read_token;
  end
  else
  if peek('#') then
  begin
    temp_s := '';
    while (accept('#') and not isError) do
    begin
      i := round(expect_and_read_number(CopString));
      if (i < 0) or (i > 255) then
        LogError('Constant out of range. Expected 0 - 255, found ' + IntToStr(i), current_line, CopString)
      else
        temp_s := temp_s + chr(i); // accept a Char #13
    end;

    Result := create_node(CopString, temp_s, current_line); // pascal allows chaining without a +, like #13#10
  end
  else
  if accept('not') then
  begin
    Result := create_node(CopLogicNot, '', current_line); // accept not
    Result.children.Add(expr());
  end
  else
  if accept('true') then
  begin
    Result := create_node(CopLogicTrue, '', current_line); // accept True
  end
  else
  if accept('false') then
  begin
    Result := create_node(CopLogicFalse, '', current_line); // accept False
  end
  else
  if accept('round') then // accept round(x);
  begin
    Result := create_node(CopRound, '', current_line); // round
    if not peek('(') then expect('('); // force ()
    Result.children.Add(add_expr());
  end
  else
  if token.id = CtokenIdentifier then
  begin
    // add it as variable, save the name in case of an error message
    temp_s := token.s;

    // s is cleared, so it won't be printed twice on debug - the only reference is the var_id now
    Result := create_node(CopVariable, '', current_line);

    // read and consume token, so we can use peek()
    read_token;

    // cache symbols
    Result.var_id := get_variable_id(temp_s);

    // variables are not allowed to have a following ()
    if peek('(') then
      LogError('Unexpected ( after variable ' + temp_s, current_line, CopVariable);
  end
  else
  // normal brackets e.g. for mathematical expressions
  if accept('(') then
  begin
    Result := expr(); // accepts anything between the brackets

    if isError then Exit;

    expect(')');
    // we don't need to add any actual bracket nodes to the ast, 
    // the structure already takes care of the right execution order :-)
  end
  else
  // special case for sign e.g. a := +a;
  // it just gets ignored, this is different from 2 + 3 which is accepted as an operator in add_expr
  if (accept('+')) then
    Result := prim_expr() 
  else
  if (accept('-')) then
  begin
    token := preview_token;
    // is it a number? just add the sign...
    if token.id = CtokenNumber then
    begin
      Result := create_node(CopNumber, '-' + token.s, current_line); // accept negative Number -0123.4
      read_token;
    end
    else 
      // special case for variables e.g. a := -x;
      // the parser adds a multiplication node so cases like -x become -1 * x
      Result := create_binary_node(CopMathMul, '', current_line, create_node(CopNumber, '-1', current_line), prim_expr()); // accept  -a
  end
  else
    LogError('Unexpected primary token ''' + token.s + '''', token.line, Cunknown_operation);
end;

// order of math operations is set by the order of procedure calls
// calling the next higher one as first line to check if the expression is known (accepted)
function mul_expr: TASTTreeNode;
begin
  Result := nil;

  if isError then Exit;
  Result := prim_expr();
  if isError then Exit;

  current_line := preview_token.line;
  while (peek('*') or peek('/') or peek('mod') or peek ('div')) and (not isError) do
  begin
    if accept('*') then
      Result := create_binary_node(CopMathMul, '', current_line, Result, prim_expr()) // accept *
    else
    if accept('/') then
      Result := create_binary_node(CopMathDiv, '', current_line, Result, prim_expr()) // accept /
    else
    if accept('mod') then
      Result := create_binary_node(CopMathMod, '', current_line, Result, prim_expr()) // accept mod
    else
    if accept('div') then
      Result := create_binary_node(CopMathDivInt, '', current_line, Result, prim_expr()); // accept div
  end;
end;

// lower math expressions
function add_expr: TASTTreeNode;
begin
  Result := nil;

  if isError then Exit;
  Result := mul_expr();
  if isError then Exit;

  current_line := preview_token.line;
  while (peek('+') or peek('-')) and (not isError) do
  begin
    if accept('+') then
      Result := create_binary_node(CopMathAdd, '', current_line, Result, mul_expr()) // accept +
    else
    if accept('-') then
      Result := create_binary_node(CopMathSub, '', current_line, Result, mul_expr()); // accept -
  end;
end;

// compare operators
function eq_expr: TASTTreeNode;
begin
  Result := nil;

  if isError then Exit;
  Result := add_expr();
  if isError then Exit;

  current_line := preview_token.line;
  if accept('=') then
    Result := create_binary_node(CopCompareEqual, '', current_line, Result, add_expr()) // accept =
  else
  if accept('<>') then
    Result := create_binary_node(CopCompareUnequal, '', current_line, Result, add_expr()) // accept <>
  else
  if accept('>') then
    Result := create_binary_node(CopCompareGreater, '', current_line, Result, add_expr()) // accept >
  else
  if accept('<') then
    Result := create_binary_node(CopCompareSmaller, '', current_line, Result, add_expr()) // accept <
  else
  if accept('>=') then
    Result := create_binary_node(CopCompareGreaterOrEqual, '', current_line, Result, add_expr()) // accept >=
  else
  if accept('<=') then
    Result := create_binary_node(CopCompareSmallerOrEqual, '', current_line, Result, add_expr()); // accept <=
end;

// logic
function logic_expr: TASTTreeNode;
begin
  Result := nil;

  if isError then Exit;
  Result := eq_expr();
  if isError then Exit;

  current_line := preview_token.line;
  while (peek('and') or peek('or')) and (not isError) do
  begin
    if accept('and') then
      Result := create_binary_node(CopLogicAnd, '', current_line, Result, eq_expr()) // accept and
    else
    if accept('or') then
      Result := create_binary_node(CopLogicOr, '', current_line, Result, eq_expr()); // accept or
  end;
end;

// assignment
function expr: TASTTreeNode;
begin
  Result := nil;

  if isError then Exit;
  Result := logic_expr();
  if isError then Exit;

  current_line := preview_token.line;

  // is it an assignment? check the left and get the right side of the assignment...
  if accept(':=') then
  begin
    if not expectVariable(Result, CopAssign) then Exit; // left side has to be a variable

    Result := create_binary_node(CopAssign, '', current_line, Result, logic_expr()); // accept :=
  end;
end;

// recognizes a statement like a "begin end" block or a "for to do" loop
function statement: TASTTreeNode;
var tempNode: TASTTreeNode;
    temp_s: AnsiString;
begin
  Result := nil;

  if isError then Exit;

  current_line := preview_token.line;

  if accept('begin') then
  begin
    Result := create_node(CopCommandBlock, '', current_line); // accept begin-end

    while not accept('end') and (not isError) do Result.children.Add(statement());

    if not peek('else') then expect(';'); // exception for else, no ; before it! otherwise expect end;
  end
  else
  if accept('if') then
  begin
    Result := create_binary_node(CopCondition, '', current_line, logic_expr(), nil); // accept condition

    expect('then');

    Result.children.Add(statement()); // statement

    if accept('else') then
      Result.children.Add(statement()); // statement for else
  end
  else
  if accept('while') then
  begin
    Result := create_binary_node(CopLoopWhile, '', current_line, logic_expr(), nil); // accept while (condition)
    expect('do');
    Result.children.Add(statement()); // statement
  end
  else
  if accept('repeat') then
  begin
    Result := create_node(CopLoopRepeatUntil, '', current_line); // accept repeat until

    while not accept('until') and (not isError) do Result.children.Add(statement());

    if isError then Exit;

    Result.children.Add(logic_expr()); // read the condition
    if not peek('else') then expect(';'); // exception for else, no ; before it! otherwise expect end;
  end
  else
  if accept('for') then // accept for i := 3 to 5 do
  begin
    tempNode := prim_expr();

    if not expectVariable(tempNode, CopLoopForTo) then Exit;

    Result := create_binary_node(CopLoopForTo, '', current_line, tempNode, nil); // add the loop variable i

    expect(':=');

    Result.children.Add(add_expr()); // add the loop start 3
    expect('to');

    Result.children.Add(add_expr()); // add the loop end 5
    expect('do');
    Result.children.Add(statement()); //looped statement
  end
  else
  if accept('writeln') then // accept messages.Add(a);
  begin
    expect('(');

    // at least one argument is expected, so don't use a while loop to check
    Result := create_node(CopWriteln, '', current_line); // messages.Add
    repeat
      tempNode := logic_expr(); // logic_expr because no lower levels are allowed (e.g. no statements like messages.Add(repeat ... until...);)
      Result.children.Add(tempNode);
    until (not accept(',')) or (isError);

    if isError then Exit;

    expect(')');
    if not peek('else') then expect(';'); // exception for else, no ; before it! otherwise expect end;
  end
  else
  begin
    temp_s := preview_token.s; // cache last token for error messages

    // parses variables, however result is not allowed to be a single variable or expression
    // like add, mul etc at statement level e.g. a := 5; is allowed a; / a + 1; a or b; etc. is not!
    Result := expr();

    if isError then Exit;

    // only assign and internal functions are allowed...
    if not ((Result.operation = CopAssign) or (Result.operation = CopRound)) then
      LogError('Unexpected "' + temp_s + '"', current_line, Cunknown_operation);


    // exception for else, to accept code like this: if true then x := 5 else x := 7;
    if not (peek('else')) then expect(';');
  end;
end;

var token: TToken;
    i: Integer;
begin
  Result := nil;

  if tokenlist = nil then Exit;

  if ClpiDebugMode then
  begin
    messages.Add('');
    messages.Add('*** Parser ***');
  end;

  current_line := Cunknown_line;

  // create dummy token for easier error handling...
  error_token := TToken.Create;
  error_token.id := CopInvalid; // invalid token
  error_token.s := '<EOF>'; // invalid string
  error_token.line := Cunknown_line; // unknown line

  rootNode := create_node(CopCommandBlock, '', 0); // encase the whole code in a begin-end block...

  // accept our language....
  while (tokenlist.Count > 0) and (not isError) do rootNode.children.Add(statement());

  // delete the token list...
  for i := 0 to tokenlist.Count - 1 do
  begin
    token := TToken(tokenlist.Items(i));
    FreeAndNil(token);
  end;

  // it could be nil if an error occured before
  if error_token <> nil then
    FreeAndNil(error_token); 

  if isError then
    FreeAndNil(rootNode) // delete the whole (likely incomplete) abstract syntax tree
  else
  begin
    if ClpiDebugMode and not isError then
    begin
      messages.Add('# Resulting Tree:');
      rootNode.debug(symbols, messages); // contains the root of the abstract syntax tree, parses the other nodes recursively
    end;

    Result := rootNode;
  end;

  FreeAndNil(tokenlist);
end;

Destructor TLPI_Parser.Destroy;
begin
  FreeAndNil(symbols);

  inherited;
end;

end.
