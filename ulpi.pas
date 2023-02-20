unit ulpi;

{$mode objfpc}

interface

uses Classes, utypes, uconstants, ulexer, uparser, uinterpreter;

type TLightPascalInterpreter = class(TObject)
       private
         rootNode: TASTTreeNode; // root of the abstract syntax tree

         lexer: TLPI_Lexer;
         parser: TLPI_Parser;
         interpreter: TLPI_Interpreter;

       public
         Constructor Create(debug: Boolean = false); virtual;
         function Load(s: AnsiString): Boolean;
         procedure ResetVariables;
         function GetVariable(name: AnsiString): Variant;
         procedure SetVariable(name: AnsiString; value: Variant);
         function Execute: Boolean;
         function isError: Boolean;
         procedure ClearMessages;
         function GetMessages(clear: Boolean = False): TStringList;
         procedure PrintMessages(clear: Boolean = False);
         Destructor Destroy; override;
     end;

implementation

uses sysutils, ulist;

Constructor TLightPascalInterpreter.Create(debug: Boolean = false);
begin
  ClpiDebugMode := debug;

  lexer := TLPI_Lexer.Create;
  parser := TLPI_Parser.Create;
  interpreter := TLPI_Interpreter.Create;
end;

// returns true on success
function TLightPascalInterpreter.Load(s: AnsiString): Boolean;
var tokenlist: TLightList;
begin
  Result := false;

  // was something already loaded?
  if rootNode <> nil then
    FreeAndNil(rootNode)
  else
    rootNode := nil;

  tokenlist := lexer.execute(s);
  if lexer.isError then Exit;

  rootNode := parser.execute(tokenlist);
  if parser.isError then Exit;

  ResetVariables; // create and initialize all variables with 0

  Result := true;
end;

procedure TLightPascalInterpreter.ResetVariables;
begin
  interpreter.init_variables(parser.symbols.Count);
end;

function TLightPascalInterpreter.GetVariable(name: AnsiString): Variant;
begin
  Result := interpreter.get_variable(name, parser.symbols);
end;

procedure TLightPascalInterpreter.SetVariable(name: AnsiString; value: Variant);
begin
  interpreter.set_variable(name, value, parser.symbols);
end;

// returns true on success
function TLightPascalInterpreter.Execute: Boolean;
begin
  Result := false;

  if (lexer.isError) or (parser.isError) then Exit;

  interpreter.execute(rootNode, parser.symbols);

  Result := not interpreter.isError;
end;

function TLightPascalInterpreter.isError: Boolean;
begin
  Result := lexer.isError or parser.isError or interpreter.isError;
end;

procedure TLightPascalInterpreter.ClearMessages;
begin
  lexer.messages.Clear;
  parser.messages.Clear;
  interpreter.messages.Clear;
end;

function TLightPascalInterpreter.GetMessages(clear: Boolean = False): TStringList;
var s: AnsiString;
begin
  Result := TStringList.Create;

  for s in lexer.messages do
    Result.Add(s);

  for s in parser.messages do
    Result.Add(s);

  for s in interpreter.messages do
    Result.Add(s);

  if clear then ClearMessages;
end;


// messages can easily be processed in other ways if needed
procedure TLightPascalInterpreter.PrintMessages(clear: Boolean = False);
var total_messages: TStringList;
begin
  total_messages := GetMessages(clear);

  while total_messages.Count > 0 do
  begin
    writeln(total_messages[0]);
    total_messages.Delete(0);
  end;

  FreeAndNil(total_messages);
end;

Destructor TLightPascalInterpreter.Destroy;
begin
  FreeAndNil(rootNode); // contains the abstract syntax tree

  FreeAndNil(interpreter);
  FreeAndNil(parser);
  FreeAndNil(lexer);

  inherited;
end;

end.

