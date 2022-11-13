unit uinterpreter;

{$mode objfpc}{$H+}

interface

uses Classes, Variants, SysUtils, utypes, uerror;

type Tvariables = array of Variant;
     TLPI_Interpreter = class(TLPI_ErrorMessage)
       private
         variables: Tvariables;

         procedure dump_variables(symbols: TStringList);

       public
         procedure init_variables(varcount: Integer);
         function get_variable(var_symbol: AnsiString; symbols: TStringList): Variant;
         procedure set_variable(var_symbol: AnsiString; var_value: Variant; symbols: TStringList);
         function evaluate(node: TASTTreeNode): Variant;
         procedure execute(rootNode: TASTTreeNode; symbols: TStringList);
     end;

implementation

uses math, uutils, uconstants;

procedure TLPI_Interpreter.init_variables(varcount: Integer);
var i: Integer;
begin
  variables := Tvariables.Create;
  SetLength(variables, varcount);

  for i := 0 to High(variables) do
    variables[i] := 0;
end;

procedure TLPI_Interpreter.dump_variables(symbols: TStringList);
var i, max_name_length: Integer;
begin
  if variables = nil then Exit;

  // find longest name
  max_name_length := 0;
  for i := 0 to High(variables) do
    max_name_length := max(max_name_length, Length(symbols[i]));

  for i := 0 to High(variables) do
    messages.Add(padstring(symbols[i], max_name_length) + ' := ' + AnsiString(variables[i]));
end;

function TLPI_Interpreter.get_variable(var_symbol: AnsiString; symbols: TStringList): Variant;
var i: Integer;
    found: Boolean;
begin
  found := false;
  Result := '';
  var_symbol := LowerCase(var_symbol);

  for i := 0 to High(variables) do
    if LowerCase(symbols[i]) = var_symbol then
    begin
      Result := variables[i];
      found := true;
      Break;
    end;

  // not found? set error state
  if not found then
    LogError('Requested Variable ' + var_symbol + ' could not be found!', -1, -1);
end;

procedure TLPI_Interpreter.set_variable(var_symbol: AnsiString; var_value: Variant; symbols: TStringList);
var i: Integer;
    found: Boolean;
begin
  found := false;
  var_symbol := LowerCase(var_symbol);

  for i := 0 to High(variables) do
    if LowerCase(symbols[i]) = var_symbol then
    begin
      variables[i] := var_value;
      found := true;
      Break;
    end;

  // not found? add the symbol + value
  if not found then
  begin
    SetLength(variables, Length(variables) + 1); // expand array
    variables[High(variables)] := var_value; // set the last value
    symbols.Add(var_symbol); // also add a matching symbol
  end;
end;

function TLPI_Interpreter.evaluate(node: TASTTreeNode): Variant;
var i, loopCycles, for_from, for_to: Integer;
    temp: Extended;
    s: AnsiString;
    var_a, var_b: Variant;
begin
  Result := 0;

  if (node = nil) or (isError) then Exit;

  case node.operation of
    CopNOP: begin {No Operation...} end;

    CopVariable:
      Result := variables[node.var_id];

    CopNumber:
      Result := node.n;

    CopString:
      Result := node.s;

    CopAssign:
      variables[node.firstChild.var_id] := evaluate(node.secondChild);

    CopCompareEqual:
      Result := evaluate(node.firstChild) = evaluate(node.secondChild);

    CopCompareUnequal:
      Result := evaluate(node.firstChild) <> evaluate(node.secondChild);

    CopCompareGreater:
      Result := Extended(evaluate(node.firstChild)) > Extended(evaluate(node.secondChild));

    CopCompareSmaller:
      Result := Extended(evaluate(node.firstChild)) < Extended(evaluate(node.secondChild));

    CopCompareGreaterOrEqual:
      Result := Extended(evaluate(node.firstChild)) >= Extended(evaluate(node.secondChild));

    CopCompareSmallerOrEqual:
      Result := Extended(evaluate(node.firstChild)) <= Extended(evaluate(node.secondChild));

    CopLogicTrue:
      Result := True;

    CopLogicFalse:
      Result := False;

    CopLogicAnd:
      Result := Boolean(evaluate(node.firstChild)) and Boolean(evaluate(node.secondChild));

    CopLogicOr:
      Result := Boolean(evaluate(node.firstChild)) or Boolean(evaluate(node.secondChild));

    CopLogicNot:
      Result := not Boolean(evaluate(node.firstChild));

    CopMathAdd:
      begin
        var_a := evaluate(node.firstChild);
        var_b := evaluate(node.secondChild);

        if (varType(var_a) = varString) or (varType(var_b) = varString) then
          Result := AnsiString(var_a) + AnsiString(var_b)
        else
          Result := Extended(var_a) + Extended(var_b);
      end;

    CopMathSub:
      Result := evaluate(node.firstChild) - evaluate(node.secondChild);

    CopMathMul:
      Result := evaluate(node.firstChild) * evaluate(node.secondChild);

    CopMathDiv:
      begin
        temp := evaluate(node.secondChild);

        if temp = 0 then
          LogError('Division by Zero', node.line, node.operation)
        else
          Result := evaluate(node.firstChild) / temp;
      end;

    CopMathDivInt:
      begin
        temp := round(StrToFloat(evaluate(node.secondChild)));

        if temp = 0 then
          LogError('Division by Zero', node.line, node.operation)
        else
          Result := round(StrToFloat(evaluate(node.firstChild))) div round(temp);
      end;

    CopMathMod:
      begin
        temp := evaluate(node.secondChild);

        if temp = 0 then
          LogError('Modulo by Zero', node.line, node.operation)
        else
          Result := round(StrToFloat(evaluate(node.firstChild))) mod round(temp);
      end;

    CopCommandBlock:
        for i := 0 to node.children.Count - 1 do evaluate(node.getChild(i));

    CopCondition:
      begin
        if Boolean(evaluate(node.firstChild)) then
          evaluate(node.secondChild)
        else
        begin
          // only execute else node if it's present
          if node.children.Count > 2 then
            evaluate(node.thirdChild);
        end;
      end;

    CopLoopWhile:
      begin
        loopCycles := 0;
        while ((Boolean(evaluate(node.firstChild))) and (loopCycles < ClpiInterpreterMaxLoopCycles) and (not isError)) do
        begin
          evaluate(node.secondChild);
          inc(loopCycles);
        end;
        if (loopCycles >= ClpiInterpreterMaxLoopCycles) then
          LogError('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(ClpiInterpreterMaxLoopCycles), node.line, CopLoopWhile);
      end;

    CopLoopRepeatUntil:
      begin
        loopCycles := 0;
        repeat
          // Count - 2 because last Child = condition!
          for i := 0 to node.children.Count - 2 do evaluate(node.getChild(i));

          inc(loopCycles);
        until (Boolean(evaluate(node.lastChild))) or (loopCycles >= ClpiInterpreterMaxLoopCycles) or (isError);
        if (loopCycles >= ClpiInterpreterMaxLoopCycles) then
          LogError('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(ClpiInterpreterMaxLoopCycles), node.line, CopLoopRepeatUntil);
      end;

    CopLoopForTo:
      begin
        for_from := round(StrToFloat(evaluate(node.secondChild)));
        for_to := round(StrToFloat(evaluate(node.thirdChild)));
        if ((for_to - for_from) >= ClpiInterpreterMaxLoopCycles) then
          LogError('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(ClpiInterpreterMaxLoopCycles), node.line, CopLoopForTo);

        for i := for_from to for_to do
        begin
          variables[node.firstChild.var_id] := i;
          evaluate(node.fourthChild);
        end;
      end;

    CopRound:
      Result := round(StrToFloat(evaluate(node.firstChild)));

    CopWriteln:
      begin
        s :='';
        for i := 0 to node.children.Count - 1 do
          s := s + AnsiString(evaluate(node.getChild(i)));

          messages.Add(s);
      end;

    else
      LogError('Unknown Operation ' + operationtostr(node.operation), node.line, node.operation);
  end;
end;


procedure TLPI_Interpreter.execute(rootNode: TASTTreeNode; symbols: TStringList);
begin
  if ClpiDebugMode then
  begin
    messages.Add('');
    messages.Add('*** Interpreter ***');
  end;

  if rootNode = nil then Exit;

  evaluate(rootNode);

  if ClpiDebugMode then
  begin
    if Length(variables) > 0 then
    begin
      messages.Add('');
      messages.Add('# Variable State Dump');

      dump_variables(symbols);
    end;

    messages.Add('');
    messages.Add('# Execution halted.');
  end;
end;

end.

