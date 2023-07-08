program test_suite;

{$mode objfpc}{$H+}

uses sysutils, ulpi;

var lpi: TLightPascalInterpreter;
    test_total, test_failed: Integer;

function run_testcase(sCode: AnsiString; sExpectedResult: Variant): Boolean;
begin
  Result := False;

  if not lpi.Load(sCode) then Exit;
  if not lpi.Execute then Exit;
  if not (lpi.GetVariable('x') = sExpectedResult) then
  begin
    writeln('testcase expected ', sExpectedResult, ', code returned ', lpi.GetVariable('x'));
    Exit;
  end;

  Result := not lpi.isError;
end;

// wrapper function
procedure run_test(sCode: AnsiString; sExpectedResult: Variant);
begin
  inc(test_total);

  if not run_testcase(sCode, sExpectedResult) then
  begin
    inc(test_failed);
    writeln(sCode, ' failed.');
  end;

  lpi.PrintMessages(True);
end;

procedure testsuite_math;
begin
  writeln('### Math');

  run_test('x := 3+5;', 8);
  run_test('x := +5+5;', 10);
  run_test('x := -5+5;', 0);
  run_test('x := +5-5;', 0);
  run_test('x := -5-5;', -10);

  run_test('x := -2-+1;', -3);
  run_test('x := -2+-1;', -3);
  run_test('x := -2--1;', -1);
  run_test('x := +2-+1;', 1);
  run_test('x := +2+-1;', 1);
  run_test('x := +2--1;', 3);

  run_test('x := +3+3*2;', 9);
  run_test('x := -3*2+3;', -3);
  run_test('x := +3-3*2;', -3);
  run_test('x := -3*2-3;', -9);

  run_test('x := +2*(3+3);', 12);
  run_test('x := -3*(2+3);', -15);
  run_test('x := +(3-2)*2;', 2);
  run_test('x := -(2-3)*3;', 3);

  run_test('x := +4/2*(3+9/3);', 12);
  run_test('x := -9/3*(2+9/3);', -15);
  run_test('x := +(3-4/2)*2;', 2);
  run_test('x := -(2-9/3)*3;', 3);

  run_test('x := 1e3 + 1.2e+1 + 5E-3;', 1012.005);
  run_test('x := $FF;', 255);
  run_test('x := $ABCDEF;', 11259375);
  run_test('x := $0A + +$F0;', 250);
  run_test('x := $0A + -$F0;', -230);
end;

procedure testsuite_variables;
begin
  writeln('### Variables');

  run_test('x := x;', 0);
  run_test('x := +x;', 0);
  run_test('x := -x;', 0);
  run_test('x := --x;', 0);

  run_test('x := ''abc''; X := 5;', 5);

  run_test('a := 3; b := 5; x := a + b;', 8);
  run_test('a := ''3''; b := 5; x := a + b;', '35');
  run_test('a := 3; b := ''5''; x := a + b;', '35');
  run_test('a := ''3''; b := ''5''; x := a + b;', '35');
end;

procedure testsuite_logic;
begin
  writeln('### Logic');

  run_test('x := true;', true);
  run_test('x := false;', false);
  run_test('x := not true;', false);
  run_test('x := not false;', true);
  run_test('x := not not true;', true);
  run_test('x := not not false;', false);

  run_test('x := true = true;', true);
  run_test('x := false = true;', false);
  run_test('x := 5 = 3;', false);
  run_test('x := not 5 = 3;', true);
  run_test('x := 3 = 3;', true);
  run_test('x := not 3 = 3;', false);

  run_test('x := true <> true;', false);
  run_test('x := false <> true;', true);
  run_test('x := 5 <> 3;', true);
  run_test('x := not 5 <> 3;', false);
  run_test('x := 3 <> 3;', false);
  run_test('x := not 3 <> 3;', true);

  run_test('x := 5 > 3;', true);
  run_test('x := 3 > 5;', false);
  run_test('x := +5 > -3;', true);
  run_test('x := -5 > +3;', false);
  run_test('x := -3 > -5;', true);
  run_test('x := 3 > 3;', false);
  run_test('x := -3 > -3;', false);
  run_test('x := 3 - 5 > 4 + 6;', false);
  run_test('x := 3 - (5 > 4) + 6;', 10); // (5 > 4) is converted to -1 inside the variant

  run_test('x := 5 < 3;', false);
  run_test('x := 3 < 5;', true);
  run_test('x := +5 < -3;', false);
  run_test('x := -5 < +3;', true);
  run_test('x := -3 < -5;', false);
  run_test('x := 3 < 3;', false);
  run_test('x := -3 < -3;', false);
  run_test('x := 3 - 5 < 4 + 6;', true);
  run_test('x := 3 - (5 < 4) + 6;', 9); // (5 < 4) is converted to 0 inside the variant

  run_test('x := 5 >= 3;', true);
  run_test('x := 3 >= 5;', false);
  run_test('x := +5 >= -3;', true);
  run_test('x := -5 >= +3;', false);
  run_test('x := -3 >= -5;', true);
  run_test('x := 3 >= 3;', true);
  run_test('x := -3 >= -3;', true);
  run_test('x := 3 - 5 >= 4 + 6;', false);
  run_test('x := 3 - (5 >= 4) + 6;', 10); // (5 >= 4) is converted to -1 inside the variant

  run_test('x := 5 <= 3;', false);
  run_test('x := 3 <= 5;', true);
  run_test('x := +5 <= -3;', false);
  run_test('x := -5 <= +3;', true);
  run_test('x := -3 <= -5;', false);
  run_test('x := 3 <= 3;', true);
  run_test('x := -3 <= -3;', true);
  run_test('x := 3 - 5 <= 4 + 6;', true);
  run_test('x := 3 - (5 <= 4) + 6;', 9); // (5 <= 4) is converted to 0 inside the variant
end;

procedure testsuite_structures;
begin
  writeln('### Structures');

  run_test('x := true; if x then x := 3;', 3);
  run_test('x := false; if x then x := 3;', false);
  run_test('x := true; if x then x := 3 else x := 2;', 3);
  run_test('x := true; if not x then x := 3 else x := 2;', 2);
  run_test('if true then if not true then x := 3 else x := 2;', 2);
  run_test('if true then if not true then x := 3 else x := 5 else x := 2;', 5);
  run_test('if false then if not true then x := 3 else x := 5 else x := 2;', 2);

  run_test('while (x < 5) do x := x + 1;', 5);
  run_test('while (x > 5) do x := x + 1;', 0);
  run_test('while (x < 5) do while (x < 10) do x := x + 1;', 10);

  run_test('repeat x := x + 1; until x >= 5;', 5);
  run_test('repeat y := 1; repeat y := y + 1; x := x + y; until y > 2; until x > 12;', 15);

  run_test('for x := 1 to 5 do begin end;', 5);
  run_test('for x := 1 to 5 do x := x + 3;', 8); // behaviour sligthly differs from pascal (last value of 5 + 3)
  run_test('for a := 1 to 3 do for b := 1 to 2 do x := x + 1;', 6);
  run_test('for a := 1 to 3 do for a := 1 to 2 do x := x + 1;', 6); // for loop stores iterator internally!
end;

procedure testsuite_complex;
begin
  writeln('### Complex Examples');

  // greatest common divisor
  run_test(
    'a := 128; b := 24; i := 1;' + #13#10 +
    'while (i < a) or (i < y) do' + #13#10 +
    'begin' + #13#10 +
    '  if (a mod i = 0) and (b mod i = 0) then x := i;' + #13#10 +
    '  i := i + 1;' + #13#10 +
    'end;', 8);

  // least common multiple
  run_test(
    'a := 15; b := 21; i := a;' + #13#10 +
    'while (i < a*b) and (x = 0) do' + #13#10 +
    'begin' + #13#10 +
    '  if (i mod a = 0) and (i mod b = 0) then x := i;' + #13#10 +
    '  i := i + 1;' + #13#10 +
    'end;', 105);

  // square root with Heron
  run_test(
    'y := 2; a := 1; b := y;' + #13#10 +
    'repeat' + #13#10 +
    '  b := (a + b) / 2;' + #13#10 +
    '  a := y / b;' + #13#10 +
    'until b - a < 0.1;' + #13#10 +
    'x := b; // square root' + #13#10 +
    'x := round(x*x*1000);', 2007);
end;

begin
  lpi := TLightPascalInterpreter.Create;
  test_total := 0;
  test_failed := 0;

  testsuite_math;
  testsuite_variables;
  testsuite_logic;
  testsuite_structures;
  testsuite_complex;

  FreeAndNil(lpi);

  writeln;
  write('Test suite done. ', test_total, ' total, ');
  if test_failed > 0 then
    writeln(test_failed, ' failed.')
  else
    writeln('all passed.');

  readln;
end.

