program lightpi;

// this is the main "dummy" project to test development, the interpreter code is inside ulpi.pas

{$mode objfpc}{$H+}

uses SysUtils, ulpi;

var lpi: TLightPascalInterpreter;
begin
  // see uconstants.pas for options, true enables debug output
  lpi := TLightPascalInterpreter.Create(true);
  lpi.Load(
    'for i := 1 to 5 do ' +
    'begin ' +
    '  b := b + a;' +
    '  if (b > 5) then' +
    '    writeln(i, '') b ('', b, '') is above 5!'')' +
    '  else' +
    '    writeln(i, '') b ('', b, '') is small.'');' +
    'end;'
  );

  // inject a value
  lpi.SetVariable('a', 2);

  if not lpi.Execute then WriteLn('Errors occured!');
  lpi.PrintMessages;

  // access to results
  writeln('b: ', lpi.GetVariable('b'));

  readln;

  FreeAndNil(lpi);
end.


