program web_pascal;

{$mode objfpc}{$H+}

uses
  fphttpapp, Unit1;

begin
  Application.Port := 8080;
  Application.LegacyRouting := True; // needed for Lazarus 3.x to work
  Application.Initialize;
  Application.Run;
end.
