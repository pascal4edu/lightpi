unit uerror;

{$mode objfpc}{$H+}

interface

uses Classes;

type TLPI_ErrorMessage = class(TObject)
       public
         messages: TStringList;
         isError: Boolean;

         Constructor Create; virtual;
         procedure LogError(s: AnsiString; line, operation: Integer);
         Destructor Destroy; override;
     end;

implementation

uses sysutils, uconstants, uutils;

Constructor TLPI_ErrorMessage.Create;
begin
  isError := false;
  messages := TStringList.Create;
end;

procedure TLPI_ErrorMessage.LogError(s: AnsiString; line, operation: Integer);
var error_message: AnsiString;
begin
  // only log the first error?
  if (ClpiDebugReportOnlyFirstError) and (isError) then Exit;

  error_message := 'Error! ' + s;

  if line <> -1 then
    error_message := error_message + ' at Line ' + IntToStr(line);

  if operation <> -1 then
    error_message := error_message + ' for Operation ' + operationtostr(operation);

  messages.Add(error_message);

  isError := true;
end;

Destructor TLPI_ErrorMessage.Destroy;
begin
  FreeAndNil(messages);

  inherited;
end;

end.

