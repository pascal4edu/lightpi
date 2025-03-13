unit Unit1;

{ ++++++++++++++++++++++++++++++++++++++++++++++++

  o install weblaz package
  o go to http://localhost:8080
  o ignore any HTTP related exceptions

  ++++++++++++++++++++++++++++++++++++++++++++++++
}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure TFPWebAction0Request(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FPWebModule1: TFPWebModule1;

implementation

uses ulpi;

{$R *.lfm}

{ TFPWebModule1 }

const
  CHTML_Template =
    '<html><body><form action="">' +
    '<textarea id="code" name="code" rows="10" cols="80">$c</textarea><br>' +
    '<input type="submit" value="Submit">' +
    '</form><hr><pre>$v</pre><body><html>';

procedure TFPWebModule1.TFPWebAction0Request(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
var
  lpi: TLightPascalInterpreter;
  s_code, s_response: AnsiString;
begin
  writeln('Request: ', ARequest.URL);

  s_code := ARequest.QueryFields.Values['code'];
  if s_code = '' then
    s_code := 'for i := 1 to 5 do writeln(i, ''x Hello World!'');';

  lpi := TLightPascalInterpreter.Create;

  lpi.Load(s_code);

  lpi.Execute();

  s_response := CHTML_Template;
  s_response := StringReplace(s_response, '$c', s_code, [rfReplaceAll, rfIgnoreCase]);
  s_response := StringReplace(s_response, '$v', lpi.GetMessages(true).Text, [rfReplaceAll, rfIgnoreCase]);

  AResponse.Content := s_response;

  FreeAndNil(lpi);

  Handled := True;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);

  writeln('Go to http://localhost:8080');
end.
