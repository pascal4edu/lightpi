unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ButtonClear: TButton;
    ButtonExecute: TButton;
    Edit1: TEdit;
    procedure add_caption(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses ulpi;

procedure TForm1.add_caption(Sender: TObject);
begin
  Edit1.Text := Edit1.Text + TButton(Sender).Caption;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  Edit1.Text := '';
end;

// probably overkill to use a pascal interpreter for this, but... it works! :-)
procedure TForm1.ButtonExecuteClick(Sender: TObject);
var lpi: TLightPascalInterpreter;
begin
  lpi := TLightPascalInterpreter.Create;

  lpi.Load('x := ' + Edit1.Text + ';');

  if lpi.Execute then
    Edit1.Text := String(lpi.GetVariable('x'))
  else
    Edit1.Text := 'Error';

  FreeAndNil(lpi);
end;

end.

