unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit, SynHighlighterPas;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonExec: TButton;
    CB_AutoExec: TCheckBox;
    CB_Debug:TCheckBox;
    Memo1: TMemo;
    SynEdit1:TSynEdit;
    SynPasSyn1:TSynPasSyn;
    Timer_auto_execute: TTimer;
    procedure FormShow(Sender:TObject);
    procedure CB_DebugChange(Sender:TObject);
    procedure CB_AutoExecChange(Sender:TObject);
    procedure Timer_auto_executeTimer(Sender:TObject);
    procedure ButtonExecClick(Sender: TObject);
  private
    isExecuting: Boolean;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses ulpi;

{ TForm1 }

procedure TForm1.FormShow(Sender:TObject);
begin
  isExecuting := false;
  SynEdit1.SetFocus;
end;

procedure TForm1.CB_DebugChange(Sender:TObject);
begin
  ButtonExecClick(Sender);
end;

procedure TForm1.CB_AutoExecChange(Sender:TObject);
begin
  Timer_auto_execute.Enabled := CB_AutoExec.Checked;
end;

procedure TForm1.Timer_auto_executeTimer(Sender:TObject);
begin
  ButtonExecClick(Sender);
end;

procedure TForm1.ButtonExecClick(Sender: TObject);
var i: Integer;
    lpi: TLightPascalInterpreter;
    code: AnsiString;
    messages: TStringList;

begin
  // check if we are already running
  if isExecuting then
    Exit
  else
    isExecuting := true;

  // glue source editor lines together
  code := '';
  for i := 0 to SynEdit1.Lines.Count - 1 do
    code := code + SynEdit1.Lines[i] + LineEnding;

  // create and launch the interpreter
  lpi := TLightPascalInterpreter.Create(CB_Debug.Checked);
  lpi.Load(code);
  lpi.Execute;

  // print out any messages
  Memo1.Lines.Clear;
  Memo1.Lines.Add('--- Processing Code ' + DateTimeToStr(Now()));

  messages := lpi.GetMessages;
  for i := 0 to messages.Count - 1 do
    Memo1.Lines.Add(messages[i]);

  // clean up :-)
  FreeAndNil(messages);
  FreeAndNil(lpi);

  isExecuting := false;
end;

end.

