unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ulpi;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1:TButton;
    Chart1:TChart;
    Chart1FuncSeries1:TFuncSeries;
    Edit1:TEdit;
    procedure Button1Click(Sender:TObject);
    procedure Chart1FuncSeries1Calculate(const AX:Double; out AY:Double);
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
  private
    { private declarations }
    lpi: TLightPascalInterpreter;

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender:TObject);
begin
  lpi := TLightPascalInterpreter.Create;
end;

procedure TForm1.Button1Click(Sender:TObject);
begin
  lpi.Load('y := ' + Edit1.Text + ';');

  if lpi.isError then
    Edit1.Color := clRed
  else
    Edit1.Color := clDefault;

  Chart1FuncSeries1.Active := true;
  Chart1.Invalidate;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX:Double; out AY:Double);
begin
  lpi.SetVariable('x', AX);

  if lpi.Execute then
    AY := lpi.GetVariable('y')
  else
    AY := AX;
end;

procedure TForm1.FormDestroy(Sender:TObject);
begin
  FreeAndNil(lpi);
end;

end.

