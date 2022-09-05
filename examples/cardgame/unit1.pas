unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin;

type
  TCard = record
    name, description, script: AnsiString;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1:TButton;
    Label1:TLabel;
    Label2:TLabel;
    ListBox1:TListBox;
    Memo1:TMemo;
    Memo2:TMemo;
    SpinEdit1:TSpinEdit;
    SpinEdit2:TSpinEdit;
    procedure Button1Click(Sender:TObject);
    procedure FormShow(Sender:TObject);
    procedure ListBox1Click(Sender:TObject);
  private
    { private declarations }
    card_no: Integer;
    procedure execute_card_script(script: AnsiString; hp, e_hp: TSpinEdit);
  public
    { public declarations }
  end;

const
  CMaxCards = 4;
  CCards: array[0..CMaxCards - 1] of TCard = (
    (name: 'Strike';
      description: 'deal 5 damage to the opponent, effect is doubled if opponent HP is above 20 HP';
      script: 'dmg := 5; if e_hp > 20 then dmg := 2*dmg; e_hp := e_hp - dmg;'),

    (name: 'Critical';
      description: 'deal 10 damage to both you and the opponent';
      script: 'hp := hp - 10; e_hp := e_hp - 10;'),

    (name: 'Heal';
      description: 'restore 10 HP for yourself';
      script: 'hp := hp + 10;'),

    (name: 'Utility';
      description: 'transfer 5 HP from the opponent to yourself';
      script: 'e_hp := e_hp - 5; hp := hp + 5;')
  );

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses ulpi;

{ TForm1 }

procedure TForm1.execute_card_script(script: AnsiString; hp, e_hp: TSpinEdit);
var lpi: TLightPascalInterpreter;
begin
  lpi := TLightPascalInterpreter.Create;

  lpi.Load(script);
  lpi.SetVariable('hp', hp.Value);
  lpi.SetVariable('e_hp', e_hp.Value);
  if lpi.Execute then
  begin
    hp.Value := lpi.GetVariable('hp');
    e_hp.Value := lpi.GetVariable('e_hp');
  end
  else ShowMessage('An Error occured during script execution!');

  FreeAndNil(lpi);
end;

procedure TForm1.Button1Click(Sender:TObject);
var e_card_no: Integer;
begin
  // your turn
  Memo1.Lines.Add('[You] Executing Card ' + CCards[card_no].name + '...');
  execute_card_script(CCards[card_no].script, SpinEdit1, SpinEdit2);

  // opponent turn, hp and e_hp are switched!
  e_card_no := random(CMaxCards);
  Memo1.Lines.Add('[Opponent] Executing Card ' + CCards[e_card_no].name + '...');
  execute_card_script(CCards[card_no].script, SpinEdit2, SpinEdit1);

  if (SpinEdit1.Value = 0) and (SpinEdit2.Value = 0) then
    ShowMessage('It''s a draw.')
  else
  if SpinEdit1.Value = 0 then
    ShowMessage('You lost.')
  else
  if SpinEdit2.Value = 0 then
    ShowMessage('You won.');
end;

procedure TForm1.FormShow(Sender:TObject);
var i: Integer;
begin
  randomize;

  // populate listbox
  for i := 0 to CMaxCards - 1 do
    ListBox1.Items.Add(CCards[i].name);

  ListBox1.ItemIndex := 0;
  ListBox1Click(Sender);
end;

procedure TForm1.ListBox1Click(Sender:TObject);
begin
  card_no := ListBox1.ItemIndex;

  Memo2.Lines.Clear;
  Memo2.Lines.Add('[ ' + CCards[card_no].name + ' ]');
  Memo2.Lines.Add(CCards[card_no].description);
end;

end.

