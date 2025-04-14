unit UnitQ;
// Roberto Della Pasqua www.dellapasqua.com
// 14 apr 2024 thread safe concurrent queue from Intel One Api v2022.1
// little test with strings

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormQ = class(TForm)
    btn_testQ: TButton;
    procedure btn_testQClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormQ: TFormQ;

implementation

{$R *.dfm}

uses RDPQueue64;

procedure TFormQ.btn_testQClick(Sender: TObject);
var
  Queue: pointer;
  S: PString;
  A,B: integer;
begin
  Queue := CreateQueue;
  B:=GetTickCount64;
  for A := 0 to 1000000 do
  begin
   New(S);
   S^ := 'CIAO THREAD '+ A.ToString;
   PushToQueue(Queue, S);
  end;
  for A := 0 to 1000000 do
  if PopFromQueue(Queue, @S) then
  begin
// ShowMessage(S^);
   Dispose(S);
  end;
  ShowMessage('Done: ' + (GetTickCount64-B).ToString);
  FreeQueue(Queue);
end;

end.
