unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  SA: string = 'abcdefghilmnopqrstuvz';
  SB: string = 'abcd';
  SC: string = 'ghi';
  SD: string = 'uvz';

  WA: widestring = 'abcdefghilmnopqrstuvz';
  WB: widestring = 'abcd';
  WC: widestring = 'ghi';
  WD: widestring = 'uvz';

  RA: rawbytestring = 'abcdefghilmnopqrstuvz';
  RB: rawbytestring = 'abcd';
  RC: rawbytestring = 'ghi';
  RD: rawbytestring = 'uvz';

procedure TForm1.Button1Click(Sender: TObject);
var
SZ : string;
WZ : widestring;
RZ : rawbytestring;

begin
  Memo1.Lines.Add(IntToStr(Pos(SB, SA)));
  Memo1.Lines.Add(IntToStr(Pos(SC, SA)));
  Memo1.Lines.Add(IntToStr(Pos(SD, SA)));
  SZ:= SA + 'uvz';
  Memo1.Lines.Add(IntToStr(Pos(SD, SZ, 20)));
  Memo1.Lines.Add('');

  Memo1.Lines.Add(IntToStr(Pos(WB, WA)));
  Memo1.Lines.Add(IntToStr(Pos(WC, WA)));
  Memo1.Lines.Add(IntToStr(Pos(WD, WA)));
  WZ:= WA + 'uvz';
  Memo1.Lines.Add(IntToStr(Pos(WD, WZ, 20)));
  Memo1.Lines.Add('');

  Memo1.Lines.Add(IntToStr(Pos(RB, RA)));
  Memo1.Lines.Add(IntToStr(Pos(RC, RA)));
  Memo1.Lines.Add(IntToStr(Pos(RD, RA)));
  RZ:= RA + 'uvz';
  Memo1.Lines.Add(IntToStr(Pos(RD, RZ, 20)));
  Memo1.Lines.Add('');
end;

end.
