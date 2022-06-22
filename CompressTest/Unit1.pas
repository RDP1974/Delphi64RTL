unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
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

//uses System.Zlib;
uses RDPZlib64;

procedure TForm1.Button1Click(Sender: TObject);
var
T,T2:TMemoryStream;
A,B:integer;
P1,P2:Pointer;
E1,E2:TBytes;
begin
T:=TMemoryStream.Create;
T.LoadFromFile('C:\Chrome.7z');
T.Seek(0,0);
GetMem(P1,T.Size);
T.Read(P1^, T.Size);
SetLength(E1,T.Size);
Move(PByte(P1)^,E1[0], T.Size);
SeaZlib.Compress(P1,P2,T.Size,A);
ShowMessage(A.ToString);

SetLength(E2,0);
SeaZlib.Compress(E1,E2);
ShowMessage(IntToStr(Length(E2)));

FreeMem(P1);
SeaZlib.Decompress(P2,P1,A,B);
ShowMessage(B.ToString);

SetLength(E1,0);
SeaZlib.Decompress(E2,E1);
ShowMessage(IntToStr(Length(E1)));

T2:= TMemoryStream.Create;
T.Seek(0,0);
SeaZlib.CompressStream(T,T2);
T2.SaveToFile('C:\Chrome.saved');
T.Free;
T2.Free;

T:=TMemoryStream.Create;
T.LoadFromFile('C:\Chrome.saved');
T2:= TMemoryStream.Create;
SeaZlib.DecompressStream(T,T2);
T2.SaveToFile('C:\Chrome.orig');
ShowMessage(IntToStr(T2.Size));

T.Free;
T2.Free;
end;

end.
