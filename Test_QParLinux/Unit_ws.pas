unit Unit_ws;

interface

uses
  System.SysUtils, System.Classes, ipwcore, ipwtypes, ipwwsserver, System.Types,
  System.Threading, System.Generics.Collections;

type
  TDM = class(TDataModule)
    WS: TipwWSServer;
    procedure WSDataIn(Sender: TObject; ConnectionId, DataFormat: Integer;
      const Text: string; const TextB: TBytes; EOM, EOL: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure WSConnectionRequest(Sender: TObject; const Address: string;
      Port: Integer; var Accept: Boolean);
    procedure WSConnected(Sender: TObject; ConnectionId: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;
var
  CQueue: TThreadedQueue<PString>;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
WS.StartListening;
Writeln('Start listening');
end;

procedure TDM.WSConnected(Sender: TObject; ConnectionId: Integer);
begin
Writeln('Connected '+ConnectionId.ToString);
end;

procedure TDM.WSConnectionRequest(Sender: TObject; const Address: string;
  Port: Integer; var Accept: Boolean);
begin
Writeln('Request '+Address+'_'+Port.ToString);
end;

procedure TDM.WSDataIn(Sender: TObject; ConnectionId, DataFormat: Integer;
  const Text: string; const TextB: TBytes; EOM, EOL: Boolean);
var
S:PString;
begin
WS.SendText(ConnectionId,'TESTO DI ESEMPIO '+FormatDateTime('ddmmyyyy hh:nn:ss',Now));
New(S);
S^:='TEST';
CQueue.PushItem(S);
if CQueue.PopItem(S) = TWaitResult.wrSignaled then
begin
Writeln(S^);
Dispose(S);
end;
end;

initialization
CQueue := TThreadedQueue<PString>.Create(1000,0,0);

end.
