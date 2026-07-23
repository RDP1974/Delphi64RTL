program TestWS;
// 23 july 2026 Roberto Della Pasqua - www.dellapasqua.com

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  Posix.Unistd,
  Posix.Signal,
  Posix.Pthread,
  RDPQueue64;

var
  T_Finish: Boolean = False;
var
  W_Queue: Pointer;

procedure SignalHandler(Sig: Integer); cdecl;
begin
  T_Finish := True;
  Writeln('Terminated signal');
end;

type
  TWorkerThread = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TWorkerThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Start;
end;

destructor TWorkerThread.Destroy;
begin
  inherited;
end;

procedure TWorkerThread.Execute;
var
S_Write:PString;
begin
    while not Terminated do
    begin
       New(S_Write);
       S_Write^ := 'THREAD '+ GetCurrentThreadID.ToString + ' '+FormatDateTime('hh:nn:ss.zzz',Now);
       PushToQueue(W_Queue, S_Write);
       Sleep(10);
    end;
end;

var
  W1:array[0..7] of TWorkerThread;
var
  S_Read: PString;
var
  NN:integer=0;

begin
  signal(SIGINT, @SignalHandler);   // ctrl+c
  signal(SIGTERM, @SignalHandler);  // kill / systemd stop
  signal(SIGQUIT, @SignalHandler);

 
   W_Queue := CreateQueue;
   for var i := Low(W1) to High(W1) do W1[i] := TWorkerThread.Create;
   try
    try
      while not T_Finish do
      begin
        while PopFromQueue(W_Queue, @S_Read) do
        begin
         Writeln(S_Read^);
         Dispose(S_Read);
         Inc(NN);
         if NN=10000 then
         begin
           Write(#27'[2J'#27'[H'); //clear console
           NN:=0;
         end;
        end;
        Sleep(10);
      end;

      for var i := Low(W1) to High(W1) do
      begin
        W1[i].Terminate;
        W1[i].WaitFor;
        W1[i].Free;
      end;

      Writeln('Terminated works exit');

    except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
    end;

  finally
   FreeQueue(W_Queue);
  end;
end.
