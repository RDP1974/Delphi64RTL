program TestQ;

uses
  RDPMM64,
  RDPQueue64,
  Vcl.Forms,
  UnitQ in 'UnitQ.pas' {FormQ};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormQ, FormQ);
  Application.Run;
end.
