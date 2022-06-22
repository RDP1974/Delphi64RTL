library IsapiTest;

uses
  RDPMM64,
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
//  Web.Win.ISAPIThreadPool, comment this line, is useless with actual IIS
  WebModuleUnit in 'WebModuleUnit.pas' {WebModuleZlib: TWebModule};

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.MaxConnections:=200; // raise this number from the default 32
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
