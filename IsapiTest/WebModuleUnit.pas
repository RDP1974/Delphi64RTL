unit WebModuleUnit;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebModuleZlib = class(TWebModule)
    procedure WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleZlib;

implementation

uses RDPWebBroker64;

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TWebModuleZlib.WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.ZlibDeflate;
end;

end.
