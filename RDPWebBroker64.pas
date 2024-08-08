unit RDPWebBroker64;

// 28 febr 2019 Roberto Della Pasqua www.dellapasqua.com
// for better performances put RDPMM64 as first unit clause in project source
// define SEAZLIB for deflate with intel ipp performance libraries
// 26 febr 2020 buffer len check
// 20 febr 2023 updated zlibdeflate for correct mime
// be careful to set the correct content-type with utf-8 charset

{$DEFINE SEAZLIB}

interface

uses Windows, Sysutils, System.Classes, Web.HTTPApp, Web.WebReq;

type
  TWBHelper = class helper for TWebResponse
  public
    procedure ZlibDeflate; overload;
  end;

implementation

uses RDPZlib64;

procedure TWBHelper.ZlibDeflate; // compress utf-8 text and json
var
  ZBuff: TMemoryStream;
  Src, Dst: TBytes;
  Cl: string;
begin
  Cl := Lowercase(ContentType); // example ContentType := 'application/json; charset="UTF-8"';
  if ((Pos('text', Cl) > 0) or (Pos('json', Cl) > 0) and (Pos('utf-8', Cl) > 0)) then
  begin
    if ContentStream = nil then // use content string
    begin
      if (Length(Content) > 1500) and (Length(Content) < 10000000) then // 10MB
      begin
        Src := TEncoding.UTF8.GetBytes(Content);
        SeaZlib.Compress(Src, Dst);
        ZBuff := TMemoryStream.Create;
        ZBuff.Write(Dst, Length(Dst));
        ContentStream := ZBuff;
        ContentStream.Seek(0,0);
        ContentEncoding := 'deflate';
        ContentLength := ContentStream.Size;
      end;
    end
    else if (ContentStream.Size > 1500) and (ContentStream.Size < 10000000) then // 10MB
    begin
      ZBuff := TMemoryStream.Create;
      SeaZlib.CompressStream(ContentStream, ZBuff, Z_BEST_SPEED_AC);
      ContentStream.Size := ZBuff.Size;
      ContentStream.Write(ZBuff, ZBuff.Size);
      ContentEncoding := 'deflate';
      ContentLength := ContentStream.Size;
      ZBuff.Free;
    end;
  end;
end;

end.
