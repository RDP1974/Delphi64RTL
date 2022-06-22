unit RDPWebBroker64;

// 28 Febr 2019 Roberto Della Pasqua www.dellapasqua.com
// for better performances put RDPMM64 as first unit clause in project source
// define SEAZLIB for compression realtime speed up
// 26 Febr 2020 buffer len check

{$DEFINE SEAZLIB}

interface

uses Windows, Sysutils, System.Classes, Web.HTTPApp, Web.WebReq;

type
  TWBHelper = class helper for TWebResponse
  public
    procedure ZlibDeflate; overload;
    procedure ZlibDeflate(const Src:TMemoryStream); overload;
  end;

implementation

{$IFDEF SEAZLIB}
uses RDPZlib64;
{$ELSE}
uses System.Zlib;
{$ENDIF}

procedure TWBHelper.ZlibDeflate;
var
  ZBuff: TMemoryStream;
  Src, Dst: TBytes;
begin
  if ContentStream = nil then // allora accedere a content string
  begin
    if (Length(Content) > 1500) and (Length(Content) < 10000000) then
    begin
      Src := TEncoding.UTF8.GetBytes(Content);
      SeaZlib.Compress(Src, Dst);
      ZBuff := TMemoryStream.Create;
      ZBuff.Write(Dst, Length(Dst));
      ContentStream := ZBuff;
      ContentStream.Seek(0, 0);
      ContentEncoding := 'deflate';
      ContentLength := ZBuff.Size;
      (* Src := TEncoding.UTF8.GetBytes(Content);
        ZBuffStr:=TMemoryStream.Create;
        ZBuffStr.Write(Src, Length(Src));
        ZBuffStr.Seek(0,0);
        ZBuff := TMemoryStream.Create;
        SeaZlib.CompressStream(ZBuffStr, ZBuff, Z_BEST_SPEED_AC);
        ZBuffStr.Free;
        ContentStream := ZBuff;
        ContentStream.Seek(0, 0);
        ContentEncoding := 'deflate';
        ContentLength := ZBuff.Size; *)
    end;
  end
  else if (ContentStream.Size > 1500) and (ContentStream.Size < 10000000) then
  begin
    ZBuff := TMemoryStream.Create;
    ContentStream.Seek(0, 0);
{$IFDEF SEAZLIB}
    SeaZlib.CompressStream(ContentStream, ZBuff, Z_BEST_SPEED_AC);
{$ELSE}
    ZCompressStream(ContentStream, ZBuff, zcFastest);
{$ENDIF}
    ContentStream.Free;
    ContentStream := ZBuff;
    ContentStream.Seek(0, 0);
    ContentEncoding := 'deflate';
    ContentLength := ZBuff.Size;
  end;
end;

procedure TWBHelper.ZlibDeflate(const Src:TMemoryStream);
{$IFDEF SEAZLIB}
var
  Helper:TMemoryStream;
begin
  if (Src.Size > 1500) and (Src.Size < 10000000) then
  begin
    Src.Seek(0, 0);
    Helper:=TMemoryStream.Create;
    SeaZlib.CompressStream(Src, Helper, Z_BEST_SPEED_AC);
    ContentStream := Helper;
    ContentStream.Seek(0, 0);
    ContentEncoding := 'deflate';
    ContentLength := Helper.Size;
  end;
  {$ELSE}
var
  HelperTZ:TZCompressionStream;
  Helper:TMemoryStream;
begin
  if (Src.Size > 1500) and (Src.Size < 10000000) then
  begin
    Src.Seek(0, 0);
    Helper:=TMemoryStream.Create;
    HelperTZ:=TZCompressionStream.Create(Helper, zcFastest, -15);
    HelperTZ.CopyFrom(Src, Src.Size);
    HelperTZ.Free;
    ContentStream := Helper;
    ContentStream.Seek(0, 0);
    ContentEncoding := 'deflate';
    ContentLength := Helper.Size;
  end;
{$ENDIF}

end;

end.
