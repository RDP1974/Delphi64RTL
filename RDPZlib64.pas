unit RDPZlib64;

// 28 febr 2019 Roberto Della Pasqua www.dellapasqua.com
// 3 march 2020 added inflate support

interface

uses
  System.SysUtils, System.Classes;

const
  ZLIBDLL = 'SeaZIP.dll';
  ZLIB_VERSION: PAnsiChar = '1.2.11';
  ZLIB_VERNUM = $1280;
  Z_FINISH = 4;
  Z_NO_FLUSH = 0;
  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_ERRNO = (-1);
  Z_BUF_ERROR = (-5);
  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED_AC = -2; // intel zlib optimize
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = 4;
  Z_DEFLATED = 8;
  Z_MAX_WBITS = -15;
  Z_MEM_LEVEL = 9;
  Z_DEFAULT_STRATEGY = 0;

const
  Z_errmsg: array [0 .. 9] of string = ('need dictionary', 'stream end', '', 'file error', 'stream error', 'data error', 'insufficient memory', 'buffer error', 'incompatible version', '');

type
  EZCompressionError = class(Exception);

  z_stream = record // sizeof 88
    next_in: PByte;
    avail_in: Cardinal;
    total_in: Cardinal;
    next_out: PByte;
    avail_out: Cardinal;
    total_out: Cardinal;
    msg: MarshaledAString;
    state: Pointer;
    zalloc: Pointer;
    zfree: Pointer;
    opaque: Pointer;
    data_type: Integer;
    adler: Cardinal;
    reserved: Cardinal;
  end;

type
  SeaZlib = class
    // deflate
    class procedure Compress(const Src: Pointer; out Dest: Pointer; SrcSize: Integer; out DestSize: Integer; Level: Integer = Z_BEST_SPEED_AC); overload; // default -2 accelerated
    class procedure Compress(const Src: TBytes; out Dest: TBytes; Level: Integer = Z_BEST_SPEED_AC); overload;
    class procedure CompressStream(Src: TStream; Dest: TStream; Level: Integer = Z_BEST_SPEED_AC);
    // inflate
    class procedure Decompress(const Src: Pointer; out Dest: Pointer; SrcSize: Integer; out DestSize: Integer); overload;
    class procedure Decompress(const Src: TBytes; out Dest: TBytes); overload;
    class procedure DecompressStream(Src: TStream; Dest: TStream);
  end;

implementation

// deflate
function DeflateInit(var strm: z_stream; Level: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external ZLIBDLL name 'deflateInit_';
function DeflateInit2(var strm: z_stream; Level, method, windowBits, memLevel, strategy: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external ZLIBDLL name 'deflateInit2_';
function Deflate(var strm: z_stream; flush: Integer): Integer; cdecl; external ZLIBDLL name 'deflate';
function DeflateEnd(var strm: z_stream): Integer; cdecl; external ZLIBDLL name 'deflateEnd';
// inflate
function InflateInit(var strm: z_stream; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external ZLIBDLL name 'inflateInit_';
function InflateInit2(var strm: z_stream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; cdecl; external ZLIBDLL name 'inflateInit2_';
function Inflate(var strm: z_stream; flush: Integer): Integer; cdecl; external ZLIBDLL name 'inflate';
function InflateEnd(var strm: z_stream): Integer; cdecl; external ZLIBDLL name 'inflateEnd';

class procedure SeaZlib.Compress(const Src: Pointer; out Dest: Pointer; SrcSize: Integer; out DestSize: Integer; Level: Integer = Z_BEST_SPEED_AC);
var
  zstream: z_stream;
  zInit, zDeflate: Integer;
begin
  FillChar(zstream, SizeOf(z_stream), 0);
  DestSize := SrcSize; // in the worse case dest len is equal at source len
  GetMem(Dest, DestSize);

  try
    zstream.next_in := Src;
    zstream.avail_in := SrcSize;
    zstream.next_out := Dest;
    zstream.avail_out := DestSize;

    zInit := DeflateInit2(zstream, Level, Z_DEFLATED, Z_MAX_WBITS, Z_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(z_stream));

    if zInit > Z_ERRNO then
    begin
      zDeflate := Deflate(zstream, Z_FINISH);
      if ((zDeflate <> Z_STREAM_END) and (zDeflate <> Z_BUF_ERROR)) or (zDeflate < Z_OK) then
        raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);
    end
    else
      raise EZCompressionError.Create(Z_errmsg[2 - zInit]);

    zDeflate := DeflateEnd(zstream);
    if zDeflate < Z_OK then
      raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);

    ReallocMem(Dest, zstream.total_out);
    DestSize := zstream.total_out;

  except
    FreeMem(Dest);
    raise;
  end;
end;

class procedure SeaZlib.Decompress(const Src: Pointer; out Dest: Pointer; SrcSize: Integer; out DestSize: Integer);
const
  BufferLen = 131070;
var
  zstream: z_stream;
  zInit, zInflate: Integer;
begin
  FillChar(zstream, SizeOf(z_stream), 0);
  DestSize := SrcSize * 3;
  GetMem(Dest, DestSize);

  try
    zstream.next_in := Src;
    zstream.avail_in := SrcSize;
    zstream.next_out := Dest;
    zstream.avail_out := DestSize;

    zInit := InflateInit2(zstream, Z_MAX_WBITS, ZLIB_VERSION, SizeOf(z_stream));
    if zInit > Z_ERRNO then
    begin

      zInflate := Inflate(zstream, Z_NO_FLUSH);
      if zInflate < Z_OK then
        raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

      while zInflate <> Z_STREAM_END do
      begin
        Inc(DestSize, BufferLen);
        ReallocMem(Dest, DestSize);
        zstream.next_out := PByte(Dest) + zstream.total_out;
        zstream.avail_out := BufferLen;
        zInflate := Inflate(zstream, Z_NO_FLUSH);
        if zInflate < Z_OK then
          raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

      end;

      if (zInflate <> Z_STREAM_END) and (zInflate <> Z_BUF_ERROR) then
        raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);
    end
    else
      raise EZCompressionError.Create(Z_errmsg[2 - zInit]);

    zInflate := InflateEnd(zstream);
    if zInflate < Z_OK then
      raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

    ReallocMem(Dest, zstream.total_out);
    DestSize := zstream.total_out;

  except
    FreeMem(Dest);
    raise;
  end;
end;

class procedure SeaZlib.Compress(const Src: TBytes; out Dest: TBytes; Level: Integer = Z_BEST_SPEED_AC);
var
  zstream: z_stream;
  zInit, zDeflate: Integer;
begin
  FillChar(zstream, SizeOf(z_stream), 0);
  SetLength(Dest, Length(Src));
  // in the worse case dest len is equal at source len

  try
    zstream.next_in := @Src[0];
    zstream.avail_in := Length(Src);
    zstream.next_out := @Dest[0];
    zstream.avail_out := Length(Dest);
    zInit := DeflateInit2(zstream, Level, Z_DEFLATED, Z_MAX_WBITS, Z_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(z_stream));

    if zInit > Z_ERRNO then
    begin
      zDeflate := Deflate(zstream, Z_FINISH);
      if ((zDeflate <> Z_STREAM_END) and (zDeflate <> Z_BUF_ERROR)) or (zDeflate < Z_OK) then
        raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);

    end
    else
      raise EZCompressionError.Create(Z_errmsg[2 - zInit]);

    zDeflate := DeflateEnd(zstream);
    if zDeflate < Z_OK then
      raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);

    SetLength(Dest, zstream.total_out);

  except
    SetLength(Dest, 0);
    raise;
  end;
end;

class procedure SeaZlib.Decompress(const Src: TBytes; out Dest: TBytes);
const
  BufferLen = 131070;
var
  zstream: z_stream;
  zInit, zInflate: Integer;
begin
  FillChar(zstream, SizeOf(z_stream), 0);
  SetLength(Dest, Length(Src) * 3);

  try
    zstream.next_in := @Src[0];
    zstream.avail_in := Length(Src);
    zstream.next_out := @Dest[0];
    zstream.avail_out := Length(Dest);
    zInit := InflateInit2(zstream, Z_MAX_WBITS, ZLIB_VERSION, SizeOf(z_stream));

    if zInit > Z_ERRNO then
    begin
      zInflate := Inflate(zstream, Z_NO_FLUSH);
      if zInflate < Z_OK then
        raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

      while zInflate <> Z_STREAM_END do
      begin
        SetLength(Dest, Length(Dest) + BufferLen);
        zstream.next_out := PByte(@Dest[0]) + zstream.total_out;
        zstream.avail_out := BufferLen;
        zInflate := Inflate(zstream, Z_NO_FLUSH);
        if zInflate < Z_OK then
          raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

      end;

      if (zInflate <> Z_STREAM_END) and (zInflate <> Z_BUF_ERROR) then
        raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);
    end
    else
      raise EZCompressionError.Create(Z_errmsg[2 - zInit]);

    zInflate := InflateEnd(zstream);
    if zInflate < Z_OK then
      raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

    SetLength(Dest, zstream.total_out);

  except
    SetLength(Dest, 0);
    raise;
  end;
end;

class procedure SeaZlib.CompressStream(Src: TStream; Dest: TStream; Level: Integer = Z_BEST_SPEED_AC);
const
  BufferLen = 131070;
var
  zstream: z_stream;
  zInit, zDeflate: Integer;
  InBuff, OutBuff: Pointer;
  InBuffSize, OutBuffSize: Integer;

begin
  FillChar(zstream, SizeOf(z_stream), 0);
  GetMem(InBuff, BufferLen);
  GetMem(OutBuff, BufferLen);

  try
    zInit := DeflateInit2(zstream, Level, Z_DEFLATED, Z_MAX_WBITS, Z_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(z_stream));
    if zInit > Z_ERRNO then
    begin

      InBuffSize := Src.Read(InBuff^, BufferLen);
      while InBuffSize > 0 do
      begin
        zstream.next_in := InBuff;
        zstream.avail_in := InBuffSize;

        repeat
          zstream.next_out := OutBuff;
          zstream.avail_out := BufferLen;
          zDeflate := Deflate(zstream, Z_NO_FLUSH);
          if zDeflate < Z_OK then
            raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);

          OutBuffSize := BufferLen - zstream.avail_out;
          if OutBuffSize > 0 then
            Dest.Write(OutBuff^, OutBuffSize)
          else
            break;
        until (zstream.avail_in = 0) and (zstream.avail_out > 0);
        InBuffSize := Src.Read(InBuff^, BufferLen);
      end;

      repeat
        zstream.next_out := OutBuff;
        zstream.avail_out := BufferLen;
        zDeflate := Deflate(zstream, Z_FINISH);
        if zDeflate < Z_OK then
          raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);

        OutBuffSize := BufferLen - zstream.avail_out;
        if OutBuffSize > 0 then
          Dest.Write(OutBuff^, OutBuffSize)
        else
          break;
      until (zDeflate = Z_STREAM_END) and (zstream.avail_out > 0);

      zDeflate := DeflateEnd(zstream);
      if zDeflate < Z_OK then
        raise EZCompressionError.Create(Z_errmsg[2 - zDeflate]);
    end;

  finally
    FreeMem(InBuff);
    FreeMem(OutBuff);
  end;
end;

class procedure SeaZlib.DecompressStream(Src: TStream; Dest: TStream);
const
  BufferLen = 131070;
var
  zstream: z_stream;
  zInit, zInflate: Integer;
  InBuff, OutBuff: Pointer;
  InBuffSize, OutBuffSize: Integer;

begin
  FillChar(zstream, SizeOf(z_stream), 0);
  GetMem(InBuff, BufferLen);
  GetMem(OutBuff, BufferLen);

  try
    zInit := InflateInit2(zstream, Z_MAX_WBITS, ZLIB_VERSION, SizeOf(z_stream));
    if zInit > Z_ERRNO then
    begin

      InBuffSize := Src.Read(InBuff^, BufferLen);
      while InBuffSize > 0 do
      begin
        zstream.next_in := InBuff;
        zstream.avail_in := InBuffSize;

        repeat
          zstream.next_out := OutBuff;
          zstream.avail_out := BufferLen;
          zInflate := Inflate(zstream, Z_NO_FLUSH);
          if zInflate < Z_OK then
            raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

          OutBuffSize := BufferLen - zstream.avail_out;
          if OutBuffSize > 0 then
            Dest.Write(OutBuff^, OutBuffSize)
          else
            break;
        until (zstream.avail_in = 0) and (zstream.avail_out > 0);
        InBuffSize := Src.Read(InBuff^, BufferLen);
      end;

      repeat
        zstream.next_out := OutBuff;
        zstream.avail_out := BufferLen;
        zInflate := Inflate(zstream, Z_FINISH);
        if zInflate < Z_OK then
          raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);

        OutBuffSize := BufferLen - zstream.avail_out;
        if OutBuffSize > 0 then
          Dest.Write(OutBuff^, OutBuffSize)
        else
          break;
      until (zInflate = Z_STREAM_END) and (zstream.avail_out > 0);

      zInflate := InflateEnd(zstream);
      if zInflate < Z_OK then
        raise EZCompressionError.Create(Z_errmsg[2 - zInflate]);
    end;

  finally
    FreeMem(InBuff);
    FreeMem(OutBuff);
  end;

end;

end.
