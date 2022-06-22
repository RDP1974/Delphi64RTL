unit RDPSimd64;

// 22 Febr 2019 Roberto Della Pasqua www.dellapasqua.com

interface

uses
   Winapi.Windows;

const
  SEASubset = 'SeaRTL.DLL';

function SeaZero(Pdst: PByte; Len: NativeUint): Integer; cdecl; external SEASubset name 'ippsZero_8u';
function SeaCopy(const Psrc: PByte; Pdst: PByte; Len: NativeUint): Integer; cdecl; external SEASubset name 'ippsCopy_8u';
function SeaMove(const Psrc: PByte; Pdst: PByte; Len: NativeUint): Integer; cdecl; external SEASubset name 'ippsMove_8u';
function SeaSet(Val: Byte; Pdst: PByte; Len: NativeUint): Integer; cdecl; external SEASubset name 'ippsSet_8u';
function SeaFind(const Psrc: PByte; Len: NativeUint; const Pfind: PByte; Lenfind: NativeUint; Pindex: PNativeUint): Integer; cdecl; external SEASubset name 'ippsFind_8u';
function SeaCompare(const Psrc1: PByte; const Psrc2: PByte; Len: NativeInt; Presult: PNativeInt): Integer; cdecl; external SEASubset name 'ippsCompare_8u';
function SeaUpperCase(const PSrcDst: PByte; Len: NativeUint): Integer; cdecl; external SEASubset name 'ippsUppercaseLatin_8u_I';
function SeaReplace(const Psrc: PByte; Pdst: PByte; Len: NativeUint; oldVal: Byte; ipp8u: Byte): Integer; cdecl; external SEASubset name 'ippsReplaceC_8u';

implementation

type
  TByteArray = array [0 .. 32767] of Byte;
  PByteArray = ^TByteArray;
  _ansichr = Ansichar;
  _WideStr = WideString;
  _RawByteStr = RawByteString;

procedure PatchCode(Old, New: Pointer; Size: Integer);
var
  NP, IR: DWORD;
  I: Integer;
begin
  if VirtualProtect(Old, Size, PAGE_EXECUTE_READWRITE, NP) then
  begin
    for I := 0 to Size - 1 do
      PByteArray(Old)^[I] := PByteArray(New)^[I];
    VirtualProtect(Old, Size, NP, IR);
    FlushInstructionCache(GetCurrentProcess, Old, Size);
  end;
end;

procedure RedirectCode(Func, RedirectFunc: Pointer);
var
  NewJump: packed record Code: Byte;
  Distance: Integer;
end;
begin
  if (Func = nil) or (RedirectFunc = nil) then Exit;
  NewJump.Code := $E9;
  NewJump.Distance := Integer(NativeUint(RedirectFunc) - NativeUint(Func) - Sizeof(NewJump));
  PatchCode(Func, @NewJump, Sizeof(NewJump));
end;

procedure Fillchar2(var Dest; Count: NativeUint; Value: _ansichr); inline;
begin
  if (Value = #0) then
    SeaZero(@Dest, Count)
  else
    SeaSet(Byte(Value), @Dest, Count);
end;

procedure Move2(const Source; var Dest; Count: NativeInt); inline;
begin
  if Count > 0 then
    SeaMove(@Source, @Dest, Count);
end;

type
  TPosRawFunc = function(const SubStr, Str: _RawByteStr; Offset: Integer = 1): Integer;
  TPosWideFunc = function(const SubStr, Str: _WideStr; Offset: Integer = 1): Integer;
  TPosUnicodeFunc = function(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;

function RetrievePosRawAddr: Pointer;
var
  f: TPosRawFunc;
begin
  f := Pos;
  Result := @f;
end;

function RetrievePosWideAddr: Pointer;
var
  f: TPosWideFunc;
begin
  f := Pos;
  Result := @f;
end;

function RetrievePosUnicodeAddr: Pointer;
var
  f: TPosUnicodeFunc;
begin
  f := Pos;
  Result := @f;
end;

function PosUnicode(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;
var
  Idx: PByte;
  LenR: Integer;
  LenS: Integer;
  LenB: Integer;
begin
  LenS := Length(Str);
  LenB := Length(SubStr);
  if (LenS < LenB) or (Offset > LenS) or (Offset < 1) then
    Result := 0
  else
  begin
    Idx := PByte(Str);
    Inc(Idx, (Offset - 1) * 2);
    SeaFind(Idx, (LenS - (Offset - 1)) * 2, PByte(SubStr), LenB * 2, @LenR);
    if LenR = -1 then
      Result := 0
    else
    begin
      Inc(LenR, (LenS * 2) - ((LenS - Offset + 1) * 2));
      Result := Trunc(LenR / 2) + 1;
    end;
  end;
end;

function PosWide(const SubStr, Str: _WideStr; Offset: Integer = 1): Integer;
var
  Idx: PByte;
  LenR: Integer;
  LenS: Integer;
  LenB: Integer;
begin
  LenS := Length(Str);
  LenB := Length(SubStr);
  if (LenS < LenB) or (Offset > LenS) or (Offset < 1) then
    Result := 0
  else
  begin
    Idx := PByte(Str);
    Inc(Idx, (Offset - 1) * 2);
    SeaFind(Idx, (LenS - (Offset - 1)) * 2, PByte(SubStr), LenB * 2, @LenR);
    if LenR = -1 then
      Result := 0
    else
    begin
      Inc(LenR, (LenS * 2) - ((LenS - Offset + 1) * 2));
      Result := Trunc(LenR / 2) + 1;
    end;
  end;
end;

function PosRaw(const SubStr, Str: _RawByteStr; Offset: Integer = 1): Integer;
var
  Idx: PByte;
  LenR: Integer;
  LenS: Integer;
  LenB: Integer;
begin
  LenS := Length(Str);
  LenB := Length(SubStr);
  if (LenS < LenB) or (Offset > LenS) or (Offset < 1) then
    Result := 0
  else
  begin
    Idx := PByte(Str);
    Inc(Idx, Offset - 1);
    SeaFind(Idx, LenS - Offset + 1, PByte(SubStr), LenB, @LenR);
    if LenR = -1 then
      Result := 0
    else
    begin
      Inc(LenR, LenS - (LenS - Offset + 1));
      Result := LenR + 1;
    end;
  end;
end;

function OrigFillchar: Pointer;
asm
  mov rax,offset System.@FillChar
end;

procedure PatchRTL64;
begin
  RedirectCode(@System.Move, @Move2);
  RedirectCode(OrigFillchar, @Fillchar2);
  RedirectCode(RetrievePosRawAddr, @PosRaw);
  RedirectCode(RetrievePosWideAddr, @PosWide);
  RedirectCode(RetrievePosUnicodeAddr, @PosUnicode);
end;

initialization
  PatchRTL64;

end.
