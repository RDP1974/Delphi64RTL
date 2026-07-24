unit RDPMM64;

// 22 Febr 2019 Roberto Della Pasqua www.dellapasqua.com
// memory manager replacement with Intel ipp libraries
// 30 oct 2024 updated to intel one api v2022.0, visual c++ v19.41.34123
// 11 apr 2025 updated to intel one api v2022.1, visual c++ v19.43.34810
// 28 ago 2025 linux update, apt install libtbb-dev (performance gain is negligible)
// 7 nov 2025 tbbmalloc oneapi 2023.0
// 20 jun 2026 update oneapi 2023.1
// 24 july 2026 linux static library (no dependencies, no stripping, clean, dext qc test passed)
// seamm.dll md5 262c4780e41fed814a9d96735dc053fa
// libseamm.so md5 56265e9523efd78947ea5c21bf55c3a7

interface

{$IFDEF MSWINDOWS}
uses
 RDPSimd64;
{$ENDIF}
 //please check the bottom file about delphi 12.x version
 //please check intel oneapi license for library distribution

implementation

const
 {$IFDEF MSWINDOWS}
  TBBMalloc = 'SeaMM.DLL';
 {$ENDIF}
 {$IFDEF LINUX}
  TBBMalloc = 'libseamm.so';
 {$ENDIF}

function SeaMalloc(Size: NativeUint): Pointer; cdecl; external TBBMalloc name 'scalable_malloc';
procedure SeaFreemem(P: Pointer); cdecl; external TBBMalloc name 'scalable_free';
function SeaRealloc(P: Pointer; Size: NativeUint): Pointer; cdecl; external TBBMalloc name 'scalable_realloc';

function QSEAGetMem(Size: Nativeint): Pointer; inline;
begin
  Result := SeaMalloc(Size);
end;

function QSEAFreeMem(P: Pointer): Integer; inline;
begin
  SeaFreemem(P);
  Result := 0;
end;

function QSEAReallocMem(P: Pointer; Size: Nativeint): Pointer; inline;
begin
  Result := SeaRealloc(P, Size);
end;

function QSEAAllocMem(Size: Nativeint): Pointer; inline;
begin
  Result := SeaMalloc(Size);
 {$IFDEF MSWINDOWS}
  if (Result <> nil) then SeaZero(Result, Size);
 {$ENDIF}
 {$IFDEF LINUX}
  if (Result <> nil) then Fillchar(Result^, Size, 0);
 {$ENDIF}
end;

function QRegisterExpectedMemoryLeak(P: Pointer): Boolean; inline;
begin
  Result := False;
end;

function QUnregisterExpectedMemoryLeak(P: Pointer): Boolean; inline;
begin
  Result := False;
end;

const
  SEAMemoryManager: TMemoryManagerEx = (
  Getmem: QSEAGetMem;
  Freemem: QSEAFreeMem;
  Reallocmem: QSEAReallocMem;
  Allocmem: QSEAAllocMem;
  RegisterExpectedMemoryLeak: QRegisterExpectedMemoryLeak;
  UnregisterExpectedMemoryLeak: QRegisterExpectedMemoryLeak
  );

var
  OldMemoryManager: TMemoryManagerEx;

initialization
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(SEAMemoryManager);

finalization
  SetMemoryManager(OldMemoryManager);

end.



