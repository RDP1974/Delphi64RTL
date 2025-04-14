unit RDPMM64;

// 22 Febr 2019 Roberto Della Pasqua www.dellapasqua.com
// memory manager replacement with Intel ipp libraries
// 30 oct 2024 updated to intel one api v2022.0, visual c++ v19.41.34123
// 11 apr 2025 updated to intel one api v2022.1, visual c++ v19.43.34810
// seamm.dll md5 df1e5b489d2f9ac325d194a9dc67c8bc size 107520

interface

uses
  RDPSimd64;
  //please check the bottom file about delphi 12.x version
  //please check intel oneapi license for library distribution

implementation

const
  TBBMalloc = 'SeaMM.DLL';

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
  if (Result <> nil) then
  {$IF CompilerVersion < 36.0}
   SeaZero(Result, Size);
  {$ELSE}
   Fillchar(Result^, Size, #0);
  {$IFEND}
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
