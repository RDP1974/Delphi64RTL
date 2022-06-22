unit RDPMM64;

// 22 Febr 2019 Roberto Della Pasqua www.dellapasqua.com

interface

uses
  RDPSimd64;

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
  if (Result <> nil) then SeaZero(Result, Size);
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
