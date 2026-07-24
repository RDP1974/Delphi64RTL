unit RDPQueue64;
// Roberto Della Pasqua www.dellapasqua.com
// 14 apr 2025 thread safe concurrent queue from Intel One Api v2022.1
// 20 jun 2026 update intel oneapi 2023.1
// 23 july 2026 linux static library tbb, libgcc and libstd++ (latest gcc and tbb at the time)

interface

{$IFDEF WIN64}
uses Windows;
const
  QPARDLL = 'SeaQPar.dll';
{$ENDIF}

{$IFDEF LINUX}
const
  QPARDLL = 'libseaqpar.so';
{$ENDIF}

function CreateQueue: pointer; stdcall; external QPARDLL name 'CreateQueue';
function PopFromQueue(queue: pointer; ptr: pointer): LongBool; stdcall; external QPARDLL name 'PopFromQueue';
function IsQueueEmpty(queue: pointer): LongBool; stdcall; external QPARDLL name 'IsQueueEmpty';
procedure PushToQueue(queue: pointer; ptr: pointer); stdcall; external QPARDLL name 'PushToQueue';
procedure FreeQueue(queue: pointer); stdcall; external QPARDLL name 'FreeQueue';

implementation

end.

