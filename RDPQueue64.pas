unit RDPQueue64;
// Roberto Della Pasqua www.dellapasqua.com
// 14 apr 2025 thread safe concurrent queue from Intel One Api v2022.1

interface

uses Windows;

const
  QPARDLL = 'SeaQPar.dll';

function CreateQueue: pointer; stdcall; external QPARDLL name 'CreateQueue';
function PopFromQueue(queue: pointer; ptr: pointer): LongBool; stdcall; external QPARDLL name 'PopFromQueue';
function IsQueueEmpty(queue: pointer): LongBool; stdcall; external QPARDLL name 'IsQueueEmpty';
procedure PushToQueue(queue: pointer; ptr: Pointer); stdcall; external QPARDLL name 'PushToQueue';
procedure FreeQueue(queue: pointer); stdcall; external QPARDLL name 'FreeQueue';

implementation

end.

