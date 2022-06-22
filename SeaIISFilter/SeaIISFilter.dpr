library SeaIISFilter;

// 19 june 2018 Roberto Della Pasqua www.dellapasqua.com
// highly iterative
// low level parallel SIMD
// threadpool wrapper
// zero memory copy
// data structure deduplication
// 5x faster than default IIS 10 gzip at the same ratio (I7 cpu 4/8 core)

{.$DEFINE DEBUGFILE }
{$O+}
{$WEAKLINKRTTI ON}
{$MINENUMSIZE 4}

uses
  RDPMM64,
  RDPSimd64,
  RDPZLib64,
  Windows,
  SysUtils;

// SDK IIS 7.0 httpfilt.h

const
  HSE_VERSION_MAJOR = 7;
  HSE_VERSION_MINOR = 0;
  HSE_LOG_BUFFER_LEN = 80;
  HSE_MAX_EXT_DLL_NAME_LEN = 256;
  HTTP_FILTER_REVISION = $70000;
  SF_MAX_USERNAME = (256 + 1);
  SF_MAX_PASSWORD = (256 + 1);
  SF_MAX_AUTH_TYPE = (32 + 1);
  SF_MAX_FILTER_DESC_LEN = (256 + 1);
  SF_STATUS_REQ_FINISHED = $8000000;
  SF_STATUS_REQ_FINISHED_KEEP_CONN = $8000001;
  SF_STATUS_REQ_NEXT_NOTIFICATION = $8000002;
  SF_STATUS_REQ_HANDLED_NOTIFICATION = $8000003;
  SF_STATUS_REQ_ERROR = $8000004;
  SF_STATUS_REQ_READ_NEXT = $8000005;
  SF_DENIED_LOGON = $00000001;
  SF_DENIED_RESOURCE = $00000002;
  SF_DENIED_FILTER = $00000004;
  SF_DENIED_APPLICATION = $00000008;
  SF_DENIED_BY_CONFIG = $00010000;
  SF_NOTIFY_SECURE_PORT = $00000001;
  SF_NOTIFY_NONSECURE_PORT = $00000002;
  SF_NOTIFY_READ_RAW_DATA = $00008000;
  SF_NOTIFY_PREPROC_HEADERS = $00004000;
  SF_NOTIFY_URL_MAP = $00001000;
  SF_NOTIFY_AUTHENTICATION = $00002000;
  SF_NOTIFY_ACCESS_DENIED = $00000800;
  SF_NOTIFY_AUTH_COMPLETE = $04000000;
  SF_NOTIFY_SEND_RESPONSE = $00000040;
  SF_NOTIFY_SEND_RAW_DATA = $00000400;
  SF_NOTIFY_END_OF_REQUEST = $00000080;
  SF_NOTIFY_LOG = $00000200;
  SF_NOTIFY_END_OF_NET_SESSION = $00000100;
  SF_NOTIFY_EXTENSION_TRIGGER = $02000000;
  SF_NOTIFY_ORDER_HIGH = $00080000;
  SF_NOTIFY_ORDER_MEDIUM = $00040000;
  SF_NOTIFY_ORDER_LOW = $00020000;
  SF_NOTIFY_ORDER_DEFAULT = SF_NOTIFY_ORDER_LOW;
  SF_NOTIFY_ORDER_MASK = (SF_NOTIFY_ORDER_HIGH or SF_NOTIFY_ORDER_MEDIUM or SF_NOTIFY_ORDER_LOW);

type
  SF_REQ_TYPE = (SF_REQ_SEND_RESPONSE_HEADER, SF_REQ_ADD_HEADERS_ON_DENIAL, SF_REQ_SET_NEXT_READ_SIZE, SF_REQ_SET_PROXY_INFO, SF_REQ_GET_CONNID,
    SF_REQ_SET_CERTIFICATE_INFO, SF_REQ_GET_PROPERTY, SF_REQ_NORMALIZE_URL, SF_REQ_DISABLE_NOTIFICATIONS);

  SF_PROPERTY_IIS = (SF_PROPERTY_SSL_CTXT, SF_PROPERTY_INSTANCE_NUM_ID);
  SF_PRIORITY_TYPE = (HIGH_PRIORITY, MEDIUM_PRIORITY, LOW_PRIORITY, DEFAULT_PRIORITY);

  THTTP_FILTER_CONTEXT = record
    cbSize: DWORD;
    Revision: DWORD;
    ServerContext: PVOID;
    ulReserved: DWORD;
    fIsSecurePort: BOOL;
    pFilterContext: PVOID;
    GetServerVariable: PVOID;
    AddResponseHeaders: PVOID;
    WriteClient: PVOID;
    AllocMem: PVOID;
    ServerSupportFunc: PVOID;
  end;

  HTTP_FILTER_CONTEXT = THTTP_FILTER_CONTEXT;
  PHTTP_FILTER_CONTEXT = ^HTTP_FILTER_CONTEXT;

  TGetServerVariable = function(pfc: PHTTP_FILTER_CONTEXT; VariableName: PAnsiChar; Buffer: LPVOID; BuffSize: PDWORD): BOOL; stdcall;
  TAddResponseHeaders = function(pfc: PHTTP_FILTER_CONTEXT; Headers: PAnsiChar; Reserved: DWORD): BOOL; stdcall;
  TWriteClient = function(pfc: PHTTP_FILTER_CONTEXT; Buffer: LPVOID; dwBytes: LPDWORD; Reserved: DWORD): BOOL; stdcall;
  TAllocMem = function(pfc: PHTTP_FILTER_CONTEXT; cbSize: DWORD; dwReserved: DWORD): LPVOID; stdcall;
  TServerSupportFunc = function(pfc: PHTTP_FILTER_CONTEXT; sfReq: SF_REQ_TYPE; pData: PVOID; ul1: DWORD; ul2: DWORD): BOOL; stdcall;

  THTTP_FILTER_RAW_DATA = record
    pvInData: PVOID;
    cbInData: DWORD;
    cbInBuffer: DWORD;
    dwReserved: DWORD;
  end;

  HTTP_FILTER_RAW_DATA = THTTP_FILTER_RAW_DATA;
  PHTTP_FILTER_RAW_DATA = ^HTTP_FILTER_RAW_DATA;

  TGetHeader = function(pfc: PHTTP_FILTER_CONTEXT; lpszName: PAnsiChar; lpvBuffer: LPVOID; lpdwSize: LPDWORD): BOOL; stdcall;
  TSetHeader = function(pfc: PHTTP_FILTER_CONTEXT; lpszName: PAnsiChar; lpszValue: PAnsiChar): BOOL; stdcall;
  TAddHeader = function(pfc: PHTTP_FILTER_CONTEXT; lpszName: PAnsiChar; lpszValue: PAnsiChar): BOOL; stdcall;
  TGetUserToken = function(pfc: PHTTP_FILTER_CONTEXT; phToken: PHANDLE): BOOL; stdcall;

  THTTP_FILTER_PREPROC_HEADERS = record
    GetHeader: PVOID;
    SetHeader: PVOID;
    AddHeader: PVOID;
    dwReserved: DWORD;
  end;

  HTTP_FILTER_PREPROC_HEADERS = THTTP_FILTER_PREPROC_HEADERS;
  PHTTP_FILTER_PREPROC_HEADERS = ^HTTP_FILTER_PREPROC_HEADERS;
  HTTP_FILTER_SEND_RESPONSE = HTTP_FILTER_PREPROC_HEADERS;
  THTTP_FILTER_SEND_RESPONSE = THTTP_FILTER_PREPROC_HEADERS;
  PHTTP_FILTER_SEND_RESPONSE = ^HTTP_FILTER_SEND_RESPONSE;

  THTTP_FILTER_AUTHENT = record
    pszUser: PAnsiChar;
    cbUserBuff: DWORD;
    pszPassword: PAnsiChar;
    cbPasswordBuff: DWORD;
  end;

  HTTP_FILTER_AUTHENT = THTTP_FILTER_AUTHENT;
  PHTTP_FILTER_AUTHENT = ^HTTP_FILTER_AUTHENT;

  THTTP_FILTER_URL_MAP = record
    pszURL: PAnsiChar;
    pszPhysicalPath: PAnsiChar;
    cbPathBuff: DWORD;
  end;

  HTTP_FILTER_URL_MAP = THTTP_FILTER_URL_MAP;
  PHTTP_FILTER_URL_MAP = ^HTTP_FILTER_URL_MAP;

  THTTP_FILTER_URL_MAP_EX = record
    pszURL: PAnsiChar;
    pszPhysicalPath: PAnsiChar;
    cbPathBuff: DWORD;
    DwFlags: DWORD;
    CchMatchingPath: DWORD;
    CchMatchingURL: DWORD;
    PszScriptMapEntry: PAnsiChar;
  end;

  HTTP_FILTER_URL_MAP_EX = THTTP_FILTER_URL_MAP_EX;
  PHTTP_FILTER_URL_MAP_EX = ^HTTP_FILTER_URL_MAP_EX;

  THTTP_FILTER_ACCESS_DENIED = record
    pszURL: PAnsiChar;
    pszPhysicalPath: PAnsiChar;
    dwReason: DWORD;
  end;

  HTTP_FILTER_ACCESS_DENIED = THTTP_FILTER_ACCESS_DENIED;
  PHTTP_FILTER_ACCESS_DENIED = ^HTTP_FILTER_ACCESS_DENIED;

  THTTP_FILTER_LOG = record
    pszClientHostName: PAnsiChar;
    pszClientUserName: PAnsiChar;
    pszServerName: PAnsiChar;
    pszOperation: PAnsiChar;
    pszTarget: PAnsiChar;
    pszParameters: PAnsiChar;
    dwHttpStatus: DWORD;
    dwWin32Status: DWORD;
    DwBytesSent: DWORD;
    DwBytesRecvd: DWORD;
    MsTimeForProcessing: DWORD;
  end;

  HTTP_FILTER_LOG = THTTP_FILTER_LOG;
  PHTTP_FILTER_LOG = ^HTTP_FILTER_LOG;

  THTTP_FILTER_AUTH_COMPLETE_INFO = record
    GetHeader: PVOID;
    SetHeader: PVOID;
    AddHeader: PVOID;
    GetUserToken: TGetUserToken;
    HttpStatus: DWORD;
    fResetAuth: BOOL;
    dwReserved: DWORD;
  end;

  HTTP_FILTER_AUTH_COMPLETE_INFO = THTTP_FILTER_AUTH_COMPLETE_INFO;
  PHTTP_FILTER_AUTH_COMPLETE_INFO = ^HTTP_FILTER_AUTH_COMPLETE_INFO;

  THTTP_FILTER_EXTENSION_TRIGGER_INFO = record
    dwTriggerType: DWORD;
    pvTriggerContext: PVOID;
  end;

  HTTP_FILTER_EXTENSION_TRIGGER_INFO = THTTP_FILTER_EXTENSION_TRIGGER_INFO;
  PHTTP_FILTER_EXTENSION_TRIGGER_INFO = ^HTTP_FILTER_EXTENSION_TRIGGER_INFO;

  THTTP_FILTER_VERSION = record
    dwServerFilterVersion: DWORD;
    dwFilterVersion: DWORD;
    lpszFilterDesc: array [0 .. SF_MAX_FILTER_DESC_LEN - 1] of AnsiChar;
    DwFlags: DWORD;
  end;

  HTTP_FILTER_VERSION = THTTP_FILTER_VERSION;
  PHTTP_FILTER_VERSION = ^HTTP_FILTER_VERSION;

  TThStruct = class
  strict private
    CPowered: TBytes;
    CAsp: TBytes;
    CAspOld: TBytes;
    CPhp: TBytes;
    CJava: TBytes;
    CAspx: TBytes;
    CDll: TBytes;
    CHttp1: TBytes;
    CHttp2: TBytes;
    CCrlf: TBytes;
    //
    CCtext: TBytes;
    CCtext2: TBytes;
    CCtext3: TBytes;
    CCtext4: TBytes;
    CCtext5: TBytes;
    //
    CAcceptText: TBytes;
    //
    CCl: TBytes;
    CTransfer: TBytes;
    CContent: TBytes;
    CLength: TBytes;
    CAccept: TBytes;
    CDeflate: TBytes;
    CGzip: TBytes;
    CXGzip: TBytes;
    CEndLine: TBytes;
    CReplace: TBytes;
    CCTDeflate: TBytes;
    //
    FThreadId: DWORD;
    FZLevel: Integer;
    //
    PHeaders: PVOID;
    PHeadersLen: Integer;
    GpvInData: PVOID;
    GcbInData: Cardinal;
    GBuffer: PVOID;
    GBufSize: Cardinal;
    IBuffer: PVOID;
    IBufSize: Integer;
    IBufferTS: PVOID;
    //
    CLen: Integer;
    CLen2: Integer;
    CLen3: Integer;
    Headers: PVOID;
    HelperS: PAnsiChar;
    HelperP: PByte;
    //
    QStartTime: Int64;
    QEndTime: Int64;
    QFrequency: Int64;
    QElapsedTime: Int64;
    QTotalHits: Int64;
    QTotalBytes: Int64;
    QTotalBytesC: Int64;
{$IFDEF DEBUGFILE}
    DebugS: AnsiString;
{$ENDIF}
  protected
    constructor Create;
    destructor Destroy; override;
  public
    procedure TreatUrlMap(pfc: PHTTP_FILTER_CONTEXT; PvNotification: PVOID);
    procedure TreatSendResponse(pfc: PHTTP_FILTER_CONTEXT; PvNotification: PVOID);
    procedure TreatSendRawData(pfc: PHTTP_FILTER_CONTEXT; PvNotification: PVOID);
    procedure TreatEndOfRequest(pfc: PHTTP_FILTER_CONTEXT);
  end;

threadvar ThInit: boolean;
threadvar ThWork: TThStruct;

constructor TThStruct.Create;
begin
  CPowered := TEncoding.ASCII.GetBytes('X-Powered-By:');
  CAsp := TEncoding.ASCII.GetBytes('asp.net');
  CAspOld := TEncoding.ASCII.GetBytes('asp');
  CPhp := TEncoding.ASCII.GetBytes('php');
  CJava := TEncoding.ASCII.GetBytes('jsp');
  CAspx := TEncoding.ASCII.GetBytes('aspx');
  CDll := TEncoding.ASCII.GetBytes('dll');
  CHttp1 := TEncoding.ASCII.GetBytes('HTTP/1.1 200 OK');
  CHttp2 := TEncoding.ASCII.GetBytes('HTTP/2');
  CCrlf := TEncoding.ASCII.GetBytes(#13#10#13#10);
  //
  CCtext := TEncoding.ASCII.GetBytes('Content-Type: text/');
  CCtext2 := TEncoding.ASCII.GetBytes('Content-Type: message/');
  CCtext3 := TEncoding.ASCII.GetBytes('Content-Type: application/javascript');
  CCtext4 := TEncoding.ASCII.GetBytes('Content-Type: application/x-javascript');
  CCtext5 := TEncoding.ASCII.GetBytes('Content-Type: application/xml');
  //
  CAcceptText := TEncoding.ASCII.GetBytes('Accept: text/html');
  //
  CCl := TEncoding.ASCII.GetBytes('<%CL%>');
  CTransfer := TEncoding.ASCII.GetBytes('Transfer-Encoding:');
  CContent := TEncoding.ASCII.GetBytes('Content-Encoding:');
  CLength := TEncoding.ASCII.GetBytes('Content-Length:');
  CAccept := TEncoding.ASCII.GetBytes('HTTP_ACCEPT_ENCODING');
  CDeflate := TEncoding.ASCII.GetBytes('deflate');
  CGzip := TEncoding.ASCII.GetBytes('gzip');
  CXGzip := TEncoding.ASCII.GetBytes('x-gzip');
  CEndLine := TEncoding.ASCII.GetBytes(#13#10);
  CReplace := TEncoding.ASCII.GetBytes('%PAR%');
  CCTDeflate := TEncoding.ASCII.GetBytes('Content-Encoding: deflate');
  //
  FThreadId := GetCurrentThreadId;
  FZLevel := 1;
  GetMem(IBufferTS, 256);
  //
  QueryPerformanceFrequency(QFrequency);
  QElapsedTime := 0;
  QTotalHits := 0;
  QTotalBytes := 0;
  QTotalBytesC := 0;
  //
  ThInit := True;
  inherited;
end;

destructor TThStruct.Destroy;
begin
  FreeMem(IBufferTS);
  inherited;
end;

procedure TThStruct.TreatEndOfRequest(pfc: PHTTP_FILTER_CONTEXT);
begin
  if DWORD(pfc^.pFilterContext) = 4 then // zerocopy buffering
  begin
    try
      QueryPerformanceCounter(QStartTime);
      IBuffer := nil;
      SeaZlib.Compress(GpvInData, GcbInData, IBuffer, IBufSize);
      QueryPerformanceCounter(QEndTime);
      Inc(QElapsedTime, QEndTime - QStartTime);
      Inc(QTotalHits);
      Inc(QTotalBytes, GcbInData);
      Inc(QTotalBytesC, IBufSize);
      // InterlockedIncrement64
      SeaFind(PByte(PHeaders), PHeadersLen, @CReplace[0], 5, @CLen);
      HelperS := IntToStrF(IBufSize);
      CLen2 := Length(HelperS);
      CLen3 := PHeadersLen + 27 - (5 - CLen2);
      GetMem(Headers, CLen3);
      HelperP := PByte(Headers);
      SeaMove(PByte(PHeaders), HelperP, CLen + 17);
      Inc(HelperP, CLen);
      SeaMove(PByte(HelperS), HelperP, CLen2);
      Inc(HelperP, CLen2);
      HelperP^ := 13;
      Inc(HelperP);
      HelperP^ := 10;
      Inc(HelperP);
      SeaMove(@CCTDeflate[0], HelperP, 25);
      Inc(HelperP, 25);
      CLen2 := PHeadersLen - (CLen + 5);
      SeaMove(PByte(PHeaders) + CLen + 5, HelperP, CLen2);
{$IFDEF DEBUGFILE}
      SetString(DebugS, PAnsiChar(PByte(Headers)), CLen3);
      Log('Level5 headers:' + #13#10'START>>' + DebugS + '<<END');
      SetString(DebugS, PAnsiChar(PByte(IBuffer)), IBufSize);
      Log('Level5 body:' + #13#10'START>>' + DebugS + '<<END');
{$ENDIF}
      TWriteClient(pfc^.WriteClient)(pfc, Headers, @CLen3, 0);
      TWriteClient(pfc^.WriteClient)(pfc, IBuffer, @IBufSize, 0);
    finally
      FreeMem(Headers);
      FreeMem(IBuffer);
    end;
  end
  else if DWORD(pfc^.pFilterContext) > 4 then // body one-shot buffering
  begin
    try
      QueryPerformanceCounter(QStartTime);
      IBuffer := nil;
      SeaZlib.Compress(GBuffer, GBufSize, IBuffer, IBufSize);
      QueryPerformanceCounter(QEndTime);
      Inc(QElapsedTime, QEndTime - QStartTime);
      Inc(QTotalHits);
      Inc(QTotalBytes, GBufSize);
      Inc(QTotalBytesC, IBufSize);
      SeaFind(PByte(PHeaders), PHeadersLen, @CReplace[0], 5, @CLen);
      HelperS := IntToStrF(IBufSize);
      CLen2 := Length(HelperS);
      CLen3 := PHeadersLen + 27 - (5 - CLen2);
      GetMem(Headers, CLen3);
      HelperP := PByte(Headers);
      SeaMove(PByte(PHeaders), HelperP, CLen + 17);
      Inc(HelperP, CLen);
      SeaMove(PByte(HelperS), HelperP, CLen2);
      Inc(HelperP, CLen2);
      HelperP^ := 13;
      Inc(HelperP);
      HelperP^ := 10;
      Inc(HelperP);
      SeaMove(@CCTDeflate[0], HelperP, 25);
      Inc(HelperP, 25);
      CLen2 := PHeadersLen - (CLen + 5);
      SeaMove(PByte(PHeaders) + CLen + 5, HelperP, CLen2);
{$IFDEF DEBUGFILE}
      SetString(DebugS, PAnsiChar(PByte(Headers)), CLen3);
      Log('Level5+ headers:' + #13#10 + DebugS);
      SetString(DebugS, PAnsiChar(PByte(IBuffer)), IBufSize);
      Log('Level5+ body:' + #13#10 + DebugS);
{$ENDIF}
      TWriteClient(pfc^.WriteClient)(pfc, Headers, @CLen3, 0);
      TWriteClient(pfc^.WriteClient)(pfc, IBuffer, @IBufSize, 0);
    finally
      FreeMem(Headers);
      FreeMem(IBuffer);
      FreeMem(GBuffer);
    end;
  end;
end;

procedure TThStruct.TreatSendRawData(pfc: PHTTP_FILTER_CONTEXT; PvNotification: Pointer);
begin
  with THTTP_FILTER_RAW_DATA(PvNotification^) do
  begin
    SeaCompare(pvInData, @CHttp1[0], 14, @CLen); // todo: http2 compatibility
    if (CLen = 0) then
    begin
{$IFDEF DEBUGFILE}
      SetString(DebugS, PAnsiChar(pvInData), cbInData);
      Log('Level1:' + #13#10 + 'CbInData:' + IntToStr(cbInData) + #13#10 + 'cbInBuffer:' + IntToStr(cbInBuffer) + #13#10 + '-' + #13#10 + DebugS);
{$ENDIF}
      if (cbInData < 100) or (cbInData > 99999) then
        Exit; // testare
      SeaFind(pvInData, cbInData, @CCrlf[0], 4, @CLen);
      if CLen > -1 then
      begin
        SeaFind(pvInData, CLen, @CCtext[0], 19, @CLen);
        if CLen > -1 then
        begin
          PHeaders := pvInData;
          PHeadersLen := cbInData;
          cbInData := 0;
          cbInBuffer := 0;
          pfc^.pFilterContext := PDWORD(3);
        end
        else
        begin
          SeaFind(pvInData, CLen, @CCtext2[0], 22, @CLen);
          if CLen > -1 then
          begin
            PHeaders := pvInData;
            PHeadersLen := cbInData;
            cbInData := 0;
            cbInBuffer := 0;
            pfc^.pFilterContext := PDWORD(3);
          end
          else
          begin
            SeaFind(pvInData, CLen, @CCtext3[0], 36, @CLen);
            if CLen > -1 then
            begin
              PHeaders := pvInData;
              PHeadersLen := cbInData;
              cbInData := 0;
              cbInBuffer := 0;
              pfc^.pFilterContext := PDWORD(3);
            end
            else
            begin
              SeaFind(pvInData, CLen, @CCtext4[0], 38, @CLen);
              if CLen > -1 then
              begin
                PHeaders := pvInData;
                PHeadersLen := cbInData;
                cbInData := 0;
                cbInBuffer := 0;
                pfc^.pFilterContext := PDWORD(3);
              end
              else
              begin
                SeaFind(pvInData, CLen, @CCtext5[0], 29, @CLen);
                if CLen > -1 then
                begin
                  PHeaders := pvInData;
                  PHeadersLen := cbInData;
                  cbInData := 0;
                  cbInBuffer := 0;
                  pfc^.pFilterContext := PDWORD(3);
                end;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      if DWORD(pfc.pFilterContext) = 3 then
      begin
{$IFDEF DEBUGFILE}
        SetString(DebugS, PAnsiChar(pvInData), cbInData);
        Log('Level2:' + #13#10 + 'CbInData:' + IntToStr(cbInData) + #13#10 + 'cbInBuffer:' + IntToStr(cbInBuffer) + #13#10 + '-' + #13#10 + DebugS);
{$ENDIF}
        GpvInData := pvInData;
        GcbInData := cbInData;
        cbInData := 0;
        cbInBuffer := 0;
        pfc^.pFilterContext := PDWORD(4); // go stage index 4
      end
      else
      begin
        if DWORD(pfc^.pFilterContext) = 4 then
        begin
          GBufSize := GcbInData + cbInData;
          GetMem(GBuffer, GBufSize);
          SeaMove(GpvInData, GBuffer, GcbInData);
          SeaMove(pvInData, PByte(GBuffer) + GcbInData, cbInData);
{$IFDEF DEBUGFILE}
          SetString(DebugS, PAnsiChar(pvInData), cbInData);
          Log('Level3:' + #13#10 + 'CbInData:' + IntToStr(cbInData) + #13#10 + 'cbInBuffer:' + IntToStr(cbInBuffer) + #13#10 + 'GcbInData:' +
            IntToStr(GcbInData) + #13#10 + 'GBufSize:' + #13#10 + '-' + #13#10 + DebugS);
{$ENDIF}
          cbInData := 0;
          cbInBuffer := 0;
          pfc^.pFilterContext := PDWORD(5); // go stage index 5
        end
        else
        begin // 5+
          ReallocMem(GBuffer, GBufSize + cbInData);
          SeaMove(pvInData, PByte(GBuffer) + GBufSize, cbInData);
          Inc(GBufSize, cbInData);
{$IFDEF DEBUGFILE}
          SetString(DebugS, PAnsiChar(pvInData), cbInData);
          Log('Level4+:' + #13#10 + 'CbInData:' + IntToStr(cbInData) + #13#10 + 'cbInBuffer:' + IntToStr(cbInBuffer) + #13#10 + 'GcbInData:' +
            IntToStr(GcbInData) + #13#10 + 'GBufSize:' + #13#10 + '-' + #13#10 + DebugS);
{$ENDIF}
          cbInData := 0;
          cbInBuffer := 0;
        end;
      end;
    end;
  end;
end;

procedure TThStruct.TreatSendResponse(pfc: PHTTP_FILTER_CONTEXT; PvNotification: Pointer);
begin
  with THTTP_FILTER_SEND_RESPONSE(PvNotification^) do
  begin
    SeaZero(@IBufferTS, 256);
    CLen := 256;
    if not TGetHeader(GetHeader)(pfc, @CTransfer[0], @IBufferTS, @CLen) then
    begin
      SeaZero(@IBufferTS, 256);
      CLen := 256;
      if TGetServerVariable(pfc.GetServerVariable)(pfc, @CAccept[0], @IBufferTS, @CLen) then
      begin
        SeaFind(@IBufferTS, 256, @CDeflate[0], 7, @CLen);
        if CLen > 0 then
        begin
          SeaZero(@IBufferTS, 256);
          CLen := 256;
          TGetHeader(GetHeader)(pfc, @CContent[0], @IBufferTS, @CLen);
          SeaFind(@IBufferTS, 256, @CDeflate[0], 7, @CLen);
          if CLen = -1 then
          begin
            SeaFind(@IBufferTS, 256, @CGzip[0], 4, @CLen);
            if CLen = -1 then
            begin
              SeaFind(@IBufferTS, 256, @CXGzip[0], 6, @CLen);
              if CLen = -1 then
              begin
                TSetHeader(SetHeader)(pfc, @CLength[0], @CReplace[0]);
                pfc^.pFilterContext := PDWORD(3);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TThStruct.TreatUrlMap(pfc: PHTTP_FILTER_CONTEXT; PvNotification: Pointer);
begin
  pfc^.pFilterContext := PDWORD(0);
  with THTTP_FILTER_URL_MAP(PvNotification^) do
  begin
    CLen := Length(pszURL);
    if CLen > 4 then // #.EXT len 5
    begin
      HelperP := PByte(pszURL) + CLen;
      SeaCompare(HelperP - 4, @CAspx[0], 4, @CLen);
      if CLen = 0 then
        pfc^.pFilterContext := PDWORD(1)
      else
      begin
        SeaCompare(HelperP - 3, @CDll[0], 3, @CLen);
        if CLen = 0 then
          pfc^.pFilterContext := PDWORD(1)
        else
        begin
          SeaCompare(HelperP - 3, @CPhp[0], 3, @CLen);
          if CLen = 0 then
            pfc^.pFilterContext := PDWORD(1)
          else
          begin
            SeaCompare(HelperP - 3, @CAspOld[0], 3, @CLen);
            if CLen = 0 then
              pfc^.pFilterContext := PDWORD(1)
            else
            begin
              SeaCompare(HelperP - 3, @CJava[0], 3, @CLen);
              if CLen = 0 then
                pfc^.pFilterContext := PDWORD(1)
            end;
          end;
        end;
      end;
    end;
  end;
end;

function HttpFilterProc(pfc: PHTTP_FILTER_CONTEXT; NotificationType: DWORD; PvNotification: LPVOID): DWORD; export; stdcall;
begin
  Result := SF_STATUS_REQ_NEXT_NOTIFICATION;
  try
    case NotificationType of
      SF_NOTIFY_URL_MAP:
        begin
          if not ThInit then // eventually use entrypoint DLL_THREAD_ATTACH
            ThWork := TThStruct.Create;
          ThWork.TreatUrlMap(pfc, PvNotification);
        end;
      SF_NOTIFY_SEND_RESPONSE:
        if DWORD(pfc^.pFilterContext) = 1 then
        begin
          if not ThInit then
            ThWork := TThStruct.Create;
          ThWork.TreatSendResponse(pfc, PvNotification);
        end;
      SF_NOTIFY_SEND_RAW_DATA:
        if DWORD(pfc^.pFilterContext) > 1 then
        begin
          if not ThInit then
            ThWork := TThStruct.Create;
          ThWork.TreatSendRawData(pfc, PvNotification);
        end;
      SF_NOTIFY_END_OF_REQUEST:
        if DWORD(pfc^.pFilterContext) > 3 then
        begin
          if not ThInit then
            ThWork := TThStruct.Create;
          ThWork.TreatEndOfRequest(pfc);
        end;
    end;
  except
    on E: exception do
    begin
      Result := SF_STATUS_REQ_NEXT_NOTIFICATION;
//    Log('Exception: ' + E.Message + #13#10 + 'StackTrace:' + #13#10 + E.StackTrace);
    end;
  end;
end;

function GetFilterVersion(pVer: PHTTP_FILTER_VERSION): BOOL; export; stdcall;
begin
  pVer^.lpszFilterDesc := 'Roberto Della Pasqua 1974 - www.dellapasqua.com - Parallel SIMD Deflate 2.02 - 03 Sept 2017';
  pVer^.dwFilterVersion := Makelong(HSE_VERSION_MINOR, HSE_VERSION_MAJOR);
  pVer^.DwFlags := SF_NOTIFY_ORDER_LOW or SF_NOTIFY_URL_MAP or SF_NOTIFY_SEND_RAW_DATA or SF_NOTIFY_SEND_RESPONSE or SF_NOTIFY_END_OF_REQUEST;
  Result := True;
end;

function TerminateFilter(DwFlags: DWORD): BOOL; export; stdcall;
begin
  Result := True;
end;

procedure DLLEntryPoint(Reason: DWORD);
// detach free struct
begin
  if Reason = Dll_Thread_Detach then
  begin
    if ThInit then
    begin
      FreeAndNil(ThWork);
      ThInit := False;
    end;
  end;
end;

exports GetFilterVersion, HttpFilterProc, TerminateFilter;

begin
  IsMultiThread := False; // avoid thread contention in new MM
  DllProc := @DLLEntryPoint;

  // controllare se il content-length deve essere 5
  // controllare se content < 99999 > 999 then exit
  // testare chunk-encoding dovrebbe passare
end.
