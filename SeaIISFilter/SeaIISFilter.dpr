library SeaIISFilter;

// 19 june 2018 Roberto Della Pasqua www.dellapasqua.com
// highly iterative
// low level parallel SIMD
// threadpool wrapper
// zero memory copy
// data structure deduplication
// 5x faster than default IIS 10 gzip at the same ratio (I7 cpu 4/8 core)

{ .$DEFINE DEBUGFILE }
{$O+}
{$WEAKLINKRTTI ON}
{$MINENUMSIZE 4}

uses
  Rdpmm64,
  Rdpsimd64,
  Rdpzlib64,
  Windows,
  Sysutils;

// SDK IIS 7.0 httpfilt.h

const
  Hse_version_major = 7;
  Hse_version_minor = 0;
  Hse_log_buffer_len = 80;
  Hse_max_ext_dll_name_len = 256;
  Http_filter_revision = $70000;
  Sf_max_username = (256 + 1);
  Sf_max_password = (256 + 1);
  Sf_max_auth_type = (32 + 1);
  Sf_max_filter_desc_len = (256 + 1);
  Sf_status_req_finished = $8000000;
  Sf_status_req_finished_keep_conn = $8000001;
  Sf_status_req_next_notification = $8000002;
  Sf_status_req_handled_notification = $8000003;
  Sf_status_req_error = $8000004;
  Sf_status_req_read_next = $8000005;
  Sf_denied_logon = $00000001;
  Sf_denied_resource = $00000002;
  Sf_denied_filter = $00000004;
  Sf_denied_application = $00000008;
  Sf_denied_by_config = $00010000;
  Sf_notify_secure_port = $00000001;
  Sf_notify_nonsecure_port = $00000002;
  Sf_notify_read_raw_data = $00008000;
  Sf_notify_preproc_headers = $00004000;
  Sf_notify_url_map = $00001000;
  Sf_notify_authentication = $00002000;
  Sf_notify_access_denied = $00000800;
  Sf_notify_auth_complete = $04000000;
  Sf_notify_send_response = $00000040;
  Sf_notify_send_raw_data = $00000400;
  Sf_notify_end_of_request = $00000080;
  Sf_notify_log = $00000200;
  Sf_notify_end_of_net_session = $00000100;
  Sf_notify_extension_trigger = $02000000;
  Sf_notify_order_high = $00080000;
  Sf_notify_order_medium = $00040000;
  Sf_notify_order_low = $00020000;
  Sf_notify_order_default = Sf_notify_order_low;
  Sf_notify_order_mask = (Sf_notify_order_high or Sf_notify_order_medium or Sf_notify_order_low);

type
  Sf_req_type = (Sf_req_send_response_header, Sf_req_add_headers_on_denial, Sf_req_set_next_read_size, Sf_req_set_proxy_info,
    Sf_req_get_connid, Sf_req_set_certificate_info, Sf_req_get_property, Sf_req_normalize_url, Sf_req_disable_notifications);

  Sf_property_iis = (Sf_property_ssl_ctxt, Sf_property_instance_num_id);
  Sf_priority_type = (High_priority, Medium_priority, Low_priority, Default_priority);

  Thttp_filter_context = record
    Cbsize: Dword;
    Revision: Dword;
    Servercontext: Pvoid;
    Ulreserved: Dword;
    Fissecureport: Bool;
    Pfiltercontext: Pvoid;
    Getservervariable: Pvoid;
    Addresponseheaders: Pvoid;
    Writeclient: Pvoid;
    Allocmem: Pvoid;
    Serversupportfunc: Pvoid;
  end;

  Http_filter_context = Thttp_filter_context;
  Phttp_filter_context = ^Http_filter_context;

  Tgetservervariable = function(Pfc: Phttp_filter_context; Variablename: Pansichar; Buffer: Lpvoid; Buffsize: Pdword)
    : Bool; stdcall;
  Taddresponseheaders = function(Pfc: Phttp_filter_context; Headers: Pansichar; Reserved: Dword): Bool; stdcall;
  Twriteclient = function(Pfc: Phttp_filter_context; Buffer: Lpvoid; Dwbytes: Lpdword; Reserved: Dword): Bool; stdcall;
  Tallocmem = function(Pfc: Phttp_filter_context; Cbsize: Dword; Dwreserved: Dword): Lpvoid; stdcall;
  Tserversupportfunc = function(Pfc: Phttp_filter_context; Sfreq: Sf_req_type; Pdata: Pvoid; Ul1: Dword; Ul2: Dword)
    : Bool; stdcall;

  Thttp_filter_raw_data = record
    Pvindata: Pvoid;
    Cbindata: Dword;
    Cbinbuffer: Dword;
    Dwreserved: Dword;
  end;

  Http_filter_raw_data = Thttp_filter_raw_data;
  Phttp_filter_raw_data = ^Http_filter_raw_data;

  Tgetheader = function(Pfc: Phttp_filter_context; Lpszname: Pansichar; Lpvbuffer: Lpvoid; Lpdwsize: Lpdword): Bool; stdcall;
  Tsetheader = function(Pfc: Phttp_filter_context; Lpszname: Pansichar; Lpszvalue: Pansichar): Bool; stdcall;
  Taddheader = function(Pfc: Phttp_filter_context; Lpszname: Pansichar; Lpszvalue: Pansichar): Bool; stdcall;
  Tgetusertoken = function(Pfc: Phttp_filter_context; Phtoken: Phandle): Bool; stdcall;

  Thttp_filter_preproc_headers = record
    Getheader: Pvoid;
    Setheader: Pvoid;
    Addheader: Pvoid;
    Dwreserved: Dword;
  end;

  Http_filter_preproc_headers = Thttp_filter_preproc_headers;
  Phttp_filter_preproc_headers = ^Http_filter_preproc_headers;
  Http_filter_send_response = Http_filter_preproc_headers;
  Thttp_filter_send_response = Thttp_filter_preproc_headers;
  Phttp_filter_send_response = ^Http_filter_send_response;

  Thttp_filter_authent = record
    Pszuser: Pansichar;
    Cbuserbuff: Dword;
    Pszpassword: Pansichar;
    Cbpasswordbuff: Dword;
  end;

  Http_filter_authent = Thttp_filter_authent;
  Phttp_filter_authent = ^Http_filter_authent;

  Thttp_filter_url_map = record
    Pszurl: Pansichar;
    Pszphysicalpath: Pansichar;
    Cbpathbuff: Dword;
  end;

  Http_filter_url_map = Thttp_filter_url_map;
  Phttp_filter_url_map = ^Http_filter_url_map;

  Thttp_filter_url_map_ex = record
    Pszurl: Pansichar;
    Pszphysicalpath: Pansichar;
    Cbpathbuff: Dword;
    Dwflags: Dword;
    Cchmatchingpath: Dword;
    Cchmatchingurl: Dword;
    Pszscriptmapentry: Pansichar;
  end;

  Http_filter_url_map_ex = Thttp_filter_url_map_ex;
  Phttp_filter_url_map_ex = ^Http_filter_url_map_ex;

  Thttp_filter_access_denied = record
    Pszurl: Pansichar;
    Pszphysicalpath: Pansichar;
    Dwreason: Dword;
  end;

  Http_filter_access_denied = Thttp_filter_access_denied;
  Phttp_filter_access_denied = ^Http_filter_access_denied;

  Thttp_filter_log = record
    Pszclienthostname: Pansichar;
    Pszclientusername: Pansichar;
    Pszservername: Pansichar;
    Pszoperation: Pansichar;
    Psztarget: Pansichar;
    Pszparameters: Pansichar;
    Dwhttpstatus: Dword;
    Dwwin32status: Dword;
    Dwbytessent: Dword;
    Dwbytesrecvd: Dword;
    Mstimeforprocessing: Dword;
  end;

  Http_filter_log = Thttp_filter_log;
  Phttp_filter_log = ^Http_filter_log;

  Thttp_filter_auth_complete_info = record
    Getheader: Pvoid;
    Setheader: Pvoid;
    Addheader: Pvoid;
    Getusertoken: Tgetusertoken;
    Httpstatus: Dword;
    Fresetauth: Bool;
    Dwreserved: Dword;
  end;

  Http_filter_auth_complete_info = Thttp_filter_auth_complete_info;
  Phttp_filter_auth_complete_info = ^Http_filter_auth_complete_info;

  Thttp_filter_extension_trigger_info = record
    Dwtriggertype: Dword;
    Pvtriggercontext: Pvoid;
  end;

  Http_filter_extension_trigger_info = Thttp_filter_extension_trigger_info;
  Phttp_filter_extension_trigger_info = ^Http_filter_extension_trigger_info;

  Thttp_filter_version = record
    Dwserverfilterversion: Dword;
    Dwfilterversion: Dword;
    Lpszfilterdesc: array [0 .. Sf_max_filter_desc_len - 1] of Ansichar;
    Dwflags: Dword;
  end;

  Http_filter_version = Thttp_filter_version;
  Phttp_filter_version = ^Http_filter_version;

  Tthstruct = class
  strict private
    Cpowered: Tbytes;
    Casp: Tbytes;
    Caspold: Tbytes;
    Cphp: Tbytes;
    Cjava: Tbytes;
    Caspx: Tbytes;
    Cdll: Tbytes;
    Chttp1: Tbytes;
    Chttp2: Tbytes;
    Ccrlf: Tbytes;
    //
    Cctext: Tbytes;
    Cctext2: Tbytes;
    Cctext3: Tbytes;
    Cctext4: Tbytes;
    Cctext5: Tbytes;
    //
    Caccepttext: Tbytes;
    //
    Ccl: Tbytes;
    Ctransfer: Tbytes;
    Ccontent: Tbytes;
    Clength: Tbytes;
    Caccept: Tbytes;
    Cdeflate: Tbytes;
    Cgzip: Tbytes;
    Cxgzip: Tbytes;
    Cendline: Tbytes;
    Creplace: Tbytes;
    Cctdeflate: Tbytes;
    //
    Fthreadid: Dword;
    Fzlevel: Integer;
    //
    Pheaders: Pvoid;
    Pheaderslen: Integer;
    Gpvindata: Pvoid;
    Gcbindata: Cardinal;
    Gbuffer: Pvoid;
    Gbufsize: Cardinal;
    Ibuffer: Pvoid;
    Ibufsize: Integer;
    Ibufferts: Pvoid;
    //
    Clen: Integer;
    Clen2: Integer;
    Clen3: Integer;
    Headers: Pvoid;
    Helpers: Pansichar;
    Helperp: Pbyte;
    //
    Qstarttime: Int64;
    Qendtime: Int64;
    Qfrequency: Int64;
    Qelapsedtime: Int64;
    Qtotalhits: Int64;
    Qtotalbytes: Int64;
    Qtotalbytesc: Int64;
{$IFDEF DEBUGFILE}
    Debugs: Ansistring;
{$ENDIF}
  protected
    constructor Create;
    destructor Destroy; override;
  public
    procedure Treaturlmap(Pfc: Phttp_filter_context; Pvnotification: Pvoid);
    procedure Treatsendresponse(Pfc: Phttp_filter_context; Pvnotification: Pvoid);
    procedure Treatsendrawdata(Pfc: Phttp_filter_context; Pvnotification: Pvoid);
    procedure Treatendofrequest(Pfc: Phttp_filter_context);
  end;

threadvar Thinit: Boolean;
threadvar Thwork: Tthstruct;

constructor Tthstruct.Create;
begin
  Cpowered := Tencoding.Ascii.Getbytes('X-Powered-By:');
  Casp := Tencoding.Ascii.Getbytes('asp.net');
  Caspold := Tencoding.Ascii.Getbytes('asp');
  Cphp := Tencoding.Ascii.Getbytes('php');
  Cjava := Tencoding.Ascii.Getbytes('jsp');
  Caspx := Tencoding.Ascii.Getbytes('aspx');
  Cdll := Tencoding.Ascii.Getbytes('dll');
  Chttp1 := Tencoding.Ascii.Getbytes('HTTP/1.1 200 OK');
  Chttp2 := Tencoding.Ascii.Getbytes('HTTP/2');
  Ccrlf := Tencoding.Ascii.Getbytes(#13#10#13#10);
  //
  Cctext := Tencoding.Ascii.Getbytes('Content-Type: text/');
  Cctext2 := Tencoding.Ascii.Getbytes('Content-Type: message/');
  Cctext3 := Tencoding.Ascii.Getbytes('Content-Type: application/javascript');
  Cctext4 := Tencoding.Ascii.Getbytes('Content-Type: application/x-javascript');
  Cctext5 := Tencoding.Ascii.Getbytes('Content-Type: application/xml');
  //
  Caccepttext := Tencoding.Ascii.Getbytes('Accept: text/html');
  //
  Ccl := Tencoding.Ascii.Getbytes('<%CL%>');
  Ctransfer := Tencoding.Ascii.Getbytes('Transfer-Encoding:');
  Ccontent := Tencoding.Ascii.Getbytes('Content-Encoding:');
  Clength := Tencoding.Ascii.Getbytes('Content-Length:');
  Caccept := Tencoding.Ascii.Getbytes('HTTP_ACCEPT_ENCODING');
  Cdeflate := Tencoding.Ascii.Getbytes('deflate');
  Cgzip := Tencoding.Ascii.Getbytes('gzip');
  Cxgzip := Tencoding.Ascii.Getbytes('x-gzip');
  Cendline := Tencoding.Ascii.Getbytes(#13#10);
  Creplace := Tencoding.Ascii.Getbytes('%PAR%');
  Cctdeflate := Tencoding.Ascii.Getbytes('Content-Encoding: deflate');
  //
  Fthreadid := Getcurrentthreadid;
  Fzlevel := 1;
  Getmem(Ibufferts, 256);
  //
  Queryperformancefrequency(Qfrequency);
  Qelapsedtime := 0;
  Qtotalhits := 0;
  Qtotalbytes := 0;
  Qtotalbytesc := 0;
  //
  Thinit := True;
  inherited;
end;

destructor Tthstruct.Destroy;
begin
  Freemem(Ibufferts);
  inherited;
end;

procedure Tthstruct.Treatendofrequest(Pfc: Phttp_filter_context);
begin
  if Dword(Pfc^.Pfiltercontext) = 4 then // zerocopy buffering
  begin
    try
      Queryperformancecounter(Qstarttime);
      Ibuffer := nil;
      Seazlib.Compress(Gpvindata, Gcbindata, Ibuffer, Ibufsize);
      Queryperformancecounter(Qendtime);
      Inc(Qelapsedtime, Qendtime - Qstarttime);
      Inc(Qtotalhits);
      Inc(Qtotalbytes, Gcbindata);
      Inc(Qtotalbytesc, Ibufsize);
      // InterlockedIncrement64
      Seafind(Pbyte(Pheaders), Pheaderslen, @Creplace[0], 5, @Clen);
      Helpers := Inttostrf(Ibufsize);
      Clen2 := Length(Helpers);
      Clen3 := Pheaderslen + 27 - (5 - Clen2);
      Getmem(Headers, Clen3);
      Helperp := Pbyte(Headers);
      Seamove(Pbyte(Pheaders), Helperp, Clen + 17);
      Inc(Helperp, Clen);
      Seamove(Pbyte(Helpers), Helperp, Clen2);
      Inc(Helperp, Clen2);
      Helperp^ := 13;
      Inc(Helperp);
      Helperp^ := 10;
      Inc(Helperp);
      Seamove(@Cctdeflate[0], Helperp, 25);
      Inc(Helperp, 25);
      Clen2 := Pheaderslen - (Clen + 5);
      Seamove(Pbyte(Pheaders) + Clen + 5, Helperp, Clen2);
{$IFDEF DEBUGFILE}
      Setstring(Debugs, Pansichar(Pbyte(Headers)), Clen3);
      Log('Level5 headers:' + #13#10'START>>' + Debugs + '<<END');
      Setstring(Debugs, Pansichar(Pbyte(Ibuffer)), Ibufsize);
      Log('Level5 body:' + #13#10'START>>' + Debugs + '<<END');
{$ENDIF}
      Twriteclient(Pfc^.Writeclient)(Pfc, Headers, @Clen3, 0);
      Twriteclient(Pfc^.Writeclient)(Pfc, Ibuffer, @Ibufsize, 0);
    finally
      Freemem(Headers);
      Freemem(Ibuffer);
    end;
  end
  else if Dword(Pfc^.Pfiltercontext) > 4 then // body one-shot buffering
  begin
    try
      Queryperformancecounter(Qstarttime);
      Ibuffer := nil;
      Seazlib.Compress(Gbuffer, Gbufsize, Ibuffer, Ibufsize);
      Queryperformancecounter(Qendtime);
      Inc(Qelapsedtime, Qendtime - Qstarttime);
      Inc(Qtotalhits);
      Inc(Qtotalbytes, Gbufsize);
      Inc(Qtotalbytesc, Ibufsize);
      Seafind(Pbyte(Pheaders), Pheaderslen, @Creplace[0], 5, @Clen);
      Helpers := Inttostrf(Ibufsize);
      Clen2 := Length(Helpers);
      Clen3 := Pheaderslen + 27 - (5 - Clen2);
      Getmem(Headers, Clen3);
      Helperp := Pbyte(Headers);
      Seamove(Pbyte(Pheaders), Helperp, Clen + 17);
      Inc(Helperp, Clen);
      Seamove(Pbyte(Helpers), Helperp, Clen2);
      Inc(Helperp, Clen2);
      Helperp^ := 13;
      Inc(Helperp);
      Helperp^ := 10;
      Inc(Helperp);
      Seamove(@Cctdeflate[0], Helperp, 25);
      Inc(Helperp, 25);
      Clen2 := Pheaderslen - (Clen + 5);
      Seamove(Pbyte(Pheaders) + Clen + 5, Helperp, Clen2);
{$IFDEF DEBUGFILE}
      Setstring(Debugs, Pansichar(Pbyte(Headers)), Clen3);
      Log('Level5+ headers:' + #13#10 + Debugs);
      Setstring(Debugs, Pansichar(Pbyte(Ibuffer)), Ibufsize);
      Log('Level5+ body:' + #13#10 + Debugs);
{$ENDIF}
      Twriteclient(Pfc^.Writeclient)(Pfc, Headers, @Clen3, 0);
      Twriteclient(Pfc^.Writeclient)(Pfc, Ibuffer, @Ibufsize, 0);
    finally
      Freemem(Headers);
      Freemem(Ibuffer);
      Freemem(Gbuffer);
    end;
  end;
end;

procedure Tthstruct.Treatsendrawdata(Pfc: Phttp_filter_context; Pvnotification: Pointer);
begin
  with Thttp_filter_raw_data(Pvnotification^) do
  begin
    Seacompare(Pvindata, @Chttp1[0], 14, @Clen); // todo: http2 compatibility
    if (Clen = 0) then
    begin
{$IFDEF DEBUGFILE}
      Setstring(Debugs, Pansichar(Pvindata), Cbindata);
      Log('Level1:' + #13#10 + 'CbInData:' + Inttostr(Cbindata) + #13#10 + 'cbInBuffer:' + Inttostr(Cbinbuffer) + #13#10 + '-' +
        #13#10 + Debugs);
{$ENDIF}
      if (Cbindata < 100) or (Cbindata > 99999) then
        Exit; // testare
      Seafind(Pvindata, Cbindata, @Ccrlf[0], 4, @Clen);
      if Clen > -1 then
      begin
        Seafind(Pvindata, Clen, @Cctext[0], 19, @Clen);
        if Clen > -1 then
        begin
          Pheaders := Pvindata;
          Pheaderslen := Cbindata;
          Cbindata := 0;
          Cbinbuffer := 0;
          Pfc^.Pfiltercontext := Pdword(3);
        end
        else
        begin
          Seafind(Pvindata, Clen, @Cctext2[0], 22, @Clen);
          if Clen > -1 then
          begin
            Pheaders := Pvindata;
            Pheaderslen := Cbindata;
            Cbindata := 0;
            Cbinbuffer := 0;
            Pfc^.Pfiltercontext := Pdword(3);
          end
          else
          begin
            Seafind(Pvindata, Clen, @Cctext3[0], 36, @Clen);
            if Clen > -1 then
            begin
              Pheaders := Pvindata;
              Pheaderslen := Cbindata;
              Cbindata := 0;
              Cbinbuffer := 0;
              Pfc^.Pfiltercontext := Pdword(3);
            end
            else
            begin
              Seafind(Pvindata, Clen, @Cctext4[0], 38, @Clen);
              if Clen > -1 then
              begin
                Pheaders := Pvindata;
                Pheaderslen := Cbindata;
                Cbindata := 0;
                Cbinbuffer := 0;
                Pfc^.Pfiltercontext := Pdword(3);
              end
              else
              begin
                Seafind(Pvindata, Clen, @Cctext5[0], 29, @Clen);
                if Clen > -1 then
                begin
                  Pheaders := Pvindata;
                  Pheaderslen := Cbindata;
                  Cbindata := 0;
                  Cbinbuffer := 0;
                  Pfc^.Pfiltercontext := Pdword(3);
                end;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      if Dword(Pfc.Pfiltercontext) = 3 then
      begin
{$IFDEF DEBUGFILE}
        Setstring(Debugs, Pansichar(Pvindata), Cbindata);
        Log('Level2:' + #13#10 + 'CbInData:' + Inttostr(Cbindata) + #13#10 + 'cbInBuffer:' + Inttostr(Cbinbuffer) + #13#10 + '-' +
          #13#10 + Debugs);
{$ENDIF}
        Gpvindata := Pvindata;
        Gcbindata := Cbindata;
        Cbindata := 0;
        Cbinbuffer := 0;
        Pfc^.Pfiltercontext := Pdword(4); // go stage index 4
      end
      else
      begin
        if Dword(Pfc^.Pfiltercontext) = 4 then
        begin
          Gbufsize := Gcbindata + Cbindata;
          Getmem(Gbuffer, Gbufsize);
          Seamove(Gpvindata, Gbuffer, Gcbindata);
          Seamove(Pvindata, Pbyte(Gbuffer) + Gcbindata, Cbindata);
{$IFDEF DEBUGFILE}
          Setstring(Debugs, Pansichar(Pvindata), Cbindata);
          Log('Level3:' + #13#10 + 'CbInData:' + Inttostr(Cbindata) + #13#10 + 'cbInBuffer:' + Inttostr(Cbinbuffer) + #13#10 +
            'GcbInData:' + Inttostr(Gcbindata) + #13#10 + 'GBufSize:' + #13#10 + '-' + #13#10 + Debugs);
{$ENDIF}
          Cbindata := 0;
          Cbinbuffer := 0;
          Pfc^.Pfiltercontext := Pdword(5); // go stage index 5
        end
        else
        begin // 5+
          Reallocmem(Gbuffer, Gbufsize + Cbindata);
          Seamove(Pvindata, Pbyte(Gbuffer) + Gbufsize, Cbindata);
          Inc(Gbufsize, Cbindata);
{$IFDEF DEBUGFILE}
          Setstring(Debugs, Pansichar(Pvindata), Cbindata);
          Log('Level4+:' + #13#10 + 'CbInData:' + Inttostr(Cbindata) + #13#10 + 'cbInBuffer:' + Inttostr(Cbinbuffer) + #13#10 +
            'GcbInData:' + Inttostr(Gcbindata) + #13#10 + 'GBufSize:' + #13#10 + '-' + #13#10 + Debugs);
{$ENDIF}
          Cbindata := 0;
          Cbinbuffer := 0;
        end;
      end;
    end;
  end;
end;

procedure Tthstruct.Treatsendresponse(Pfc: Phttp_filter_context; Pvnotification: Pointer);
begin
  with Thttp_filter_send_response(Pvnotification^) do
  begin
    Seazero(@Ibufferts, 256);
    Clen := 256;
    if not Tgetheader(Getheader)(Pfc, @Ctransfer[0], @Ibufferts, @Clen) then
    begin
      Seazero(@Ibufferts, 256);
      Clen := 256;
      if Tgetservervariable(Pfc.Getservervariable)(Pfc, @Caccept[0], @Ibufferts, @Clen) then
      begin
        Seafind(@Ibufferts, 256, @Cdeflate[0], 7, @Clen);
        if Clen > 0 then
        begin
          Seazero(@Ibufferts, 256);
          Clen := 256;
          Tgetheader(Getheader)(Pfc, @Ccontent[0], @Ibufferts, @Clen);
          Seafind(@Ibufferts, 256, @Cdeflate[0], 7, @Clen);
          if Clen = -1 then
          begin
            Seafind(@Ibufferts, 256, @Cgzip[0], 4, @Clen);
            if Clen = -1 then
            begin
              Seafind(@Ibufferts, 256, @Cxgzip[0], 6, @Clen);
              if Clen = -1 then
              begin
                Tsetheader(Setheader)(Pfc, @Clength[0], @Creplace[0]);
                Pfc^.Pfiltercontext := Pdword(3);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure Tthstruct.Treaturlmap(Pfc: Phttp_filter_context; Pvnotification: Pointer);
begin
  Pfc^.Pfiltercontext := Pdword(0);
  with Thttp_filter_url_map(Pvnotification^) do
  begin
    Clen := Length(Pszurl);
    if Clen > 4 then // #.EXT len 5
    begin
      Helperp := Pbyte(Pszurl) + Clen;
      Seacompare(Helperp - 4, @Caspx[0], 4, @Clen);
      if Clen = 0 then
        Pfc^.Pfiltercontext := Pdword(1)
      else
      begin
        Seacompare(Helperp - 3, @Cdll[0], 3, @Clen);
        if Clen = 0 then
          Pfc^.Pfiltercontext := Pdword(1)
        else
        begin
          Seacompare(Helperp - 3, @Cphp[0], 3, @Clen);
          if Clen = 0 then
            Pfc^.Pfiltercontext := Pdword(1)
          else
          begin
            Seacompare(Helperp - 3, @Caspold[0], 3, @Clen);
            if Clen = 0 then
              Pfc^.Pfiltercontext := Pdword(1)
            else
            begin
              Seacompare(Helperp - 3, @Cjava[0], 3, @Clen);
              if Clen = 0 then
                Pfc^.Pfiltercontext := Pdword(1)
            end;
          end;
        end;
      end;
    end;
  end;
end;

function Httpfilterproc(Pfc: Phttp_filter_context; Notificationtype: Dword; Pvnotification: Lpvoid): Dword; export; stdcall;
begin
  Result := Sf_status_req_next_notification;
  try
    case Notificationtype of
      Sf_notify_url_map:
        begin
          if not Thinit then // eventually use entrypoint DLL_THREAD_ATTACH
            Thwork := Tthstruct.Create;
          Thwork.Treaturlmap(Pfc, Pvnotification);
        end;
      Sf_notify_send_response:
        if Dword(Pfc^.Pfiltercontext) = 1 then
        begin
          if not Thinit then
            Thwork := Tthstruct.Create;
          Thwork.Treatsendresponse(Pfc, Pvnotification);
        end;
      Sf_notify_send_raw_data:
        if Dword(Pfc^.Pfiltercontext) > 1 then
        begin
          if not Thinit then
            Thwork := Tthstruct.Create;
          Thwork.Treatsendrawdata(Pfc, Pvnotification);
        end;
      Sf_notify_end_of_request:
        if Dword(Pfc^.Pfiltercontext) > 3 then
        begin
          if not Thinit then
            Thwork := Tthstruct.Create;
          Thwork.Treatendofrequest(Pfc);
        end;
    end;
  except
    on E: Exception do
    begin
      Result := Sf_status_req_next_notification;
      // Log('Exception: ' + E.Message + #13#10 + 'StackTrace:' + #13#10 + E.StackTrace);
    end;
  end;
end;

function Getfilterversion(Pver: Phttp_filter_version): Bool; export; stdcall;
begin
  Pver^.Lpszfilterdesc := 'Roberto Della Pasqua 1974 - www.dellapasqua.com - Parallel SIMD Deflate 2.02 - 03 Sept 2017';
  Pver^.Dwfilterversion := Makelong(Hse_version_minor, Hse_version_major);
  Pver^.Dwflags := Sf_notify_order_low or Sf_notify_url_map or Sf_notify_send_raw_data or Sf_notify_send_response or
    Sf_notify_end_of_request;
  Result := True;
end;

function Terminatefilter(Dwflags: Dword): Bool; export; stdcall;
begin
  Result := True;
end;

procedure Dllentrypoint(Reason: Dword);
// detach free struct
begin
  if Reason = Dll_thread_detach then
  begin
    if Thinit then
    begin
      Freeandnil(Thwork);
      Thinit := False;
    end;
  end;
end;

exports Getfilterversion, Httpfilterproc, Terminatefilter;

begin
  Ismultithread := False; // avoid thread contention in new MM
  Dllproc := @Dllentrypoint;

  // controllare se il content-length deve essere 5
  // controllare se content < 99999 > 999 then exit
  // testare chunk-encoding dovrebbe passare
end.
